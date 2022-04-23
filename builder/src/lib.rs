#![feature(let_else)]
#![feature(let_chains)]

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    expand::derive_builder(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

mod expand {
    use proc_macro2::{Ident, TokenStream};
    use quote::{format_ident, quote};
    use syn::{
        parse_quote_spanned, punctuated::Punctuated, spanned::Spanned,
        AngleBracketedGenericArguments, Data, DeriveInput, Error, Field, Fields, GenericArgument,
        Lit, Meta, MetaNameValue, NestedMeta, Path, PathArguments, PathSegment, Result, Token,
        Type, TypePath,
    };

    #[derive(Clone, Debug)]
    struct FieldInfo {
        orig_field: Field,
        inner_ty: Type,
        repeated_name: Option<Ident>,
        is_optional: bool,
    }

    pub fn derive_builder(input: DeriveInput) -> Result<TokenStream> {
        let input_span = input.span();
        let name = input.ident;

        let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

        let ty_generics_turbofish = ty_generics.as_turbofish();

        let builder_name = format_ident!("{}Builder", name);

        let Data::Struct(st) = input.data else {
            return Err(Error::new(
                input_span,
                "only structs are supported",
            ));
        };

        let Fields::Named(fields) = st.fields else {
            return Err(Error::new(st.fields.span(), "Only named fields are supported"));
        };

        let fields = fields
            .named
            .into_iter()
            .map(|f| {
                let mut inner_ty = f.clone().ty;
                let mut is_optional = false;
                let mut repeated_name = None;
                if let Type::Path(TypePath { qself: None, path: Path {
                    segments,
                    ..
                }}) = &inner_ty
                    && segments.len()  == 1
                    && let Some(PathSegment {
                            ident: orig_ty_ident,
                            arguments,
                        }) = segments.first()
                    && orig_ty_ident == "Option"
                    && let PathArguments::AngleBracketed(
                        AngleBracketedGenericArguments {
                            args,..
                        }
                    ) = arguments
                    && let Some(GenericArgument::Type(arg_ty)) = args.first()
                {
                    is_optional = true;
                    inner_ty = arg_ty.clone();
                } else if let Some(attr) = f.attrs.first()
                    && let meta = attr.parse_meta()?
                    && let Meta::List(meta) = meta
                    && let Some(NestedMeta::Meta(meta)) = meta.nested.first()
                    && let Meta::NameValue(MetaNameValue {
                        path,
                        lit: Lit::Str(item_name),
                        ..
                    }) = meta
                {
                    if !path.is_ident("each") {
                        return Err(Error::new(path.span(), "expected `builder(each = \"...\")`"))
                    }
                    repeated_name = Some(Ident::new(&item_name.value(), item_name.span()));
                    if let Type::Path(TypePath { qself: None, path: Path {
                        segments,
                        ..
                    }}) = &inner_ty
                        && segments.len()  == 1
                        && let Some(PathSegment {
                                ident: orig_ty_ident,
                                arguments,
                            }) = segments.first()
                        && orig_ty_ident == "Vec"
                        && let PathArguments::AngleBracketed(
                            AngleBracketedGenericArguments {
                                args,..
                            }
                        ) = arguments
                        && let Some(GenericArgument::Type(arg_ty)) = args.first()
                    {
                        inner_ty = arg_ty.clone();
                    } else {
                        return Err(Error::new(inner_ty.span(), "Vec is expected for #[builder]"));
                    }
                }

                Ok(FieldInfo {
                    orig_field: f,
                    inner_ty,
                    is_optional,
                    repeated_name,
                })
            })
            .collect::<Result<Vec<_>>>()?;

        let builder_fields = fields
            .iter()
            .cloned()
            .map(|field| {
                let inner_ty = field.inner_ty;
                match field.repeated_name {
                    Some(_) => Field {
                        ty: Type::Path(TypePath {
                            qself: None,
                            path: parse_quote_spanned!(inner_ty.span() => std::vec::Vec<#inner_ty>),
                        }),
                        attrs: vec![],
                        ..field.orig_field
                    },
                    None => Field {
                        ty: Type::Path(TypePath {
                            qself: None,
                            path: parse_quote_spanned!(inner_ty.span() => std::option::Option<#inner_ty>),
                        }),
                        ..field.orig_field
                    },
                }
            })
            .collect::<Punctuated<Field, Token![,]>>();

        let non_repeated_setters = fields
            .iter()
            .filter(|f| f.repeated_name.is_none())
            .map(|f| {
                let name = &f.orig_field.ident;
                let inner_ty = &f.inner_ty;
                quote! {
                    pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                        self.#name = Some(#name);
                        self
                    }
                }
            });

        let repeated_setters = fields
            .iter()
            .filter_map(|f| {
                f.repeated_name
                    .as_ref()
                    .map(|name| (name, &f.orig_field.ident, &f.inner_ty))
            })
            .map(|(name, ident, inner_ty)| {
                quote! {
                    pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                        self.#ident.push(#name);
                        self
                    }
                }
            });

        let impl_default_for_builder = {
            let names = fields
                .iter()
                .map(|f| &f.orig_field.ident)
                .collect::<Vec<_>>();
            quote! {
                impl #impl_generics Default for #builder_name #ty_generics #where_clause {
                    fn default() -> Self {
                        Self {
                            #(#names: Default::default(),)*
                        }
                    }
                }
            }
        };

        let build = {
            let (required_fields, optional_fields): (Vec<_>, Vec<_>) = fields
                .iter()
                .map(|f| {
                    (
                        f.is_optional || f.repeated_name.is_some(),
                        &f.orig_field.ident,
                    )
                })
                .partition(|f| !f.0);
            let required_fields = required_fields.into_iter().map(|f| f.1).collect::<Vec<_>>();
            let optional_fields = optional_fields.into_iter().map(|f| f.1).collect::<Vec<_>>();

            quote! {
                pub fn build(&mut self) -> std::option::Option<#name #ty_generics> {
                    let v = std::mem::take(self);
                    if let (
                        #(std::option::Option::Some(#required_fields),)*
                        #(#optional_fields,)*
                    ) = (#(v.#required_fields,)* #(v.#optional_fields,)*) {
                        std::option::Option::Some(#name #ty_generics_turbofish {
                            #(#required_fields,)*
                            #(#optional_fields,)*
                        })
                    } else {
                        None
                    }
                }
            }
        };

        Ok(quote! {
            pub struct #builder_name #ty_generics #where_clause {
                #builder_fields
            }

            #impl_default_for_builder

            impl #impl_generics #builder_name #ty_generics #where_clause {
                #(#non_repeated_setters)*

                #(#repeated_setters)*

                #build
            }

            impl #impl_generics #name #ty_generics #where_clause {
                pub fn builder() -> #builder_name #ty_generics {
                    #builder_name #ty_generics_turbofish ::default()
                }
            }
        })
    }
}
