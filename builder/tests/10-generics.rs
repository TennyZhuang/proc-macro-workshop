use derive_builder::Builder;

#[derive(Builder)]
pub struct Point<S> {
    p1: S,
    p2: S,
    p3: S,
}

fn main() {}
