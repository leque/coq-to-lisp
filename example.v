Require Import Arith.
Require Import Coq.Lists.Streams.

CoFixpoint repeat {A: Type} (x: A) := Cons x (repeat x).
CoFixpoint iterate {A: Type} (f : A -> A) (x : A) := Cons x (iterate f (f x)).

Extraction Language Scheme.

Extraction "example" plus hd tl repeat iterate.
