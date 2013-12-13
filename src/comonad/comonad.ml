module type COMONAD = sig
  type 'a t
  val extract : 'a t -> 'a
  val extend  : ('a t -> 'b) -> 'a t -> 'b t
end
