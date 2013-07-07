(* A description of the monad signature, as well as concrete implementations
 * of commonly used monads. The monad primitives below can be combined via
 * the monad transformers provided in monad_transformers.ml
 *
 *
 * TODO: Add infix operations to the monad module
 *
 * @date  : 06/2013
 * @author: bkc
 *)

module type MONAD = sig
  type 'a t
  val unit : 'a   -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module MonadMJ =
  functor (M : MONAD) -> struct
    type 'a t                                  = 'a M.t
    let unit x                          : 'a t = M.unit x
    let bind (m : 'a t) (f : 'a-> 'b t) : 'b t = M.bind m f
    let map  (f : 'a -> 'b) (m : 'a t)  : 'b t = M.bind m (fun x -> M.unit (f x))
    let join (m : 'a t t)               : 'a t = M.bind m (fun x -> x)
  end

(********************************* MONADS *************************************)

module Identity : MONAD = struct
  type 'a t    = 'a
  let unit x   = x
  let bind m f = f m
end

module CPS (D : sig type t end) : MONAD = struct
  type 'a t    = ('a -> D.t) -> D.t
  let unit x   = fun k -> k x
  let bind m f = fun k -> m (fun x -> f x k)
end

module Option : MONAD = struct
  type 'a t    = 'a option
  let unit x   = Some x
  let bind m f = match m with
    | Some x -> f x
    | None   -> None
end

module Lazy : MONAD = struct
  type 'a t    = unit -> 'a
  let unit x   = fun () -> x
  let bind m f = f (m ())
end

module List : MONAD = struct
  type 'a t    = 'a list
  let unit x   = [x]
  let bind m f = List.flatten (List.map f m)
end

module State (S : sig type t end) : MONAD = struct
  type 'a t    = State.t -> 'a * State.t
  let unit x   = fun s -> (x,s)
  let bind m f = fun s -> let (x,s') = m s in f x s'
end

module Reader (Env : sig type t end) : MONAD = struct
  type 'a t    = Env.t -> 'a
  let unit x   = fun e -> x
  let bind m f = fun e -> f (m e)
end

module Writer (Mon : MONOID) : MONAD = struct
  type 'a t        = 'a * Mon.t
  let unit x       = (x, Mon.id)
  let bind (x,w) f = let (x',w') = f x in (x', M.op (w,w'))
end

(**************************** Adding Map and Join *****************************)

module IdentityMJ                    = MonadMJ (Identity)
module CPSMJ (D : sig type t end)    = MonadMJ (CPS (D))
module OptionMJ                      = MonadMJ (Option)
module LazyMJ                        = MonadMJ (Lazy)
module ListMJ                        = MonadMJ (List)
module StateMJ (S : sig type t end)  = MonadMJ (State (S))
module Reader (Env : sig type t end) = MonadMJ (Reader (Env))
module Writer (Mon : MONOID)         = MonadMJ (Writer (Mon))
