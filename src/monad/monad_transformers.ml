(* Here are the implementations of commonly used monad transformers.
 * These may be used from the concrete monads provided in monad.ml
 * as well as any other module that complies with the signature. To
 * recover the map and join operations simply precompose with the
 * MonadMJ functor.
 *
 * Example:
 *
 * module N (M : MONAD) = MonadMJ (OptionT (M))
 *
 * @date  : 01/2014
 * @author: bkc
 *)

open Monad


module type MONAD_TRANSFORMER =
  functor (M : MONAD) -> sig
    module N : MONAD
    val lift : 'a M.t -> 'a N.t
  end

module type MONOID = sig
  type t
  val id : t
  val op : t*t -> t
end

(**************************** MONAD TRANSFORMERS ******************************)

module OptionT : MONAD_TRANSFORMER =
  functor (M : MONAD) -> struct
    module N = struct
      type 'a t = 'a option M.t
      let return x = M.return (Some x)
      let bind m f = M.bind m (fun x ->
        match x with
        | Some v -> f v
        | None   -> M.return None)
    end
    let lift m = M.bind m (fun x -> M.return (Some x))
  end

(* The Exception monad transformer is a higher-order functor taking an
 * exception type and returning a usual monad transformer.
 *)
module ExceptionT (Ex : sig type t end) : MONAD_TRANSFORMER =
  functor (M : MONAD) -> struct
    module N = struct
      type 'a s = V of 'a | E of Ex.t
      type 'a t = 'a s M.t
      let return x = M.return (V x)
      let bind (m : 'a t) (f : 'a -> 'b t) = M.bind m (fun x ->
        match x with
        | V v -> f v
        | E e -> M.return (E e))
    end
    let lift m = M.bind m (fun x -> N.return x)
  end

(* The Reader monad transformer is similar to the exception transformer.
 * Implemented as a functor of an environment type.
 *
 * The Writer transformer takes some Monoid type as the input to the functor and
 * returns an approprate MONAD_TRANSFORMER.
 *)
module ReaderT (Env : sig type t end) : MONAD_TRANSFORMER =
  functor (M : MONAD) -> struct
    module N = struct
      type 'a t = Env.t -> 'a M.t
      let return x = fun env -> M.return x
      let bind m f = fun env -> M.bind (m env) (fun x -> f x env)
    end
    let lift m = fun env -> m
  end

module WriterT (Mon : MONOID) : MONAD_TRANSFORMER =
  functor (M : MONAD) -> struct
    module N = struct
      type 'a t = (Mon.t * 'a) M.t
      let return x = M.return (Mon.id,x)
      let bind m f = M.bind m (fun (x,a) ->
        M.bind (f a) (fun (y,b) -> M.return (Mon.op (x,y),b)))
    end
    let lift m = M.bind m (fun x -> M.return (Mon.id,x))
  end

module StateT (State : sig type t end) : MONAD_TRANSFORMER =
  functor (M : MONAD) -> struct
    module N = struct
      type 'a t = State.t -> ('a * State.t) M.t
      let return x = fun s -> M.return (x,s)
      let bind m f = fun s -> M.bind (m s) (fun (x,s') -> f x s')
    end
    let lift m = fun s -> M.bind m (fun x -> M.return (x,s))
  end

module ContT (D : sig type t end) : MONAD_TRANSFORMER =
  functor (M : MONAD) -> struct
    module N = struct
      type 'a t = ('a -> D.t M.t) -> D.t M.t
      let return x = fun k -> k x
      let bind m f = fun k -> m (fun x -> f x k)
    end
    let lift = M.bind
  end
