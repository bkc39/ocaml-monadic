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
 * @date  : 07/2013
 * @author: bkc
 *)


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
      let unit x = M.unit (Some x)
      let bind m f = M.bind m (fun x ->
        match x with
        | Some v -> f v
        | None   -> M.unit None)
    end
    let lift m = M.bind m (fun x -> M.unit (Some x))
  end
vv
(* The Exception monad transformer is a higher-order functor taking an
 * exception type and returning a usual monad transformer.
 *)
module ExceptionT (Ex : sig type t end) : MONAD_TRANSFORMER =
  functor (M : MONAD) -> struct
    module N = struct
      type 'a s = V of 'a | E of Ex.t
      type 'a t = 'a s M.t
      let unit x = M.unit (V x)
      let bind (m : 'a t) (f : 'a -> 'b t) = M.bind m (fun x ->
        match x with
        | V v -> f v
        | E e -> M.unit (E e))
    end
    let lift m = M.bind m (fun x -> N.unit x)
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
      let unit x = fun env -> M.unit x
      let bind m f = fun env -> M.bind (m env) (fun x -> f x env)
    end
    let lift m = fun env -> m
  end

module WriterT (Mon : MONOID) : MONAD_TRANSFORMER =
  functor (M : MONAD) -> struct
    module N = struct
      type 'a t = (Mon.t * 'a) M.t
      let unit x = M.unit (Mon.id,x)
      let bind m f = M.bind m (fun (x,a) ->
        M.bind (f a) (fun (y,b) -> M.unit (Mon.op (x,y),b)))
    end
    let lift m = M.bind m (fun x -> M.unit (Mon.id,x))
  end

module StateT (State : sig type t end) : MONAD_TRANSFORMER =
  functor (M : MONAD) -> struct
    module N = struct
      type 'a t = State.t -> ('a * State.t) M.t
      let unit x = fun s -> M.unit (x,s)
      let bind m f = fun s -> M.bind (m s) (fun (x,s') -> f x s')
    end
    let lift m = fun s -> M.bind m (fun x -> M.unit (x,s))
  end

module ContT (D : sig type t end) : MONAD_TRANSFORMER =
  functor (M : MONAD) -> struct
    module N = struct
      type 'a t = ('a -> D.t M.t) -> D.t M.t
      let unit x = fun k -> k x
      let bind m f = fun k -> m (fun x -> f x k)
    end
    let lift = M.bind
  end
