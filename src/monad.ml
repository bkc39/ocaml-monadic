(* A description of the monad signature, as well as concrete implementations
 * of commonly used monads. The monad primitives below can be combined via
 * the monad transformers provided in monad_transformers.ml
 *
 *
 * TODO:
 *   - Lift all of the instances of MonadPlus to monadPlus instances
 *   - Wrap each monad and its corresponding OPS in a module adding
 *     specific functionality.
 *   - For the above consider refactoring each monad instance into its own file
 *
 * @date  : 12/2013
 * @author: bkc
 *)

module type MONAD = sig
  type 'a t
  val return  : 'a     -> 'a t
  val ( >>= ) : 'a t   -> ('a -> 'b t) -> 'b t
end

module MONAD_OPS = functor (M : MONAD) -> struct
  open M

  exception MonadException of string

  let fail (s : string) = return (raise (MonadException s))

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t = m >>= f
  let map  (f : 'a -> 'b) (m : 'a t)   : 'b t = m >>= (fun x -> return (f x))
  let join (m : 'a t t)                : 'a t = m >>= (fun x -> x)

  let for_m (m : 'a t) (f : 'a -> 'b) : 'b t = m >>= (fun x -> return (f x))
  let for_mignore m f = for_m m f >>= (fun _ -> return ())

  let rec sequence = function
    | [] -> return []
    | m::ms -> sequence ms >>= (fun xs -> m >>= (fun x -> return (x::xs)))

  let sequence_ignore ms = sequence ms >>= (fun _ -> return ())

  (* Infix Operators *)
  let ( >> )  (m : 'a t) (n : 'b t) : 'b t = m >>= (fun x -> n)
  let ( <<= ) (f : 'a -> 'b t) (m : 'a t) : 'b t = m >>= f
  let ( >=> ) (f : 'a -> 'b t) (g : 'b -> 'c t) (x : 'a) : 'c t = (f x) >>= g
  let ( <=< ) (g : 'b -> 'c t) (f : 'a -> 'b t) (x : 'a) : 'c t = (f x) >>= g

  let rec forever (m : 'a t) : 'b t = m >>= (fun _ -> forever m)
  let void (m : 'a t) = m >>= (fun _ -> return ())

  (* Generalization of list functions *)
  let rec filter_m (f : 'a -> bool t) = function
    | []    -> return []
    | x::xs -> begin
      (f >=> (fun b ->
        if b then (filter_m f xs) >>= (fun ys -> return (x::ys))
        else filter_m f xs)) x
    end

  let rec map_and_unzip_m (f : 'a -> ('b * 'c) t) = function
    | []    -> return ([],[])
    | x::xs -> begin
      (map_and_unzip_m f xs) >>=
        (fun (bs,cs) -> f x >>= (fun (b,c) -> return (b::bs, c::cs)))
    end

  let rec zip_with_m (f : 'a -> 'b -> 'c t) xs ys = match xs,ys with
    | [], []       -> return []
    | x::xs, y::ys -> begin
      (zip_with_m f xs ys) >>=
        (fun cs -> (f x y) >>= (fun c -> return (c::cs)))
    end
    | _            -> fail "zip_with_m requires arguments of equal length."

  let zip_with_mignore f xs ys = (zip_with_m f xs ys) >>= (fun _ -> return ())

  let rec fold_m (f : 'a -> 'b -> 'a t) (a : 'a) = function
    | [] -> return a
    | x::xs -> (fold_m f a xs) >>= (fun v -> f v x)

  let fold_mignore f a xs = (fold_m f a xs) >>= (fun _ -> return ())

  let rec replicate_m (n : int) (m : 'a t) =
    if n=0 then return []
    else (replicate_m (n-1) m) >>= (fun xs -> m >>= (fun x -> return (x::xs)))

  let replicate_mignore n m = (replicate_m n m) >>= (fun _ -> return ())

  (* Conditional Evaluvation of Monadic Expressions *)
  let whenever (b : bool) (m : unit t) =
    if b then m >>= (fun () -> return ()) else return ()

  let unless (b : bool) (m : unit t) =
    if b then return () else m >>= (fun () -> return ())

  (* Lifting operators *)
  let lift_m (f : 'a -> 'b) = fun m -> m >>= (fun x -> return (f x))

  let lift_m2 f m n = m >>= (fun x -> n >>= (fun y -> return (f x y)))

  let lift_m3 f m n o =
    m >>= (fun x -> n >>= (fun y -> o >>= (fun z -> return (f x y z))))

  let lift_m4 f m n o p =
    m >>= (fun x ->
      n >>= (fun y -> o >>= (fun z -> p >>= (fun w -> return (f x y z w)))))

  let lift_m5 f m n o p q =
    m >>= (fun x ->
      n >>= (fun y ->
        o >>= (fun z ->
          p >>= (fun w -> q >>= (fun v -> return (f x y z w v))))))

  let ap (f : ('a -> 'b) t) (m : 'a t) = f >>= (fun f' -> lift_m f' m)
end

(********************************* MONADS *************************************)

module CPS (D : sig type t end ) : MONAD = struct
  type 'a t       = ('a -> D.t) -> D.t
  let return  x   = fun k -> k x
  let ( >>= ) m f = fun k -> m (fun x -> f x k)
end

module Identity : MONAD = struct
  type 'a t       = 'a
  let return  x   = x
  let ( >>= ) m f = f m
end


module Lazy : MONAD = struct
  type 'a t       = unit -> 'a
  let return x    = fun () -> x
  let ( >>= ) m f = f (m ())
end

module ListM : MONAD = struct
  type 'a t       = 'a list
  let return  x   = [x]
  let ( >>= ) m f = List.flatten (List.map f m)
end



module State (S : sig type t end) : MONAD = struct
  type 'a t       = S.t -> 'a * S.t
  let return  x   = fun s -> (x,s)
  let ( >>= ) m f = fun s -> let (x,s') = m s in f x s'
end

module Reader (Env : sig type t end) : MONAD = struct
  type 'a t       = Env.t -> 'a
  let return  x   = fun e -> x
  let ( >>= ) m f = fun e -> f (m e) e
end

module type MONOID = sig
  type t
  val id : t
  val op : (t * t) -> t
end

module Writer (M : MONOID) : MONAD = struct
  type 'a t           = 'a * M.t
  let return  x       = (x, M.id)
  let ( >>= ) (x,w) f = let (x',w') = f x in (x', M.op (w,w'))
end

(******************************* MONAD PLUS ***********************************)

module type MONAD_PLUS = sig
  include MONAD

  val mzero  : 'a t
  val ( ++ ) : 'a t -> 'a t -> 'a t
end

module MONAD_PLUS_OPS (P : MONAD_PLUS) = struct
  include MONAD_OPS(P)
  open P

  let msum ms     = List.fold_left ( ++ ) mzero ms
  let mfilter p m = m >>= (fun x -> if p x then return x else mzero)
  let guard b = if b then return () else mzero
end

(**************************** MONAD PLUS INSTANCES ****************************)

module Option : MONAD_PLUS = struct
  type 'a t = 'a option

  let return x = Some x

  let ( >>= ) m f = match m with
    | Some x -> f x
    | None   -> None

  let mzero = None
  let ( ++ ) n m = match n,m with
    | Some x, _ -> Some x
    | _         -> m
end
