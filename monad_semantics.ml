(* Implementation of the monad semantics for IMP *)

(* Here is my implementation for a functor that spits out
 * various semantics (interpreters) for a simple imperative language
 * similar to IMP (c.f. Winskel).
 * 
 * Notes:
 *   - This could be made significantly more sexy with GADTs, but I
 *     haven't had the time to build that up yet and the syntax is
 *     still greek to me
 *   - the next step would be to build an abstract interpreter for
 *     the AST and have the most general monad semantics for the
 *     language
 *   - Note that Lift is naturally isomorphic to Maybe/Option via:
 *     Inj x -> Some x | Bottom -> None. I just called it lift because
 *     that is more similar to how you see translations of this type
 *     written down in "semantics notation".
 *
 *  - bkc 4/30/2013
 *)

module type MONAD = sig
  type 'a t
  val unit : 'a           -> 'a t
  val bind : ('a -> 'b t) -> 'a t -> 'b t
end

module AST = struct
  type op   = Add | Sub | Mul | Leq | Eq
  type aexp = Var of string | Val of int | Op of op * aexp * aexp
  type bexp = T | F | Not of bexp | And of bexp * bexp | Eq of aexp * aexp
  type com  = Skip
	      | Assign of aexp * aexp
	      | Seq of com * com
	      | If of bexp * com * com
	      | While of bexp * com
  type exp  = com
end

(* Recall that the translation of an IMP program is a map C: D -> M(D),
 *  where D is the source language and M(D) is the lift of D. We can model
 * this by letting the source be the AST  with a natural natural injection 
 * into the target, as well as a bottom element for non-termination.
 *)
module Lift : MONAD = struct
  type 'a t = Inj of 'a | Bottom

  let unit s = Inj s
  let bind f = function
    | Inj s  -> f s
    | Bottom   -> Bottom
end

(* Here is the CPS translation for the AST given via the monad
 * transformer below.
 * 
 * Note: We specify this as a functor taking the output domain to the
 * desired monad because the CPS translation does not depend on the
 * continuation codomain. Because the module type TRANSLATE takes a
 * MONAD we must specify an output domain before we can translate.
 * - bkc
 *)
module type ANSWER = sig type t end
module CPS (D : ANSWER) : MONAD = struct
  type 'a t = ('a -> D.t) -> D.t
  let unit s = fun k -> k s
  let bind f m = fun k -> m (fun s -> f s k)
end

module type TRANSLATE = functor (M : MONAD) -> sig
  type store
  exception Error of string
  val translate : AST.exp -> store -> store M.t
end

(* Given a monad, generates the monad semantics for the AST above.
 * If we pass in the Lift monad (isomorphic to the option monad)
 * then we get the usual denotational semantics for the AST
 * 
 * Note: This code could be made much prettier with GADTS
 *       because we could eliminate the aok, bok functions
 *       which are unnecessarily complicated due to the
 *       typechecking logic. GADTS would pass the burden off
 *       to ocaml. -bkc
 *)
module Translate : TRANSLATE = functor (M : MONAD) -> struct
  type store = AST.aexp -> int
  exception Error of string

  (* Helper functions *)
  let extend (s : store) (x : AST.aexp) (v : AST.aexp) : store =
    match x,v with
    | AST.Var _, AST.Val n -> fun y -> if y = x then n else s y
    | _ -> raise (Error "Type error: expected (Var,Val)")

  let aok = function
    | AST.Val n -> n
    | _         -> raise (Error "Type error: expected Value") 

  let rec fix f x = f (fix f) x

  (* Primitive expression translations *)
  let rec atrans (exp : AST.aexp) (s : store) =
    match exp with
    | AST.Var _ | AST.Val _ -> exp
    | AST.Op (f,a1,a2)      -> begin
      match f with
      | AST.Add -> AST.Val ((aok (atrans a1 s)) + (aok (atrans a2 s)))
      | AST.Sub -> AST.Val ((aok (atrans a1 s)) - (aok (atrans a2 s)))
      | AST.Mul -> AST.Val ((aok (atrans a1 s)) * (aok (atrans a2 s)))
      | _       -> raise (Error "Invalid Op: Expected +,-,*")
    end

  let rec btrans (exp : AST.bexp) (s : store) =
    match exp with
    | AST.T -> true
    | AST.F -> false
    | AST.Not b -> not (btrans b s)
    | AST.And (b1,b2) -> (btrans b1 s) && (btrans b2 s)
    | AST.Eq (a1,a2) -> begin
      let t1,t2 = (atrans a1 s), (atrans a2 s) in
      match t1,t2 with
      | AST.Val n, AST.Val m -> n = m
      | AST.Var _, AST.Var _ -> (s t1) = (s t2)
      | AST.Val n, AST.Var _ -> n = (s t2)
      | AST.Var _, AST.Val n -> (s t1) = n
      | _                    -> raise (Error "Type Error: Var/Val expected")
    end

  (* What we came here for: The MONAD semantics.
   * 
   * Note: We could also pass in the original store unmodified
   *       so that we don't return a store with some of the variables
   *       overwritten. Alternatively, we could wrap the values in a
   *       Maybe monad and have type AST.exp -> store -> store M.t Maybe.t
   *       - bkc
   *)
  let rec translate (exp : AST.exp) (s : store) : store M.t =
    try begin
      match exp with
      | AST.Skip           -> M.unit s
      | AST.Assign (a1,a2) -> M.unit (extend s (atrans a1 s) (atrans a2 s))
      | AST.Seq (c1,c2)    -> (M.bind (translate c2)) (translate c1 s)
      | AST.If (b,c1,c2)   -> begin
        if btrans b s then translate c1 s else translate c2 s 
      end
      | AST.While (b,c)    -> begin
        fix (fun w s ->
          if btrans b s then M.bind w (translate c s) else M.unit s) s
      end
    end 
    with Error message -> begin
      (print_endline message);
      (print_endline "Returning partial computation");
      M.unit s
    end
end

module DenotationalSemantics = Translate (Lift)
module CPSSemantics (D : ANSWER) = Translate (CPS (D))
