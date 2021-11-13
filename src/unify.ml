(*****************************
* Unification of type terms *
*****************************)
(* hello world comment *)
open Ast

(* invariant for substitutions: no id on a lhs occurs in any term earlier  *)
(* in the list                                                             *)
type substitution = (id * typ) list

(* check if a variable occurs in a term *)
let rec mem (x : id) (t : typ) : bool =
    match t with
    | TInt 
    | TBool -> false
    | TVar y -> x = y
    | Arrow (u, v) -> mem x u || mem x v

(* substitute term s for all occurrences of var x in term t,
   wherever there is var "x" in term t, that will be
   replaced by term s *)
let rec subst (s : typ) (x : id) (t : typ) : typ =
    match t with
    | TVar y -> if x = y then s else t
    | Arrow (u, v) -> Arrow (subst s x u, subst s x v)
    | r -> r (*TInt either TBool*)

(* apply a substitution to t right to left *)
let apply (s : substitution) (t : typ) : typ =
    List.fold_right (fun (x, e) -> subst e x) s t

(* unify one pair *)
let rec unify_one (s : typ) (t : typ) : substitution =
    match (s, t) with
    | (TVar x, TVar y) -> if x = y then [] else [(x, t)]
    | (Arrow (x, y), Arrow (u, v)) -> unify [(x, u); (y, v)]
    | (TVar x, (Arrow (u, v) as z)) 
    | ((Arrow (u, v) as z), TVar x) ->
        if mem x z
        then failwith "not unifiable: circularity"
        else [(x, z)]
    | (TInt, TBool)   
    | (TBool, TInt) -> failwith "non unifiable: bool & int"

    | (_, TVar x) -> [(x, s)] (*TInt or TBool*)
    | (TVar x, _) -> [(x, t)] 
    (* | ((Arrow (u, v)), _) -> let (TVar x) = Inferer.next_type_var() in 
        [(x, t)] (*TInt or TBool*)
    | (_, (Arrow (u, v))) -> let (TVar x) = Inferer.next_type_var() in 
        [(x, s)] *)
    | (_, _) -> []

(* unify a list of pairs *)
and unify (s : (typ * typ) list) : substitution =
    match s with
    | [] -> []
    | (x, y) :: t ->
        let t2 = unify t in
        let t1 = unify_one (apply t2 x) (apply t2 y) in
        t1 @ t2