(*****************************
* Unification of type terms *
*****************************)

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