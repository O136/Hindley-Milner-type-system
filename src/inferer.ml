open Ast

let code = ref (Char.code 'a')

let reset_type_vars() = code := Char.code 'a'

let next_type_var() : typ =
  let c = !code in
  if c > Char.code 'z' then failwith "too many type variables";
  incr code;
  TVar (String.make 1 (Char.chr c))

let type_of (ae : aexpr) : typ =
    match ae with
    | AVar (_, a) -> a
    | AFun (_, _, a) -> a
    | AApp (_, _, a) -> a
    | AConst (ConstBool _) -> TBool
    | AConst (ConstInt _) -> TInt

(* annotate all subexpressions with types *)
(* bv = stack of bound variables for which current expression is in scope *)
(* fv = hashtable of known free variables *)
let annotate (e : expr) : aexpr =
    let (h : (id, typ) Hashtbl.t) = Hashtbl.create 17 in
    let rec annotate' (e : expr) (bv : (id * typ) list) : aexpr =
        match e with
        | Var x ->
            (* bound variable? *)
            (try let a = List.assoc x bv in 
                AVar (x, a)
            (* known free variable? *)
            with Not_found -> 
                try let a = Hashtbl.find h x in 
                    AVar (x, a)
                (* unknown free variable *)
                with Not_found -> 
                    let a = next_type_var() in 
                    Hashtbl.add h x a;
                    AVar (x, a))
        | Fun (x, e) ->
            (* assign a new type to x *)
            let a = next_type_var() in
            let ae = annotate' e ((x, a) :: bv) in
                AFun (x, ae, Arrow (a, type_of ae))
        | App (e1, e2) ->
            AApp (annotate' e1 bv, annotate' e2 bv, next_type_var())
        | Const c -> AConst c
        in annotate' e []
(* collect constraints for unification *)
(* let collect (aexprs : aexpr list) : (typ * typ) list =  *)
let rec collect' (aexprs : aexpr list) (u : (typ * typ) list) : (typ * typ) list =
  match aexprs with
    | [] -> u
    | AVar (_, _) :: r -> collect' r u
    | AFun (_, ae, _) :: r -> collect' (ae :: r) u
    | AApp (ae1, ae2, a) :: r ->
        let (f, b) = (type_of ae1, type_of ae2) in
        collect' (ae1 :: ae2 :: r) ((f, Arrow (b, a)) :: u)
    | _ -> u
(* in collect' aexprs []  *)
let infer (e : expr) : typ = failwith "Not implemented" 