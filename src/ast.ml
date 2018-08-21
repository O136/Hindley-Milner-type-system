(*********************************************************************
 * Abstract syntax tree for lambda expressions and type expressions *
 *********************************************************************)

type id = string

(* type expressions *)
type typ =
    | TBool
    | TInt
    | TVar of id
    | Arrow of typ * typ
    [@@deriving show, eq, read]

type const =
    | ConstBool of bool
    | ConstInt of int
    [@@deriving show, eq, read]

type expr =
    | Fun of id * expr
    | App of expr * expr
    | Var of id
    | Const of const
    [@@deriving show, eq, read]

(* annotated expressions *)
type aexpr =
    | AFun of id * aexpr * typ
    | AApp of aexpr * aexpr * typ
    | AVar of id * typ
    | AConst of const
    [@@deriving show, eq, read]
