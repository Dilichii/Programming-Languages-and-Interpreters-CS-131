(* Abstract syntax tree and parsing. *)

open Sexprlib
open Loc

(* Type of identifiers. *)
type id = string
type lit = 
  | Integer of int
  | Boolean of bool
  | Unit 


(* Scheme expressions. *)
type expr =
  | Literal of loc * lit
  | Quote   of loc * Sexpr.expr
  | Var     of loc * id
  | Set     of loc * id * expr
  | If      of loc * expr * expr * expr
  | While   of loc * expr * expr list
  | Begin   of loc * expr list
  | Let     of loc * (id * expr) list * expr list
  | LetStar of loc * (id * expr) list * expr list
  | LetRec  of loc * (id * expr) list * expr list
  | Lambda  of loc * id list * expr list
  | LambdaX of loc * id list * id * expr list
  | Call    of loc * expr * expr list
  | Cond    of loc * (expr list) list 
  | And     of loc * expr list
  | Or      of loc * expr list
  

(* Scheme top-level forms. *)
type def = 
  | Val         of loc * id * expr
  | Define      of loc * id * id list * expr list
  | Exp         of loc * expr
  | Use         of loc * string
  | CheckExpect of loc * expr * expr
  | CheckError  of loc * expr
  | ValRec      of loc * (id * expr) list
  | DefineX     of loc * id * id list * id * expr list
  

val loc_of_expr : expr -> loc
val loc_of_def  : def -> loc

(*
 * Syntax analysis.
 *)

val parse_expr : Sexpr.expr -> expr
val parse_def  : Sexpr.expr -> def
