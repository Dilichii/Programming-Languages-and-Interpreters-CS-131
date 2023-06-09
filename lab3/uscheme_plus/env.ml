open Sexprlib


module StringMap = Map.Make(String)

type env = ((value ref) StringMap.t)

and value = 
  | PrimFuncVal  of (Loc.loc -> value list -> value)
  | UserFuncVal  of string list * Ast.expr * env
  | UnitVal
  | BoolVal      of bool
  | IntVal       of int
  | SymVal       of Ast.id
  | NilVal
  | PairVal      of value * value
  | Unspecified  of string


let rec string_of_value v =
  match v with
    | SymVal _
    | PairVal _ -> "'" ^ _string_of_value v
    | _ -> _string_of_value v

and _string_of_value = function
  | UserFuncVal _  -> "<user function>"
  | PrimFuncVal _  -> "<primitive function>"
  | UnitVal        -> "#u"
  | BoolVal true   -> "#t"
  | BoolVal false  -> "#f"
  | IntVal i       -> string_of_int i
  | SymVal s       -> s 
  | NilVal         -> "nil"
  | PairVal (a, b) -> "(" ^ string_of_pair a b ^ ")"
  | Unspecified s  -> "<unspecified value for " ^ s ^ ">"

and string_of_pair a b =
  let a' =
    match a with
      | SymVal s -> s
      | _ -> _string_of_value a
  in
    match b with
      | NilVal         -> a'
      | PairVal (b, c) -> a' ^ " " ^ string_of_pair b c
      | other          -> a' ^ " . " ^ _string_of_value other

let truthy l = function
  | BoolVal false -> false
  | Unspecified s -> Error.unspecified_err l s
  | _ -> true

let mem name env = StringMap.mem name env

let lookup l env name = 
  try !(StringMap.find name env)
  with Not_found -> Error.name_err l name

let set l env name v =
  try StringMap.find name env := v
  with Not_found -> Error.name_err l name

(* Make a new name/value binding in an environment.
 * If the name already exists, do not overwrite it,
 * but replace the old binding with the new one.
 * (This is a subtle point.) *)
let bind env name v = 
  let e1 = StringMap.remove name env in
    StringMap.add name (ref v) e1

let make_env alist = 
  List.fold_left 
    (fun e (n, v) -> StringMap.add n (ref v) e) StringMap.empty alist

