open Sexprlib

type value = 
  | Unit
  | Bool  of bool
  | Int   of int
  | Array of value array

type func = 
  | PrimFunction of (Loc.loc -> value list -> env -> env * value)
  | UserFunction of Ast.id list * (Ast.id * value) list * Ast.exp
and env = (value, func) Env.t

let rec string_of_value = function
  | Unit       -> "#u"
  | Bool true  -> "#t"
  | Bool false -> "#f"
  | Int i      -> string_of_int i
  | Array a    -> let funn p curr = 
                if p = "" then "[" ^ (string_of_value curr)
                else p ^ " " ^ (string_of_value curr) in
                (Array.fold_left funn "" a) ^"]"
    
    (*
    
    begin 
                    Printf.printf "[";
                    let iter arr = 
                      match arr with
                      | [] -> ""
                      | h :: t -> begin (Printf.printf "" ^ string_of_value h); iter t end
                    in (Array.to_list a);
                    Printf.printf "]";
                    "#u"
                    end *)

let rec equal_values l v1 v2 =
  match (v1, v2) with 
    | (Unit, Unit) -> true
    | (Bool b1, Bool b2) -> b1 = b2
    | (Int i1, Int i2) -> i1 = i2
    | (Array arr1, Array arr2) -> let rec iter a1 a2 =
                                    match (a1, a2) with
                                    | (_ :: _, []) -> false
                                    | ([], _ :: _) -> false
                                    | ([], []) -> true
                                    | (h1 :: t1, h2 :: t2) -> if (equal_values l h1 h2) 
                                                          then iter t1 t2 else false
                                  in iter (Array.to_list arr1) (Array.to_list arr2)
    | _ -> Error.bug_in_type_checker_err l 
             "equality test requires values of the same type"
