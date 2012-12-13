
type id = string

(* XXX: We need to make sure these don't result in recursive toplevel
 * dependancies.
 *)
and litval =
   IntLit of int
 | FloatLit of float
 | CharLit of char
 | StringLit of string
 | ArrayLit of expr list
 | UnionLit of string * expr


and symtype = {
  vname : id;
  vtype : typeval;
  (*mutable vloc : int; *)
}

and typeval = 
   SimpleType of string
 | PolymorphicType of string
 | ParameterizedType of string * typeval list 
 | FunctionType of typeval list * typeval list


(* All of these carry their line numbers with them. *)
and expr =
  (* Name, value *)
    Varexpr of int * id * typeval * expr

  (* Condition, ifpart, elsepart *)
  | If of int * expr * expr list * expr list

  (* Condition, body *)
  | While of int * expr * expr list

  (* Function, params *)
  | Funcall of int * expr * expr list

  | Var of int * id

  | Typecase of int * expr * (id * expr list) list

  (* Arguments, return type, body *)
  | Fun of int * (id * typeval) list * (typeval list) * expr list

  | Return of int * expr

  | Literal of int * litval

  | None of int

;;


type decl =
   Vardecl of int * id * typeval * expr
 | Typedecl of int * typeval * typeval
 | Enumdecl of int * id * id list
 | Structdecl of int * typeval * (id * typeval) list
 | Uniondecl of int * typeval * (id * typeval) list
 | Nulldecl (* For testing *)
;;
