%{
(*
XXX: ACK!  We have no syntax for actually creating tagged unions!
We also have no way of returning null.
*)

open ErrorReport
open Syntree

(* I use this for testing sometimes.  Seriously! *)
let print_bop () = print_endline "BOP!";;

(* turns 'if ... then ... elif ... then ...  else ... end'
 * into 'if ... then ... else if ... then ... else ... end end'
 *)

let canonicalizeElifs tuplelist finalelse =
   let rec loop lst =
      if lst = [] then
         finalelse
      else
         let line, ifpart, body = List.hd lst in
         [If( line, ifpart, body, loop (List.tl lst) )]
   in
   List.hd (loop tuplelist)
;;

%}

%token LPAREN RPAREN LBRACE RBRACE COMMA COLON ASSIGN
%token IF THEN ELIF ELSE END TYPE TYPECASE STRUCT UNION ENUM
%token WHILE DO DEF DEFUN FUN VAR RETURN CASE
%token EOF
%token <int> INT
%token <float> FLOAT
%token <char> CHAR
%token <string> STRING
%token <string> PARATYPE
%token <string> ID

%type <Syntree.decl list> main
%start main

%%

main: 
   /* empty */ 
   { [] }
 | stmlist
   { $1 }
;


stmlist:
   stm
      {[$1]}
 | stm stmlist
      {$1 :: $2}
;

stm:
   vardecl
   { $1 }
 | typedecl
   { $1 }
;

vardecl:
   DEF ID typedef COLON expr
   { Vardecl( !lineNum, $2, $3, $5 ) } 
 | DEFUN ID funargoption LBRACE rettypeoption RBRACE exprlist END
   { let f = Fun( !lineNum, $3, $5, $7 ) in
     let argnames, argtypes = List.split $3 in
     let ft = FunctionType( argtypes, $5 ) in
     Vardecl( !lineNum, $2, ft, f )
   } 
;

typedecl:
   TYPE typedef COLON typedef
   { Typedecl( !lineNum, $2, $4 ) }
 | STRUCT typedef COLON structmemberlist END
   { Structdecl( !lineNum, $2, $4 ) }
 | UNION typedef COLON unionmemberlist END
   { Uniondecl( !lineNum, $2, $4 ) }
 | ENUM ID COLON enummemberlist END
   { Enumdecl( !lineNum, $2, $4 )}
;

enummemberlist:
   enummember
   { [$1] }
 | enummember enummemberlist
   { $1 :: $2 }
;

enummember:
   ID
   { $1 }
;

structmemberlist:
   structmember
   { [$1] }
 | structmember structmemberlist
   { $1 :: $2 }
;

structmember:
   ID typedef
   { ($1, $2) }
;

unionmemberlist:
   unionmember
   { [$1] }
 | unionmember unionmemberlist
   { $1 :: $2 }
;

unionmember:
   ID LPAREN typedef RPAREN
   { ($1, $3) }
;



typedef:
   ID
   { SimpleType( $1 ) }
 | PARATYPE
   { PolymorphicType( $1 ) }
 | ID LPAREN typedeflist RPAREN
   /*{ ParameterizedType( $1, $3 ) } */
   { SimpleType( $1 ) }
 | FUN LPAREN typedeflist RPAREN LPAREN typedeflist RPAREN
   /*{ FunctionType( $3, $6 ) } */
   { SimpleType( "BLAH" ) }
;

typedeflist:
   typedef
   { [$1] }
 | typedef typedeflist
   { $1 :: $2 }
;



exprlist:
   expr
     { [$1] }
 | expr exprlist
     { $1 :: $2 }
;

expr:
   litexpr
      { Literal( !lineNum, $1 ) }
 | funcallexpr
      { $1 }
 | ifexpr
      { $1 }
 | whileexpr
      { $1 }
 | varexpr
      { $1 }
 | typecaseexpr
      { $1 }
 | funexpr
      { $1 }
 | retexpr
      { $1 }
 | ID
      { Var( !lineNum, $1 ) }
;

litexpr:
   INT
   { IntLit( $1 ) }
 | FLOAT
   { FloatLit( $1 ) }
 | CHAR
   { CharLit( $1 ) }
 | STRING
   { StringLit( $1 ) }
 | arraylit
   { $1 }
 | unionlit
   { $1 }
; 
arraylit:
   LBRACE exprlist RBRACE
   { ArrayLit( $2 ) }
;

unionlit:
   COLON ID LBRACE expr RBRACE
   { UnionLit( $2, $4 ) }
;

funcallexpr:
   LPAREN exprlist RPAREN
   { Funcall( !lineNum, (List.hd $2), (List.tl $2) )}
;

ifexpr:
   IF expr THEN exprlist END
   { If( !lineNum, $2, $4, [None( !lineNum )] ) }
 | IF expr THEN exprlist ELSE exprlist END
   { If( !lineNum, $2, $4, $6 ) }
 | IF expr THEN exprlist eliflist END
   { let a = (!lineNum, $2, $4) :: $5 in
   canonicalizeElifs a [None( !lineNum )] }
 | IF expr THEN exprlist eliflist ELSE exprlist END
   { let a = (!lineNum, $2, $4) :: $5 in
    canonicalizeElifs a $7 }
;

eliflist:
   elif
   { [$1] }
 | elif eliflist
   { $1 :: $2 }
;

elif:
   ELIF expr THEN exprlist
   { (!lineNum, $2, $4) }
;

whileexpr:
   WHILE expr DO exprlist END
   { While( !lineNum, $2, $4 ) }
;

varexpr:
   VAR ID typedef COLON expr
   { Varexpr( !lineNum, $2, $3, $5 ) }
;

typecaseexpr:
   TYPECASE expr COLON typecasememberlist END
   { Typecase( !lineNum, $2, $4 ) }
;

typecasememberlist:
   typecasemember
   { [$1] }
 | typecasemember typecasememberlist
   { $1 :: $2 }
;

typecasemember:
   CASE ID COLON exprlist
   { ($2, $4) }
;

funarglist:
   funarg
   { [$1] }
 | funarg COMMA funarglist
   { $1 :: $3 }
;

funarg:
   ID typedef
   { ($1, $2 )}
;

rettypelist:
   typedef
   { [$1] }
 | typedef COMMA typedeflist
   { $1 :: $3 }
;

rettypeoption:
   rettypelist
     {$1}
 |  /* empty */
     {[]}
;

funargoption:
   funarglist
     {$1}
 | /* empty */
     {[]}
;

funexpr:
   FUN funargoption LBRACE rettypeoption RBRACE exprlist END 
   { Fun( !lineNum, $2, $4, $6 ) }
;

retexpr:
   RETURN expr
   { Return( !lineNum, $2 ) }
;

      

%%

