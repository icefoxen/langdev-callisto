(* lex.mll
   A lexer for Callisto
*)

{

open Parse
open ErrorReport
exception Eof
exception Lexer_error

let inComment = ref 0;;

(* Abbreviation for the func that returns the string
   being lexed.
*)
let gs = Lexing.lexeme;;

(* Advances the position of the error-checking vars. *)
let adv lb =
  (*
  let c = (gs lb) in
  if c <> " " then
     Printf.printf "Lexed: '%s'\n" (gs lb);
  *)
  chrNum := !chrNum + (String.length (Lexing.lexeme lb))
;;

let str2float x =
   Scanf.sscanf x "%f" (fun x -> x)
;;

let str2int x =
   Scanf.sscanf x "%i" (fun x -> x)
;;
let str2char x =
   Scanf.sscanf x "%C" (fun x -> x) 

let str2str x =
   Scanf.sscanf x "%S" (fun x -> x) 
;;

}


let id = 
   ['a'-'z' 'A'-'Z' '_' '+' '-' '/' '*' '%' '!' '>' '<' '=' '@' '#' '$'
   '^' '&' '|' '?']
   ['a'-'z' 'A'-'Z' '0'-'9' '_' '+' '-' '/' '*' '%' '!' '>' '<' '=' '@' '#' '$'
   '^' '&' '|' '?']*

let paratype = 
   '%'['a'-'z' 'A'-'Z' '_' '+' '-' '/' '*' '%' '!' '>' '<' '=' '@' '#' '$'
   '^' '&' '|' '?']+

let inum =
   '-'?(['0'-'9']+|"0x"['0'-'9''a'-'f''A'-'F']+|"0o"['0'-'7']+)
let bnum =
   '-'?"0b"['0''1']+
let fnum =
   '-'?['0'-'9']+'.'['0'-'9']*
let chr =
   ("'"_"'") | ("'\\"(inum|bnum)"'") | ("'\\"("n"|"b"|"r"|"t"|"'"|"\\")"'")

(* XXX: To do: parse character thingies in strings! *)
let str = '"'([^'"''\\']|'\\'_)*'"'

rule token = parse
   ";"			{ adv lexbuf; lcomment lexbuf }
 | "/-"			{ adv lexbuf; incr inComment; bcomment lexbuf }
 | (inum|bnum)		{ adv lexbuf; INT( str2int (gs lexbuf) ) }
 | fnum			{ adv lexbuf; FLOAT( str2float (gs lexbuf) ) }
 | chr                  { adv lexbuf; CHAR( str2char (gs lexbuf) ) }
 | str                  { adv lexbuf; STRING( str2str (gs lexbuf) ) }
 | "\n"			{ nl (); token lexbuf }
 | [' ''\t']		{ adv lexbuf; token lexbuf }
 | "("			{ adv lexbuf; LPAREN }
 | ")"			{ adv lexbuf; RPAREN }
 | "{"                  { adv lexbuf; LBRACE }
 | "}"                  { adv lexbuf; RBRACE }
 | "<-"                  { adv lexbuf; ASSIGN }
 | "if"			{ adv lexbuf; IF }
 | "then"                  { adv lexbuf; THEN }
 | "elif"                  { adv lexbuf; ELIF }
 | "else"                  { adv lexbuf; ELSE }
 | "end"                  { adv lexbuf; END }
 | "while"                  { adv lexbuf; WHILE }
 | "do"                  { adv lexbuf; DO }
 | "def"                  { adv lexbuf; DEF }
 | "defun"                  { adv lexbuf; DEFUN }
 | "fun"                  { adv lexbuf; FUN }
 | "type"                  { adv lexbuf; TYPE }
 | "case"                  { adv lexbuf; CASE }
 | "typecase"                  { adv lexbuf; TYPECASE }
 | "struct"                  { adv lexbuf; STRUCT }
 | "enum"                  { adv lexbuf; ENUM }
 | "union"                  { adv lexbuf; UNION }
 | "var"                    { adv lexbuf; VAR }
 | ":"                  { adv lexbuf; COLON }
 | paratype             { adv lexbuf; PARATYPE( (gs lexbuf) ) }
 | id                   { adv lexbuf; ID( (gs lexbuf) ) }
 | eof			{ EOF }
 | _			{ errorAndDie "Invalid token!" }

and bcomment = parse
   "/-"			{ adv lexbuf; incr inComment; 
   (*Printf.printf "Pushing comment stack, now %d\n" !inComment;*)
                          bcomment lexbuf }
 | "-/"			{ adv lexbuf; decr inComment; 
   (*Printf.printf "Popping comment stack, now %d\n" !inComment;*)
                          if !inComment <= 0 then token lexbuf
			                    else bcomment lexbuf }
 | '\n'			{ nl (); bcomment lexbuf }
 | _			{ adv lexbuf; bcomment lexbuf } 


and lcomment = parse
   '\n'			{ nl (); token lexbuf }
 | _			{ adv lexbuf; lcomment lexbuf }
