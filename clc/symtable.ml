(* With a linked list of linked lists, there's a lot of O(n^2)'s running
 * around here... however, n should just about always be < 10ish, so.
 *)
open ErrorReport
open Syntree

type scope = {
   mutable svars : symtype list
};;

type symtable = scope list;;

let symtableIsEmpty symtable =
   symtable = []

let getTopScope symtable =
   if symtableIsEmpty symtable then
      errorAndDie "getTopScope: Symbol table underflow!  Eeek!"
   else
      List.hd symtable
;;

let getNextScope symtable =
   if symtableIsEmpty symtable then
      errorAndDie "getNextScope: Symbol table underflow!  Eeek!"
   else
      List.tl symtable
;;

let varIsBoundInToplevel vname symtable =
   let t = getTopScope symtable in
   List.exists (fun x -> x.vname = vname) t
;;

let getVarInToplevel vname symtable =
   let t = getTopScope symtable in
   try
      List.find (fun x -> x.vname = vname) t
   with
      Not_found -> errorAndDie "getVarInToplevel: Hopefully, this shouldn't happen."
;;


let rec varIsBound vname symtable =
   if symtableIsEmpty symtable then
      false
   else if varIsBoundInToplevel vname symtable then
      true
   else
      varIsBound vname (getNextScope symtable)
;;

let makeVar vname vtype = {
   vname = vname;
   vtype = vtype;
};;


let bindVar vname vtype symtable =
   let v = makeVar vname vtype
   and top = getTopScope symtable
   and next = getNextScope symtable in
   if varIsBoundInToplevel vname symtable then
      errorAndDie ("bindVar: Redefining variable " ^ vname)
   else
      (v :: top) :: next
;;


let rec getVar vname symtable =
   if symtableIsEmpty symtable then
      errorAndDie ("getVar: var not found: " ^ vname)
   else if varIsBoundInToplevel vname symtable then
      getVarInToplevel vname symtable
   else
      getVar vname (getNextScope symtable)
;;
