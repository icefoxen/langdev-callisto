(* Intermediate representation. 
 * Explicit jumps, 2-operation stuff, maybe explicit applies, simpler control
 * structures, that sorta stuff...
 *)

type ir =
   OpStm of op * ir * ir
   | LitStm of literal


and op =
   IntAdd
   | IntSub
   | IntMul
   | IntDiv
   | IntMod

and literal =
   IntLit of Int64.t
;;
