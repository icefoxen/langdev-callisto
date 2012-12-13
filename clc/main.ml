(* main.ml
   Main driver and UI junk.
   Not quite sure how the overall toolchain is going to be structured yet.
   As in, compiler, linker, assembler, package manager, library manager...

   Simon Heath
   Whenever
*)

let compileFile fn outfile =
  ErrorReport.reset fn;
  try
    (* Kludge the module name from the filename... 
       For now, we don't actually have heirarchical modules.  Boo hoo.
    *)
    (*let modulename = [(String.sub fn 0 (String.length fn - 3))] in *)
    let instream = open_in fn in
    let lexbuf = Lexing.from_channel instream in
    let parsetree = Parse.main Lex.token lexbuf in
    ignore parsetree;
    print_endline "Parsing done";
    (* List.iter (fun x -> print_int x) parsetree; *)
(*      Printf.printf "# of statements parsed: %d\n" (List.length parsetree); *)
      (*close_in instream;
      print_endline "Parsing succeeded!  Have a cookie!";
      Modules.dumpInterfaceToFile parsetree modulename;
      let symtbl, ir = (Semant.doSemanticStuff parsetree modulename) in
      let newir = IrTransform.transformIR ir in
      Ir.printIR newir
      *)
(*

	print_endline "Semantic junk succeceded!  Have another cookie!";
	let asmmodule = (Codegen.compileJunk symtbl ir (fn ^ ".asm")) in
	  print_endline "Compiled to assembly!  Have a pie!";
	  X86nasm.writeModule asmmodule (outfile);
*)

  with
      Sys_error a -> (Printf.eprintf "File does not exist: %s\n" a; 
		      exit 1)
    | Parsing.Parse_error -> 
	ErrorReport.error "Fatal parse error: Bad programmer, no cookie for you!"
;;

let usage () = 
  let name = Sys.argv.(0) in
  Printf.printf "Usage: %s filename\n" name;
;;


let _ =
  let fn = Sys.argv.(1) in
  let outfile = (String.sub fn 0 (String.length fn - 3)) ^ ".ct" in
    compileFile fn outfile
  
;;

