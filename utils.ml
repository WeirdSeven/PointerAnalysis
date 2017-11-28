(*
 * Graduate Programming Languages - Wes Weimer
 *
 * Test Input Generation Project - Global Utilities
 *
 * All of the real action happens in "tigen.ml". 
 *)
open Cil

(* 
 * This "debugging output" function behaves just like printf(), but it also
 * flushes the output buffer so that you'll always see all of your output,
 * even if the program terminates abnormally. 
 *)
let debug fmt = 
  let k result = begin
    output_string stdout result ; 
    flush stdout ; 
  end in
  Printf.kprintf k fmt 

let abort fmt = 
  debug fmt ;
  exit 1 

(* 
 * Load, lex and parse and pre-processed C-language file (typically a .i
 * file obtained by "gcc -E" or "gcc -save-temps") into a Cil.file abstract
 * syntax tree data structure.
 *)
let load_c_file (filename : string) : Cil.file = 
  Frontc.parse filename () 

(*
 * Pretty-print a Cil expression as a string. Handy for debugging. 
 *)
let cil_exp_to_str (e : Cil.exp) : string = 
  Pretty.sprint ~width:80 (d_exp () e) 

(* 
 * returns the first N elements of the given list 
 *) 
let rec first_nth lst n =  
  if n < 1 then [] 
  else match lst with
  | [] -> []
  | hd :: tl -> hd :: (first_nth tl (pred n))

(*let print_statement statement = 
  debug "%s\n" (Pretty.sprint ~width:80 (d_stmt () statement))

let print_expression expression = 
  debug "Assume %s\n" (Pretty.sprint ~width:80 (d_exp () expression)) 

let print_block block = 
  debug "%s\n" (Pretty.sprint ~width:80 (d_block () block)) 

let print_path path = 
  List.iter (fun step -> 
    match step with
  | Statement(s) -> print_statement s 
  | Assume(e) ->  print_expression e
  ) path

let print_path_exploration path_exploration = match path_exploration with 
  | Exploring_Block(block) -> debug "Path exploration: block"; 
                              print_block blcok
  | Exploring_Statement(stmt) -> debug "Path exploration: statement"; 
                                 print_statement stmt
  | Exploring_Done -> debug "Path exploration: done" 

let print_path_exploration_list path_exploration_list =
  List.iter (fun path_exploration -> print_path_exploration path_exploration) path_exploration_list

let print_work path here nn nb nc =
  debug "***Print path***\n";
  print_path path;
  debug "***Print current exploration***\n";
  print_path_exploration here;
  debug "***Print next if current is normal***\n";
  print_path_exploration_list nn;
  debug "***Print next if current is break***\n";
  print_path_exploration_list nb;
  debug "***Print next if current is continue***\n";
  print_path_exploration_list nc;
  debug "*******************Done*******************"*)















