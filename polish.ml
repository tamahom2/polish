include Auxilary
include Simpl
include Vars
include Sign

(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(*****************************************************************************)


let read_polish (filename:string) (simpl:bool) : program = string_to_instr (read_file filename) simpl

let print_polish (p:program) : unit = print_blk p 

let eval_polish (p:program) : unit = eval_blk p

let usage () =
  print_string "-reprint : print le programme sans les commentaires\n";
  print_string "-eval    : évaluation du programme\n";
  print_string "-simpl   : simplication du programme\n";
  print_string "-vars    : calcul statique des variables\n";
  print_string "-sign    : analyse statique du signe possible des variables lors du déroulement du programme\n"

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file false)
  | [|_;"-eval";file|] -> eval_polish (read_polish file false)
  | [|_;"-simpl";file|] -> print_polish (eliminate_deadblock (read_polish file true))
  | [|_;"-vars";file|] -> vars (read_polish file false)
  | [|_;"-sign";file|] -> sign (read_polish file false)
  | _ -> usage ()

(* lancement de ce main *)
let () = Printexc.record_backtrace true ; main ()
