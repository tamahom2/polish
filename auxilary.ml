include Types
include Convertor
(** Auxilary functions to read and eval Polish *)

module SS = Set.Make(String);;


(** Initialisation de la Hashmap *)
let var_map = Hashtbl.create 100;;

(** Error list*)
let errors = ref []

(** Add errors *)
let append_err (pos:int) = errors := (Printf.sprintf "Divbyzero in %d" pos)::!errors;;
let append_err2 (pos:int) = errors := (Printf.sprintf "Not defined in %d" pos)::!errors;;


(** Fonctions primaires*)

(** Lecture du fichier filename : écris le contenu de filename dans une liste ls *)
let read_file (filename:string) : lines =
  let ls = ref [] in 
  let pos = ref 1 in 
  let rd = open_in filename in
  try 
    while true; do 
      ls := (!pos,input_line rd):: !ls;
      pos := !pos +1;
    done; List.rev !ls 
  with End_of_file ->
    close_in rd; 
    List.rev !ls 
;;

exception Division_by_zero;;

(** Evaluation en OCaml d'une expression arithmétiques en Polish *)
let rec eval_expr (expre:expr) : int = match expre with
  | Num(a) -> a
  | Op(a,x,y) -> (match a with 
      | Add -> eval_expr x + eval_expr y 
      | Sub -> eval_expr x - eval_expr y 
      | Mul -> eval_expr x * eval_expr y 
      | Div -> if eval_expr y = 0 then raise Division_by_zero else eval_expr x / eval_expr y
      | Mod -> if eval_expr y = 0 then raise Division_by_zero else eval_expr x mod eval_expr y )
  |Var(a) -> Hashtbl.find var_map a 
;;

(** Evaluation en OCaml d'une condition en Polish *)
let eval_cond (condition:cond) : bool = match condition with
    | (expr1,comp,expr2)->(match comp with
        | Eq -> eval_expr expr1 = eval_expr expr2 
        | Ne -> eval_expr expr1 <> eval_expr expr2 
        | Lt -> eval_expr expr1 < eval_expr expr2 
        | Le -> eval_expr expr1 <= eval_expr expr2 
        | Gt -> eval_expr expr1 > eval_expr expr2 
        | Ge -> eval_expr expr1 >= eval_expr expr2 )
;; 

(** Read expression *)
let read_expr (n:string) : unit = 
let ()  = print_string (Printf.sprintf "Read the value of %s :\n" n)
in let a =  int_of_string (String.trim (read_line()))
in Hashtbl.add var_map n a
;;

(** Met à jour la valeur de la variable n dans la VarMap *)
let set_val (n:string) (expre:expr) : unit = 
    let value = (eval_expr expre)
    in Hashtbl.remove var_map n;
    Hashtbl.add var_map n value
;;

(** Print le block *)
let print_blk (blk:block)= 
  let rec aux depth blk = match blk with
  | [] -> print_string ""
  | a::blk -> match a with
      |(pos,inst) ->( match inst with
        | Set(a,b) -> (print_string ((String.make (depth*2) ' ')^(Printf.sprintf "%s := %s\n" a (expr_to_string b)));
                        aux depth blk)
        | Read(a) -> (print_string ((String.make (depth*2) ' ')^(Printf.sprintf "READ %s\n" a));
                      aux depth blk)
        | Print(a) -> (print_string ((String.make (depth*2) ' ')^(Printf.sprintf "PRINT %s\n" (expr_to_string a)));
                       aux depth blk)
        | If(cnd,blk1,blk2) when List.length blk2 = 0 -> ( print_string ((String.make (depth*2) ' ')^(Printf.sprintf "IF %s\n" (cond_to_string cnd))); 
                                                          aux (depth+1) blk1 ;
                                                          aux  depth blk;
                                                         )
        | If(cnd,blk1,blk2) -> ( print_string ((String.make (depth*2) ' ')^(Printf.sprintf "IF %s\n" (cond_to_string cnd)));
                                 aux (depth+1) blk1 ;
                                 print_string ((String.make (depth*2) ' ')^(Printf.sprintf "ELSE\n"));
                                 aux (depth+1) blk2;
                                 aux depth blk)
        | While (cnd,blk1) -> (print_string ((String.make (depth*2) ' ')^(Printf.sprintf "WHILE %s\n" (cond_to_string cnd))); 
                               aux (depth+1) blk1;
                               aux depth blk;)
      )
      in aux 0 blk
;;

(**Evaluation d'un block d'instructions *)
let rec eval_blk (blk:block) = match blk with
  | [] -> print_string ""
  | a::blk -> match a with
      |(pos,inst) ->( match inst with
        | Set(a,b) -> set_val a b;eval_blk blk
        | Read(a) -> read_expr a;eval_blk blk
        | Print(a) -> Format.printf "%d\n" (eval_expr a); eval_blk blk
        | If(cnd,blk1,blk2) when List.length blk2 = 0 -> (if(eval_cond cnd) then (eval_blk blk1;eval_blk blk) else eval_blk blk) 
        | If(cnd,blk1,blk2) -> if(eval_cond cnd) then (eval_blk blk1;eval_blk blk) else (eval_blk blk2;eval_blk blk)
        | While (cnd,blk1) -> while(eval_cond cnd) do eval_blk blk1 done; eval_blk blk
      )
;;

