include Types
include Auxilary

(** Fonctions pour l'option -vars *)

(** Evaluation en OCaml d'une expression arithmétiques en Polish *)
let rec eval_expr_vars expre init_var none_init = match expre with
  | Num(a) -> (init_var,none_init)
  | Op(a,x,y) -> let (newinit,newnone) = eval_expr_vars x init_var none_init in let (newinit2,newnone2) = eval_expr_vars y init_var none_init in  (SS.union init_var (SS.union newinit newinit2),SS.union none_init (SS.union newnone newnone2))
  | Var(a) -> if(SS.exists (fun x-> x=a) init_var) then (init_var,none_init) else (init_var,SS.add a none_init)
;;

(** Evaluation en OCaml d'une condition en Polish *)
let eval_cond_vars condition init_var none_init = match condition with
    | (expr1,comp,expr2)-> let (newinit,newnone) = eval_expr_vars expr1 init_var none_init in let (newinit2,newnone2) = eval_expr_vars expr2 init_var none_init in (SS.union init_var (SS.union newinit newinit2),SS.union none_init (SS.union newnone newnone2))
;;

(** Evaluation d'un block d'instructions *)
let eval_blk_vars (blk:block) = 
  let init_var = SS.empty
  in  let none_init = SS.empty
  in let rec aux blk init_var none_init = match blk with
  | [] -> (init_var,none_init)
  | a::blk -> (match a with
      |(pos,inst) ->( match inst with
        | Set(a,b) -> (let (newinit,newnone) = eval_expr_vars b init_var none_init
                      in aux blk (SS.union init_var (SS.add a newinit)) (SS.union none_init newnone))
        | Read(a) -> aux blk (SS.add a init_var) none_init
        | Print(a) -> (let (newinit,newnone) = eval_expr_vars a init_var none_init 
                      in aux blk (SS.union init_var newinit) (SS.union none_init newnone))
        | If(cnd,blk1,blk2) -> (let (newinit,newnone) = eval_cond_vars cnd init_var none_init 
                                in let (newinit2,newnone2) = aux blk1 init_var none_init 
                                in let (newinit3,newnone3) = aux blk2 init_var none_init 
                                in let (newinit4,newnone4) = aux blk ((SS.union init_var (SS.inter newinit2 newinit3))) none_init 
                                in ((SS.union init_var (SS.union newinit (SS.union newinit2 (SS.union newinit3 newinit4)))),(SS.union none_init (SS.union newnone (SS.union newnone2 (SS.union newnone3 newnone4)))))
                                )
        | While (cnd,blk1) -> (let (newinit,newnone) = eval_cond_vars cnd init_var none_init 
                              in let (newinit2,newnone2) = aux blk1 init_var none_init 
                              in let (newinit3,newnone3) = aux blk init_var none_init
                              in (SS.union init_var (SS.union newinit (SS.union newinit2 newinit3)),SS.union none_init (SS.union newnone (SS.union newnone2 newnone3)))
      )))
  in aux blk init_var none_init 
;;

(** Affichage des variables et de celles non initialisées*)
let vars (blk:block) = 
  let (init,none) = eval_blk_vars blk 
  in print_string "Variables are :\n";
  SS.iter (fun x->print_string (Printf.sprintf "%s " x)) init;
  print_string "\nVariable non initialised are :\n";
  SS.iter (fun x->print_string (Printf.sprintf "%s " x)) none;
  print_string "\n"
;;