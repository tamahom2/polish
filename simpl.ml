include Types

(** Fonctions pour l'option -simpl*)

(** Simplification d'addition *)
let simpl_add e1 e2 = match (e1,e2) with 
  | (Num(x),Num(y)) ->  Num(x+y) 
  | (Num(x),e2) when x=0 -> e2
  | (e1,Num(y)) when y=0 -> e1
  | (e1,e2) -> Op(Add,e1,e2)
;;

(** Simplification de soustraction *)
let simpl_sub e1 e2 = match (e1,e2) with 
  | (Num(x),Num(y)) ->  Num(x-y) 
  | (e1,Num(y)) when y=0 -> e1
  | (e1,e2) -> Op(Sub,e1,e2)
;;

(** Simplification de multiplication*)
let simpl_mult e1 e2 = match (e1,e2) with 
  | (Num(x),Num(y)) ->  Num(x*y) 
  | (e1,Num(y)) when y=1 -> e1
  | (Num(x),e2) when x=1 -> e2
  | (e1,Num(y)) when y=0 -> Num(0)
  | (Num(x),e2) when x=0 -> Num(0)
  | (e1,e2) -> Op(Mul,e1,e2)
;;

(** Simplification de division *)
let simpl_div e1 e2 = match (e1,e2) with 
  | (Num(x),Num(y)) ->  Num(x/y) 
  | (Num(x),e2) when x=0 -> Num(0)
  | (e1,Num(y)) when y=1 -> e1
  | (e1,e2) -> Op(Div,e1,e2)
;;

(** Simplification du modulo *)
let simpl_mod e1 e2 = match (e1,e2) with 
  | (Num(x),Num(y)) ->  Num(x mod y) 
  | (e1,Num(y)) when y=1 -> Num(0)
  | (Num(x),e2) when x=1 -> Num(1)
  | (Num(x),e2) when x=0 -> Num(0)
  | (e1,e2) -> Op(Mod,e1,e2)
;;

(** Simplification d'opération *)
let simpl_op op e1 e2 = match op with 
  | Add -> simpl_add e1 e2
  | Sub -> simpl_sub e1 e2
  | Mul -> simpl_mult e1 e2
  | Div -> simpl_div e1 e2
  | Mod -> simpl_mod e1 e2
;;

(** Simplification d'expression *)
let rec simpl_expr e = match e with 
  | Op(a,x,y) -> simpl_op a (simpl_expr x) (simpl_expr y)
  | e -> e
;;

(** Evaluation d'une expression *)
let rec eval_expr_simpl (expre:expr) : int = match expre with
  | Num(a) -> a
  | Op(a,x,y) -> (match a with 
      | Add -> eval_expr_simpl x + eval_expr_simpl y 
      | Sub -> eval_expr_simpl x - eval_expr_simpl y 
      | Mul -> eval_expr_simpl x * eval_expr_simpl y 
      | Div -> eval_expr_simpl x / eval_expr_simpl y
      | Mod -> eval_expr_simpl x mod eval_expr_simpl y )
  |Var(a) -> raise Not_found
;;

(** Evaluation en OCaml d'une condition en Polish *)
let eval_cond_simpl (condition:cond) : bool = match condition with
    | (expr1,comp,expr2)->(match comp with
        | Eq -> eval_expr_simpl expr1 = eval_expr_simpl expr2 
        | Ne -> eval_expr_simpl expr1 <> eval_expr_simpl expr2 
        | Lt -> eval_expr_simpl expr1 < eval_expr_simpl expr2 
        | Le -> eval_expr_simpl expr1 <= eval_expr_simpl expr2 
        | Gt -> eval_expr_simpl expr1 > eval_expr_simpl expr2 
        | Ge -> eval_expr_simpl expr1 >= eval_expr_simpl expr2 )
;; 

(** Reverses left and concatenates it with right*)
let rev_append left right = 
  let rec aux acc l1 l2 = match l1, l2 with 
    | [], [] -> List.rev(acc)
    | h :: t, [] -> aux (h :: acc) t []
    | [], h :: t -> aux (h :: acc) [] t
    | h :: t, h1 :: t1 -> aux (h :: acc) t (h1 :: t1)
  in aux [] (List.rev(left)) right
;;

(** Concatène deux listes*)
let append left right = rev_append (List.rev left) right ;;

(** Elimination des blocs "morts"*)
let eliminate_deadblock (blk:block) =  
  let rec aux blk newblk = match blk with
          | [] -> newblk
          | a::blk -> (match a with
              |(pos,inst) ->( match inst with
                      | If(cnd,blk1,blk2) -> (try (if(eval_cond_simpl cnd) then aux blk (append blk1 newblk) else aux blk (append blk2 newblk)) with Not_found -> aux blk (a::newblk))
                      | While(cnd,blk1) ->(try (if(eval_cond_simpl cnd) then aux blk (a::newblk) else aux blk newblk) with Not_found -> aux blk (a::newblk))
                      | _ -> aux blk (a::newblk)
          ))
  in List.rev (aux blk [])
;;