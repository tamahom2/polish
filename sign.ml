include Types
include Auxilary

(** Fonctions pour l'option -sign*)

(**Analyse statique du signe *)
let rec find e = function
    | [] -> false
    | h::t -> h = e || find e t
;;

(** append_err lists without duplicates*)
let rec help_append_err_list l1 l2 =
    match l1 with
    | [] -> l2
    | h::t -> if find h l2 then help_append_err_list t l2
              else help_append_err_list t (h::l2)
;;
            
let append_err_list x y = help_append_err_list x (help_append_err_list y []);;

(** Signes d'une addition*)
let sign_add x y = match (x,y) with
  |[Error],_ -> [Error]
  |_,[Error] -> [Error]
  |x,y when ((find Error x) || (find Error y)) -> (if(((find Pos x)||(find Pos y)) && ((find Neg x)||(find Neg y))) then append_err_list (append_err_list x [Zero]) y else append_err_list x y)
  |[Pos;Neg;Zero],_ -> [Pos;Neg;Zero]
  |_,[Pos;Neg;Zero] -> [Pos;Neg;Zero]
  |[Zero],y -> y
  |x, [Zero] -> x
  |[Pos;Zero],[Pos;Zero] -> [Pos;Zero]
  |[Pos;Zero],[Pos] -> [Pos]
  |[Pos;Zero],[Neg;Zero] -> [Pos;Neg;Zero]
  |[Pos;Zero],[Neg] -> [Pos;Neg;Zero]
  |[Neg;Zero],[Neg;Zero] -> [Neg;Zero]
  |[Neg;Zero],[Neg] -> [Neg;Zero]
  |[Neg;Zero],[Pos;Zero] -> [Pos;Neg;Zero]
  |[Neg;Zero],[Pos] -> [Pos;Neg;Zero]
  |[Neg],[Neg;Zero] -> [Neg;Zero]
  |[Neg],[Neg] -> [Neg]
  |[Neg],[Pos;Zero] -> [Pos;Neg;Zero]
  |[Neg],[Pos] -> [Pos;Neg;Zero]
  |[Pos],[Neg;Zero] -> [Pos;Neg;Zero]
  |[Pos],[Neg] -> [Pos;Neg;Zero]
  |[Pos],[Pos;Zero] -> [Pos;Zero]
  |[Pos],[Pos] -> [Pos]
  |[Pos;Neg],_ -> [Pos;Neg;Zero]
  |x,y -> x
;;

(** Signes d'une soustraction*)
let sign_sub x y = match (x,y) with
|[Error],_ -> [Error]
|_,[Error] -> [Error]
|x,y when ((find Error x) || (find Error y)) -> (if((find Pos x) && (find Pos y)) then [Pos;Neg;Zero;Error] 
                                                  else if((find Pos x)&& (find Neg y) && (find Zero x) && (find Zero y)) then [Pos;Zero;Error]
                                                  else if((find Pos x)&& (find Neg y)) then [Pos;Error]
                                                  else if((find Neg x)&& (find Pos y) && (find Zero x) && (find Zero y)) then [Neg;Zero;Error]
                                                  else if((find Neg x)&& (find Pos y)) then [Neg;Error]
                                                  else if((find Neg x)&& (find Neg y)) then [Pos;Neg;Zero;Error] 
                                                  else if((find Zero x) && (find Pos y)) then [Neg;Error]
                                                  else if((find Zero x) && (find Pos y) && (find Zero y)) then [Neg;Zero;Error]
                                                  else if ((find Zero x) && (find Neg y) && (find Zero y)) then [Pos;Zero;Error]
                                                  else if ((find Zero x) && (find Neg y)) then [Pos;Error]
                                                  else if ((find Zero x) && (find Zero y)) then [Zero;Error]
                                                  else append_err_list x y)
  |[Pos;Neg;Zero],_ -> [Pos;Neg;Zero]
  |_,[Pos;Neg;Zero] -> [Pos;Neg;Zero]
  |[Zero],[Zero] -> [Zero]
  |[Zero],[Pos] -> [Neg]
  |[Zero],[Neg] -> [Pos]
  |[Zero],[Pos;Zero] -> [Neg;Zero]
  |[Zero],[Neg;Zero] -> [Pos;Zero]
  |x, [Zero] -> x
  |[Pos;Zero],[Pos;Zero] -> [Pos;Neg;Zero]
  |[Pos;Zero],[Pos] -> [Pos;Neg;Zero]
  |[Pos;Zero],[Neg;Zero] -> [Pos;Zero]
  |[Pos;Zero],[Neg] -> [Pos]
  |[Neg;Zero],[Neg;Zero] -> [Pos;Neg;Zero]
  |[Neg;Zero],[Neg] -> [Pos;Neg;Zero]
  |[Neg;Zero],[Pos;Zero] -> [Neg;Zero]
  |[Neg;Zero],[Pos] -> [Neg;Zero]
  |[Neg],[Neg;Zero] -> [Pos;Neg;Zero]
  |[Neg],[Neg] -> [Pos;Neg;Zero]
  |[Neg],[Pos;Zero] -> [Neg;Zero]
  |[Neg],[Pos] -> [Neg;Zero]
  |[Pos],[Neg;Zero] -> [Pos;Zero]
  |[Pos],[Neg] -> [Pos]
  |[Pos],[Pos;Zero] -> [Pos;Neg;Zero]
  |[Pos],[Pos] -> [Pos;Neg;Zero]
  |[Pos;Neg],_ -> [Pos;Neg;Zero]  
  |x,y -> x
;;

(** Signes d'une multiplication*)
let sign_mul x y = match (x,y) with
|[Error],_ -> [Error]
  |_,[Error] -> [Error]
  |x,y when ((find Error x) || (find Error y)) -> (if(x=[Zero;Error] || y=[Zero;Error]) then [Zero;Error]
                                                  else if(x=[Pos;Neg;Zero;Error] || y=[Pos;Neg;Zero;Error]) then [Pos;Neg;Zero;Error]
                                                  else if((find Pos x)&& (find Neg y) && ((find Zero x) || (find Zero y))) then [Neg;Zero;Error]
                                                  else if((find Neg x)&& (find Pos y) && ((find Zero x) || (find Zero y)) ) then [Neg;Zero;Error]
                                                  else if((find Pos x) && (find Pos y) && ((find Zero x) || (find Zero y))) then [Pos;Zero;Error]
                                                  else if((find Neg x) && (find Neg y) && ((find Zero x) || (find Zero y))) then [Pos;Zero;Error]
                                                  else if (((find Pos x)&& (find Neg y)) || ((find Neg x)&& (find Pos y) ) ) then [Neg;Error] 
                                                  else [Pos;Error] )
  |[Zero],_ -> [Zero]
  |_, [Zero] -> [Zero]
  |[Pos;Neg;Zero],_ -> [Pos;Neg;Zero]
  |_,[Pos;Neg;Zero] -> [Pos;Neg;Zero]
  |[Pos;Zero],[Pos;Zero] -> [Pos;Zero]
  |[Pos;Zero],[Pos] -> [Pos;Zero]
  |[Pos;Zero],[Neg;Zero] -> [Neg;Zero]
  |[Pos;Zero],[Neg] -> [Neg;Zero]
  |[Neg;Zero],[Neg;Zero] -> [Pos;Zero]
  |[Neg;Zero],[Neg] -> [Pos;Zero]
  |[Neg;Zero],[Pos;Zero] -> [Neg;Zero]
  |[Neg;Zero],[Pos] -> [Neg;Zero]
  |[Neg],[Neg;Zero] -> [Pos;Zero]
  |[Neg],[Neg] -> [Pos]
  |[Neg],[Pos;Zero] -> [Neg;Zero]
  |[Neg],[Pos] -> [Neg]
  |[Pos],[Neg;Zero] -> [Neg;Zero]
  |[Pos],[Neg] -> [Neg]
  |[Pos],[Pos;Zero] -> [Pos;Zero]
  |[Pos],[Pos] -> [Pos]
  |[Pos;Neg],y when (find Zero y) -> [Pos;Neg;Zero]
  |[Pos;Neg],y-> [Pos;Neg]
  |x,y -> x
;;

(** Signes d'une division*)
let sign_div x y (posi:int) = match (x,y) with
  |[Error],_ -> [Error]
  |_,[Error] -> [Error]
  |x,y when ((find Error x) || (find Error y)) -> (if(y=[Zero;Error]) then [Error]
                                                  else if(x=[Pos;Neg;Zero;Error] || y=[Pos;Neg;Zero;Error]) then [Pos;Neg;Zero;Error]
                                                  else if((find Pos x)&& (find Neg y) && ((find Zero x) || (find Zero y))) then [Neg;Zero;Error]
                                                  else if((find Neg x)&& (find Pos y) && ((find Zero x) || (find Zero y)) ) then [Neg;Zero;Error]
                                                  else if((find Pos x) && (find Pos y) && ((find Zero x) || (find Zero y))) then [Pos;Zero;Error]
                                                  else if((find Neg x) && (find Neg y) && ((find Zero x) || (find Zero y))) then [Pos;Zero;Error]
                                                  else if (((find Pos x)&& (find Neg y)) || ((find Neg x)&& (find Pos y) ) ) then [Neg;Error] 
                                                  else [Pos;Error] )
  |x,[Pos;Neg;Zero] -> ( let ate = append_err posi in if(x=[Zero]) then [Zero;Error]
                                                  else [Pos;Neg;Zero;Error])
  |x, [Zero] -> let ate = append_err posi in [Error]
  |x,[Pos;Zero]-> (let ate = append_err posi in if(x=[Zero]) then [Zero;Error]
                                            else if(x=[Pos]) then [Pos;Error]
                                            else if(x=[Neg]) then [Neg;Error]
                                            else if(x=[Pos;Zero]) then [Pos;Zero;Error]
                                            else if(x=[Neg;Zero]) then [Neg;Zero;Error] 
                                            else [Pos;Neg;Zero;Error])
  |x,[Neg;Zero] ->(let ate = append_err posi in if(x=[Zero]) then [Zero;Error]
                                            else if(x=[Pos]) then [Neg;Error]
                                            else if(x=[Neg]) then [Pos;Error]
                                            else if(x=[Pos;Zero]) then [Neg;Zero;Error]
                                            else if(x=[Neg;Zero]) then [Pos;Zero;Error] 
                                            else [Pos;Neg;Zero;Error])
  |[Zero],_ -> [Zero]
  |[Pos;Neg;Zero],_ -> [Pos;Neg;Zero]
  |[Pos;Zero],[Pos] -> [Pos;Zero]
  |[Pos;Zero],[Neg] -> [Neg;Zero]
  |[Neg;Zero],[Neg] -> [Pos;Zero]
  |[Neg;Zero],[Pos] -> [Neg;Zero]
  |[Neg],[Neg] -> [Pos]
  |[Neg],[Pos] -> [Neg]
  |[Pos],[Neg] -> [Neg]
  |[Pos],[Pos] -> [Pos]
  |[Pos;Neg],_ -> [Pos;Neg]
  |x,y -> x
;;

(** Signes d'un modulo*)
let sign_mod x y (posi:int) = match (x,y) with
  |[Error],_ -> [Error]
  |_,[Error] -> [Error]
  |x,y when ((find Error x) || (find Error y)) ->  (if(y=[Zero;Error]) then [Error]
                                                  else if(x=[Pos;Neg;Zero;Error] || x=[Pos;Neg;Error]) then [Pos;Neg;Zero;Error]
                                                  else if((find Neg x)) then [Neg;Zero;Error]
                                                  else [Pos;Zero;Error] )
  |x,[Pos;Neg;Zero] ->  ( let ate = append_err posi in if(x=[Neg;Zero] || x=[Neg]) then [Neg;Zero;Error]
                                                 else if(x=[Pos;Zero] || x = [Pos]) then [Pos;Neg;Error]
                                                 else [Pos;Neg;Zero;Error])
  |x, [Zero] ->  let ate = append_err posi in [Error]
  |x,[Pos;Zero]->  ( let ate = append_err posi in if(x=[Neg;Zero] || x=[Neg]) then [Neg;Zero;Error]
                                                 else if(x=[Pos;Zero] || x = [Pos]) then [Pos;Neg;Error]
                                                 else [Pos;Neg;Zero;Error])
  |x,[Neg;Zero] ->  ( let ate = append_err posi in if(x=[Neg;Zero] || x=[Neg]) then [Neg;Zero;Error]
                                                 else if(x=[Pos;Zero] || x = [Pos]) then [Pos;Neg;Error]
                                                 else [Pos;Neg;Zero;Error])
  |[Zero],_ -> [Zero]
  |[Pos;Neg;Zero],_ -> [Pos;Neg;Zero]
  |[Pos;Zero],[Pos] -> [Pos;Zero]
  |[Pos;Zero],[Neg] -> [Pos;Zero]
  |[Neg;Zero],[Neg] -> [Neg;Zero]
  |[Neg;Zero],[Pos] -> [Neg;Zero]
  |[Neg],[Neg] -> [Neg;Zero]
  |[Neg],[Pos] -> [Neg;Zero]
  |[Pos],[Neg] -> [Pos;Zero]
  |[Pos],[Pos] -> [Pos;Zero]
  |[Pos;Neg],_ -> [Pos;Neg;Zero]
  |x,y -> x
;;

(** Signes =*)
let sign_equal x y : (sign list) = match (x,y) with
  |x,y when ((find Error x) || (find Error y)) -> [Error]
  |[Zero],[Zero] -> [Pos]
  |[Neg],[Zero] -> [Zero]
  |[Pos],[Zero] -> [Zero]
  |[Neg],[Pos]->[Zero]
  |[Pos],[Neg]->[Zero]
  |[Zero],[Neg] -> [Zero]
  |[Zero],[Pos] -> [Zero]
  |[Zero],[Pos;Neg]->[Zero]
  |[Pos;Neg],[Zero]->[Zero]
  |_,_ -> [Error]
;;

(** Signes >*)
let sign_greater x y : (sign list) = match (x,y) with
  |x,y when ((find Error x) || (find Error y)) -> [Error]
  |[Zero],[Zero] -> [Zero]
  |[Neg],[Zero] -> [Zero]
  |[Pos],[Zero] -> [Pos]
  |[Neg],[Pos]->[Zero]
  |[Pos],[Neg]->[Pos]
  |[Zero],[Neg] -> [Pos]
  |[Zero],[Pos] -> [Zero]
  |[Pos;Zero],[Neg]->[Pos]
  |_,_ -> [Error]
;;

(** Signes >=*)
let sign_greater_or_equal x y : (sign list) = match (x,y) with
  |x,y when ((find Error x) || (find Error y)) -> [Error]
  |[Zero],[Zero] -> [Pos]
  |[Neg],[Zero] -> [Zero]
  |[Pos],[Zero] -> [Pos]
  |[Neg],[Pos]->[Zero]
  |[Pos],[Neg]->[Pos]
  |[Zero],[Neg] -> [Pos]
  |[Zero],[Pos] -> [Zero]
  |[Pos;Zero],[Neg]->[Pos]
  |[Zero],[Neg;Zero]->[Pos]
  |[Pos;Zero],[Zero] -> [Pos]
  |_,_ -> [Error]
;;



(**Merge two lists *)
let merge x y=
  let module StrMap = Map.Make(String) in
  let map1 = List.to_seq x |> StrMap.of_seq in
  let map2 = List.to_seq y |> StrMap.of_seq in
  StrMap.bindings  (StrMap.merge (fun key x y ->
      match x, y with
      | Some a, None -> Some a
      | None, Some b -> Some b
      | Some a, Some b -> Some (append_err_list a b)
      | None, None -> None ) map1 map2) 
;;

(** Getting the new value instead *)
let merge_new x y=
  let module StrMap = Map.Make(String) in
  let map1 = List.to_seq x |> StrMap.of_seq in
  let map2 = List.to_seq y |> StrMap.of_seq in
  StrMap.bindings  (StrMap.merge (fun key x y ->
      match x, y with
      | Some a, None -> Some a
      | None, Some b -> Some b
      | Some a, Some b when (find Error a)-> Some (append_err_list [Error] b)
      | Some a, Some b -> Some b
      | None, None -> None ) map1 map2) 
;;

(**Reverse condition *)
let reverse cond = match cond with
    | (expr1,comp,expr2)->(match comp with
        | Eq -> (expr1,Ne,expr2)
        | Ne -> (expr1,Eq,expr2)
        | Lt -> (expr1,Ge,expr2)
        | Le -> (expr1,Gt,expr2) 
        | Gt -> (expr1,Le,expr2)
        | Ge -> (expr1,Lt,expr2) )
;;

let get_condi_equal_sign x y = match (x,y) with
  |[Pos;Neg;Zero;Error],y -> (y,y)
  |[Pos;Neg;Zero],y -> (y,y)
  |[Pos;Zero],[Zero] ->([Zero],[Zero])
  |[Pos;Neg],y when (find Zero y)=false -> (y,y)
  |(x,y) -> (x,y);
;;

let get_condi_notequal_sign x y = match (x,y) with
  |[Pos;Neg;Zero;Error],y when y=[Zero] -> ([Pos;Neg],[Pos;Neg])
  |[Pos;Neg;Zero],y when y=[Zero] -> ([Pos;Neg],[Pos;Neg])
  |[Pos;Zero],[Zero] ->([Pos],[Zero])
  |[Pos;Neg;Zero;Error],y -> (y,y)
  |[Pos;Neg;Zero],y -> (y,y)
  |(x,y) -> (x,y);
;;

(**TODO:Complete it and the Greater or equal *)
let get_condi_greater_sign x y = match (x,y) with
  |[Pos;Neg;Zero;Error],[Pos] -> ([Pos;Error],[Pos])
  |[Pos;Neg;Zero;Error],[Zero] -> ([Pos;Error],[Zero])
  |[Pos;Neg;Zero;Error],[Neg] -> ([Pos;Zero;Neg;Error],[Neg])
  |[Pos;Neg;Zero;Error],[Pos;Zero] -> ([Pos;Error],[Pos;Zero])
  |[Pos;Neg;Zero;Error],[Neg;Zero] -> ([Pos;Zero;Neg;Error],[Neg;Zero])
  |[Pos;Neg;Zero;Error], y  -> ([Pos;Neg;Zero;Error],y)
  |[Pos;Neg;Zero],[Zero] -> ([Pos],[Zero])
  |[Pos;Neg;Zero],[Pos;Zero] -> ([Pos],[Pos;Zero])
  |[Pos;Neg;Zero],[Pos] -> ([Pos],[Pos])
  |[Pos;Neg;Zero],[Pos;Neg] -> ([Pos;Neg;Zero],[Pos;Neg])
  |[Zero],_ ->([Zero],[Neg])
  |[Pos],y ->([Pos],y)
  |(x,y) -> (x,y);
;;

let get_condi_greater_equal_sign x y = match (x,y) with
  |[Pos;Neg;Zero;Error],[Pos] -> ([Pos;Error],[Pos])
  |[Pos;Neg;Zero;Error],[Zero] -> ([Pos;Zero;Error],[Zero])
  |[Pos;Neg;Zero;Error],[Neg] -> ([Pos;Zero;Neg;Error],[Neg])
  |[Pos;Neg;Zero;Error],[Pos;Zero] -> ([Pos;Error],[Pos;Zero])
  |[Pos;Neg;Zero;Error],[Neg;Zero] -> ([Pos;Zero;Neg;Error],[Neg;Zero])
  |[Pos;Neg;Zero;Error], y  -> ([Pos;Neg;Zero;Error],y)
  |[Pos;Neg;Zero],[Zero] -> ([Pos;Zero],[Zero])
  |[Pos;Neg;Zero],[Pos] -> ([Pos],[Pos])
  |[Pos;Neg;Zero],[Pos;Zero] -> ([Pos;Zero],[Pos;Zero])
  |[Pos;Neg;Zero],[Pos;Neg] -> ([Pos;Neg;Zero],[Pos;Neg])
  |[Pos],y ->([Pos],y)
  |(x,y) -> (x,y);
;;

(** Evaluation du signe d'une expression*)
let rec eval_expr_sign (expre:expr) (env:(name * sign list) list) (pos:int): sign list = match expre with
  | Num(a) -> if(a>0) then [Pos] else if (a<0) then [Neg] else [Zero]
  | Op(a,x,y) ->  (match a with 
      | Add -> sign_add (eval_expr_sign x env pos)  (eval_expr_sign y env pos)
      | Sub -> sign_sub (eval_expr_sign x env pos)  (eval_expr_sign y env pos)
      | Mul -> sign_mul (eval_expr_sign x env pos)  (eval_expr_sign y env pos)
      | Div -> sign_div (eval_expr_sign x env pos)  (eval_expr_sign y env pos) pos
      | Mod -> sign_mod (eval_expr_sign x env pos)  (eval_expr_sign y env pos) pos )
  | Var(a) -> List.assoc a env
;;

let eval_expr_sign2 (expr1:expr) (expr2:expr) (env:(name * sign list) list) comp = match expr1,expr2 with
  | Var(a),Var(b) -> (match comp with
        | Eq -> let (x,y) = (get_condi_equal_sign (eval_expr_sign (Var(a)) env 0) (eval_expr_sign (Var(b)) env 0)) in [(a,x);(b,y)]
        | Ne -> let (x,y) = (get_condi_notequal_sign (eval_expr_sign (Var(a)) env 0) (eval_expr_sign (Var(b)) env 0)) in [(a,x);(b,y)]
        | Lt -> let (x,y) = (get_condi_greater_sign (eval_expr_sign (Var(b)) env 0) (eval_expr_sign (Var(a)) env 0)) in [(a,y);(b,x)]
        | Le -> let (x,y) = (get_condi_greater_equal_sign (eval_expr_sign (Var(b)) env 0) (eval_expr_sign (Var(a)) env 0)) in [(a,y);(b,x)]
        | Gt -> let (x,y) = (get_condi_greater_sign (eval_expr_sign (Var(a)) env 0) (eval_expr_sign (Var(b)) env 0)) in [(a,x);(b,y)]
        | Ge -> let (x,y) = (get_condi_greater_equal_sign (eval_expr_sign (Var(a)) env 0) (eval_expr_sign (Var(b)) env 0) ) in [(a,x);(b,y)])
  | x,Var(b) -> (match comp with
        | Eq -> let (x,y) = (get_condi_equal_sign (eval_expr_sign (x) env 0) (eval_expr_sign (Var(b)) env 0)) in [(b,y)]
        | Ne -> let (x,y) = (get_condi_notequal_sign (eval_expr_sign (x) env 0) (eval_expr_sign (Var(b)) env 0)) in [(b,y)]
        | Lt -> let (x,y) = (get_condi_greater_sign (eval_expr_sign (Var(b)) env 0) (eval_expr_sign (x) env 0)) in [(b,x)]
        | Le -> let (x,y) = (get_condi_greater_equal_sign (eval_expr_sign (Var(b)) env 0) (eval_expr_sign (x) env 0) ) in [(b,x)]
        | Gt -> let (x,y) = (get_condi_greater_sign (eval_expr_sign (x) env 0) (eval_expr_sign (Var(b)) env 0)) in [(b,y)]
        | Ge -> let (x,y) = (get_condi_greater_equal_sign (eval_expr_sign (x) env 0) (eval_expr_sign (Var(b)) env 0)) in [(b,y)])
  | Var(b),y -> (match comp with
        | Eq -> let (x,y) = (get_condi_equal_sign (eval_expr_sign (Var(b)) env 0) (eval_expr_sign (y) env 0)) in [(b,x)]
        | Ne -> let (x,y) = (get_condi_notequal_sign (eval_expr_sign (Var(b)) env 0) (eval_expr_sign (y) env 0) ) in [(b,x)]
        | Lt -> let (x,y) = (get_condi_greater_sign (eval_expr_sign (y) env 0) (eval_expr_sign (Var(b)) env 0) ) in [(b,y)]
        | Le -> let (x,y) = (get_condi_greater_equal_sign (eval_expr_sign (y) env 0) (eval_expr_sign (Var(b)) env 0)) in [(b,y)]
        | Gt -> let (x,y) = (get_condi_greater_sign (eval_expr_sign (Var(b)) env 0) (eval_expr_sign (y) env 0) ) in [(b,x)]
        | Ge -> let (x,y) = (get_condi_greater_equal_sign (eval_expr_sign (Var(b)) env 0) (eval_expr_sign (y) env 0) ) in [(b,x)])
  | _,_ -> []
;;

let get_condi_sign condition env = match condition with
    | (expr1,comp,expr2)->(eval_expr_sign2 expr1 expr2 env comp)
;;

let isNum (e:expr) = try let b = eval_expr e in (b,true) 
                            with 
                            | Not_found -> (0,false)
                            | Division_by_zero -> (-1,false)
                            ;;

let eval_cond_sign condition env pos : (sign list)= match condition with
    | (Num(a),comp,Num(b)) -> (match comp with
        | Eq -> if(a=b) then [Pos] else [Zero]
        | Ne -> if(a<>b) then [Pos] else [Zero]
        | Lt -> if(a<b) then [Pos] else [Zero]
        | Le -> if(a<=b) then [Pos] else [Zero]
        | Gt -> if(a<=b) then [Zero] else [Pos] 
        | Ge -> if(a<b) then [Zero] else [Pos]
        )
    | (expr1,comp,expr2) when (snd (isNum expr1) && snd (isNum expr2)) ->(let a,b = (fst (isNum expr1), fst (isNum expr2)) in match comp with
        | Eq -> if(a=b) then [Pos] else [Zero]
        | Ne -> if(a<>b) then [Pos] else [Zero]
        | Lt -> if(a<b) then [Pos] else [Zero]
        | Le -> if(a<=b) then [Pos] else [Zero]
        | Gt -> if(a<=b) then [Zero] else [Pos] 
        | Ge -> if(a<b) then [Zero] else [Pos]
        )  
    | (expr1,comp,expr2)->(match comp with
        | Eq -> sign_equal (eval_expr_sign expr1 env pos) (eval_expr_sign expr2 env pos)
        | Ne -> let a =(sign_equal (eval_expr_sign expr1 env pos) (eval_expr_sign expr2 env pos)) in  if(a=[Pos]) then [Zero] else if(a=[Zero]) then [Pos] else a
        | Lt -> sign_greater (eval_expr_sign expr2 env pos) (eval_expr_sign expr1 env pos)
        | Le -> sign_greater_or_equal (eval_expr_sign expr2 env pos) (eval_expr_sign expr1 env pos)
        | Gt -> sign_greater (eval_expr_sign expr1 env pos) (eval_expr_sign expr2 env pos)
        | Ge -> sign_greater_or_equal (eval_expr_sign expr1 env pos) (eval_expr_sign expr2 env pos)
        )
;;

let sign_read (a:name) : (sign list) = [Pos;Neg;Zero];;
let sign_error (a:name) : (sign list) = [Error];;


let getsign (e:sign list) : int = match e with
  |[Pos] -> 1
  |[Zero] -> 0
  |_ -> -1
;;
let getexpr (a:expr) : string = match a with
  |Var(b) -> b
  | a -> ""
;;
let rec contains_all (list1:sign list) (list2:sign list) = match list1 with
    |[] -> true
    |a::list1 -> (List.mem a list2) && (contains_all list1 list2)
;;

let rec contain (a:string) (b:sign list) (list:(string * (sign list)) list) = match list with
    |[] -> false
    |(x,y)::list -> ((x=a) && (contains_all b y)) || contain a b list
;;  

let rec contains_all2 (list1:(string * (sign list)) list) (list2:(string * (sign list)) list)= match list1 with
    |[] -> true
    |(a,b)::list1 -> (contain a b list2) && (contains_all2 list1 list2)
;;

let equivalence (list1:(string * (sign list)) list) (list2:(string * (sign list)) list) = 
              (contains_all2 list1 list2) && (contains_all2 list2 list1)
;;
let exists expre env= match expre with
        |Var(c) -> (try (let b =List.assoc c env in true) with Not_found -> false)
        |_ -> true
        ;;

(** Evaluation des signes d'un block*)
let eval_blk_sign (blk:block) = 
  let env = []
  in let rec aux blk (env:(name * sign list) list) = match blk with
  | [] -> env
  | a::blk -> (match a with
      |(pos,inst) ->( match inst with
        | Set(a,b) -> let si = eval_expr_sign b env pos in  aux blk (merge_new env [(a,si)])
        | Read(a) ->  aux blk ((a,sign_read a)::env)
        | Print(a) -> if (exists a env) then aux blk env else let ate = append_err2 pos in  aux blk ((getexpr a,sign_error "")::env)
        | If(cnd,blk1,blk2) -> (let e = eval_cond_sign cnd env pos in if((getsign e) = 1) then let a = aux blk1 env in aux blk (merge a env) 
                                else if((getsign e) = 0) then let a = aux blk2 env in aux blk (merge a env) 
                                else let newenv = (get_condi_sign cnd env) in  
                                let a = aux blk1 (merge_new env newenv) in 
                                let b = aux blk2 (merge_new env (get_condi_sign (reverse cnd) env)) 
                                in aux blk (merge env (merge a b))
        )
        | While (cnd,blk1) -> (let e = eval_cond_sign cnd env pos in if((getsign e) = 0) then aux blk env   
                                else let revercond = get_condi_sign (reverse cnd) env in
                                let newenv = merge_new env (get_condi_sign cnd env) in
                                let e1 = ref (aux blk1 newenv) in 
                                let e0 = ref env in
                                while((equivalence (!e1) (!e0))=false) do 
                                      e0 := !e1;
                                      e1 := aux blk1 !e1
                                done; aux blk (merge_new (merge env !e1) revercond)
                                ))
      )
  in aux blk [] 
;;

(** Affichage des signes d'un block*)
let sign (blk:block) = 
    let env = eval_blk_sign blk in
    List.iter (fun (x,y) -> print_string (Printf.sprintf "%s " x);print_signlist y;print_string "\n") env;
    if(List.length (!errors) = 0) then print_string "Safe\n"
    else List.iter (fun x -> print_string (Printf.sprintf "%s\n" x)) !errors
;;
