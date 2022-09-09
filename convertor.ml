include Types
include Simpl
(** Fonctions de conversion*)

exception Indent_error of string;;
exception Comparator_error of string;;

(** Convertit sign en string *)
let sign_to_string (o:sign) : string = match o with
    | Pos -> "+"
    | Neg -> "-"
    | Zero -> "0"
    | Error -> "!"
;;
let print_signlist (o:sign list) : unit = List.iter (fun x->print_string (Printf.sprintf "%s " (sign_to_string x))) o;;


(** Convertit un opérateur arithmétique en string *)
let op_to_string (o:op) : string = match o with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
;;

(** Convertit une instruction en string *)
let instr_to_string (ins:instr) : string = match ins with
  | Set(a,b) -> ":="
  | Read(a) -> "READ"
  | Print(a) -> "PRINT"
  | If(a,b,c) -> "IF"
  | While(a,b) -> "WHILE"
;;

(** Convertit un opérateur de comparaison en string *)
let comp_to_string (c:comp) : string = match c with
    | Eq -> "="
    | Ne -> "<>"
    | Lt -> "<"
    | Le -> "<="
    | Gt -> ">"
    | Ge -> ">="
;;

(** Convertit une expression en string *)
let rec expr_to_string (expression:expr) : string = match expression with
    | Op(a,x,y) -> (op_to_string a)^" "^(expr_to_string x)^" "^(expr_to_string y)
    | Num(a) -> string_of_int a
    | Var(a) -> a
;;

(** Convertit une condition en string *)
let cond_to_string (condi:cond) : string = match condi with
    |(a,cmp,b) -> (expr_to_string a)^" "^(comp_to_string cmp)^" "^(expr_to_string b)
;;

(** Convertit un string en opérateur arithmétique *)
let string_to_op (str:string) : op = match str with
    | "+" -> Add
    | "-" -> Sub
    | "*" -> Mul
    | "/" -> Div
    | "%" -> Mod
    | str  -> failwith "Error"
;;

(** Convertit un string en opérateur de comparaison*)
let string_to_comp (str:string) : comp = match str with
    | "=" -> Eq
    | "<>" -> Ne
    | "<" -> Lt
    | "<=" -> Le
    | ">" -> Gt
    | ">=" -> Ge
    | str  -> failwith "Error"
;;

(** Count number of a char in a string *)
let count s c = 
  let n = ref 0 in 
  try
    for j = 0 to String.length s - 1 do
      if(s.[j] = c) then n := !n + 1
      else raise Exit;
      done; !n
  with Exit -> !n
;;

(** Check si s2 dans s1 *)
let contains s1 s2 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done;
    false
  with Exit -> true
;;

(** Check si un string est un int *)
let check_str s = 
  try int_of_string s |> ignore; true
  with Failure _ -> false
;;

(** Convertit string en liste *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) ((String.make 1 s.[i]) :: l) in
  exp (String.length s - 1) []
;;

(** Check si s est une operation *)
let is_op (s:string) : bool =  "+"=s || "-"=s || "*"=s || "/"=s || "%"=s;;

(** Convertit un string en expression arithmétique *)
let string_to_expression (str:string) (pos:int) (simpl:bool): expr = 
  let l =  List.filter (fun x -> x <> "") (String.split_on_char ' ' str) in 
  let rec aux l = match l with
    | [] -> failwith (Printf.sprintf "Expression not found in line %d" pos)
    | [x] -> if check_str x then (Num(int_of_string x),[]) else (Var(x),[])
    | a::l when (not (is_op a)) -> fst (aux [a]), l
    | x::a::l -> (if(is_op  x && not (is_op a)) then let (z,y) = aux l in (Op(string_to_op x, fst (aux [a]) , z),y)
                  else    let (z,y) = aux (a::l) in
                    let (x1,y1) = aux y in
                    Op(string_to_op x, z, x1),y1)
  
  in if(simpl) then simpl_expr (fst (aux l)) else fst (aux l)
;;


 (** Convertit un string en condition *)
let string_to_condition (str:string) (pos:int) (simpl:bool): cond =
  if(contains str "<>" ) then ( let l = String.split_on_char '<' str 
                                in let p = List.nth l 1
                                in let (a,b) = (List.nth l 0, List.nth (String.split_on_char '>' p) 1)  
                                in (string_to_expression a pos simpl,Ne,string_to_expression b pos simpl))
  else if(contains str "<="  ) then (let l = String.split_on_char '<' str 
                                     in let p = List.nth l 1
                                     in let (a,b) = (List.nth l 0, List.nth (String.split_on_char '=' p) 1) 
                                     in (string_to_expression a pos simpl,Le,string_to_expression b pos simpl))
  else if(contains str ">="  ) then (let l = String.split_on_char '>' str 
                                     in let p = List.nth l 1
                                     in let (a,b) = (List.nth l 0, List.nth (String.split_on_char '=' p) 1) 
                                     in (string_to_expression a pos simpl,Ge,string_to_expression b pos simpl))
  else if(String.contains str '<' ) then (let l = String.split_on_char '<' str in 
                                          let (a,b) = (List.nth l 0,List.nth l 1) 
                                          in (string_to_expression a pos simpl,Lt,string_to_expression b pos simpl))
  else if(String.contains str '>' ) then (let l = String.split_on_char '>' str in 
                                          let (a,b) = (List.nth l 0,List.nth l 1) 
                                          in (string_to_expression a pos simpl,Gt,string_to_expression b pos simpl))
  else if (String.contains str '=' ) then (let l = String.split_on_char '=' str in 
        let (a,b) = (List.nth l 0,List.nth l 1) 
        in (string_to_expression a pos simpl,Eq,string_to_expression b pos simpl))

  else failwith (Printf.sprintf "Problem of Comparator in line %d" pos)
;; 

(**Supprime le premier élément d'une liste *)
let cut = function
  | [] -> [] 
  | _::xs -> xs

(** Convertit un string en instruction *)
let string_to_instr (str:lines) (simpl:bool) : block =
  let rec aux str depth blocks = match str with
    | [] -> (blocks,str)
    | a::str -> ( if(count (snd a) ' ' != 2*depth) then (
                      let num_indent = count (snd a) ' '
                      in if(num_indent mod 2 = 1) then failwith (Printf.sprintf "Problem of indentation in line %d" (fst a))
                      else if(num_indent>2*depth) then failwith (Printf.sprintf "Problem of indentation in line %d" (fst a))
                      else (blocks,a::str)
                    )
                  else if (contains (snd a) "COMMENT") then  aux str depth blocks
                  else if(contains (snd a) "READ") then 
                    let (pos,s) = a in
                    let l = String.split_on_char ' ' (String.trim s) 
                    in aux str depth ((pos,Read(List.nth l 1))::blocks)
                  else if(contains (snd a) "PRINT") then 
                    let (pos,s) = a in
                    let len = String.length (String.trim s) in
                    let l = String.sub (String.trim s) 6 (len-6) in
                    aux str depth ((pos,Print(string_to_expression l pos simpl))::blocks)
                  else if(contains (snd a) ":=") then 
                    let (pos,s) = a in
                    let check = List.nth (String.split_on_char ' ' (String.trim s)) 1
                    in if(check <> ":=") then failwith (Printf.sprintf "Problem of Set in line %d" pos )
                    else(
                    let l = String.split_on_char '=' (String.trim s) in
                    let m = String.split_on_char ':' (List.nth l 0) in
                    aux str depth ((pos,Set(String.trim (List.nth m 0),string_to_expression (List.nth l 1) pos simpl))::blocks))
                  else if(contains (snd a) "IF") then 
                    let (pos,s) = a in
                    let len = String.length (String.trim s) in
                    let l = String.sub (String.trim s) 3 (len-3) in
                    let (b,newstr) =  aux str (depth+1) [] in
                    let (pos1,s1) = List.nth newstr 0 in 
                    if(contains s1 "ELSE") then  
                      let p = cut newstr in
                      let (b1,newstr1) = aux p (depth+1) []
                      in aux  newstr1 depth ((pos,If(string_to_condition l pos simpl,List.rev b, List.rev b1))::blocks)
                    else aux newstr depth ((pos,If(string_to_condition l pos simpl,List.rev b,[]))::blocks)
                  else if(contains (snd a) "WHILE") then
                    let (pos,s) = a in
                    let len = String.length (String.trim s) in
                    let l = String.sub (String.trim s) 6 (len-6) in
                    let (b,newstr) =  aux str (depth+1) [] in
                    aux newstr depth ((pos,While(string_to_condition l pos simpl,List.rev b))::blocks)
                  else failwith (Printf.sprintf "Instruction not found in line %d" (fst a))
                )
  in List.rev (fst (aux str 0 []))
;;
