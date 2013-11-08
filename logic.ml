type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;

let rec eval2 a av b bv expr =
  match expr with
    | Var x -> if x = a then av else
        if x = b then bv else failwith "Invalid argument"
    | Not e -> not (eval2 a av b bv e)
    | And (e1, e2) -> eval2 a av b bv e1 && eval2 a av b bv e2
    | Or (e1, e2) -> eval2 a av b bv e1 || eval2 a av b bv e2;;

let e = And ( Or(Var "a", Var "b"), And(Var "a", Var "b"));;
eval2 "a" true "b" false e;;

let table2 a b expr =
  [(true, true, eval2 a true b true expr);
   (true, false, eval2 a true b false expr);
   (false, false, eval2 a false b false expr);
   (false, true, eval2 a false b true expr)];;

table2 "a" "b" e;;

let evaln be_list expr =
  let rec gen_list a =
    match be_list with
      | [] -> a
      | 
let rec bool_of_var l x =
  match l with
    | [] -> failwith "not found"
    | (a, b) :: t -> if a = x then b else bool_of_var t x
in
let rec eval expr =
  match expr with
    | Var x -> let b_of_x = bool_of_var be_list x in b_of_x
    | Not e -> not (eval e)
    | And (e1, e2) -> eval e1 && eval e2
    | Or (e1, e2) -> eval e1 || eval e2
in
  aux expr;;

let table3 be_list expr =


  let a = Var "a" and b = Var "b" and c = Var "c" in
    evaln [("a", true); ("b", false); ("c", false)] (Or(And(a, Or(b,c)), Or(And(a,b), And(a,c))));
