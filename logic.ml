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

let rec pow x n =
  if n = 0 then 1 else x * pow x (n-1);;

let repeat l =
  let m = pow 2 (List.length l) in
  let rec aux k a =
    match k with
      | 0 -> a
      | _ -> aux (k-1) (l :: a)
  in
    aux m [];;

repeat ["a"; "b"; "c"];;

let boolean_nest l =
  let rec nest_true l a =
    match l with
      | [] -> a
      | h :: t -> nest_true t ((h @ [true]) :: a)
  in
  let rec nest_false l a =
    match l with
      | [] -> a
      | h :: t -> nest_false t ((h @ [false]) :: a)
  in
  let nested_true = nest_true l [] in
  let nested_false = nest_false l [] in
    nested_true @ nested_false;;

let rec boolean_table n =
  match n with
    | 0 -> []
    | 1 -> [ [true]; [false] ]
    | k -> boolean_nest (boolean_table (k-1));;

boolean_table 3;;

let rec zip al bl =
  match al, bl with
    | [], bl -> []
    | al, [] -> []
    | a :: at, b :: bt -> (a, b) :: zip at bt;;

zip ["a"; "b"; "c"] [1;2;3];;

let rec merge al bl =
  match al, bl with
    | [], _ -> []
    | _, [] -> []
    | a :: at, b :: bt -> [zip a b] @ merge at bt;;

let evaln be_list expr =
  let rec bool_of_var h x =
    match h with
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
    eval expr;;

let a = Var "a" and b = Var "b" and c = Var "c" in
  evaln [("a", true); ("b", false); ("c", false)] (Or(And(a, Or(b,c)), Or(And(a,b), And(a,c))));;

let rec table l expr =
  match l with
    | [] -> []
    | h :: t -> (h, evaln h expr) :: table t expr;;

let l = merge (repeat ["a"; "b"; "c"]) (boolean_table 3) in
let a = Var "a" and b = Var "b" and c = Var "c" in
  table l (Or(And(a, Or(b,c)), Or(And(a,b), And(a,c))));;


let grey_code_nest l =
  let rec nest_one l a =
    match l with
      | [] -> a
      | h :: t -> nest_one t ((h ^ "1") :: a)
  in
  let rec nest_zero l a =
    match l with
      | [] -> a
      | h :: t -> nest_zero t ((h ^ "0") :: a)
  in
  let nested_one = nest_one l [] in
  let nested_zero = nest_zero l [] in
    nested_one @ nested_zero;;

let rec grey_code_table n =
  match n with
    | 0 -> []
    | 1 -> [ "0"; "1" ]
    | k -> grey_code_nest (grey_code_table (k-1));;

grey_code_table 1;;
grey_code_table 2;;
grey_code_table 3;;
grey_code_table 4;;


module PrioQueue =
struct
  type priority = T

  type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue

  let empty = Empty

  let rec insert queue prio elt =
    match queue with
      | Empty -> Node(prio, elt, Empty, Empty)
      | Node(p, e, left, right) ->
          if prio <= p
          then Node(prio, elt, insert right p e, left)
          else Node(p, e, insert right prio elt, left)
  exception Queue_is_empty

  let rec remove_top = function
    | Empty -> raise Queue_is_empty
    | Node(prio, elt, left, Empty) -> left
    | Node(prio, elt, Empty, right) -> right
    | Node(prio, elt, (Node(lprio, lelt, _, _) as left),
           (Node(rprio, relt, _, _) as right)) ->
        if lprio <= rprio
        then Node(lprio, lelt, remove_top left, right)
        else Node(rprio, relt, left, remove_top right)

  let extract = function
    | Empty -> raise Queue_is_empty
    | Node(prio, elt, _, _) as queue -> (prio, elt, remove_top queue)
end;;

let a =PrioQueue.insert PrioQueue.empty '-' 99;;


type 'a freq = Fr of 'a * int;;
type 'a huffman_encoding = Hs of 'a * string;;

let freqs = [Fr('a',45); Fr('b',13); Fr('c',12); Fr('d',16); Fr('e',9); Fr('f',5)];;

let get_prio q = match q with
  | (p, e, _) -> p;;

let get_queue q = match q with
  | (_, _, queue) -> queue;;

let huffman fs =
  let rec aux a f =
    match f with
      | [] -> a
      | Fr(c, f) :: t -> aux (PrioQueue.insert a f c) t
  in
  let orig_queue = aux PrioQueue.empty fs in
  let rec aux2 a q =
    match q with
      | PrioQueue.Node(prio, elt, PrioQueue.empty, PrioQueue.empty) ->
          PrioQueue.insert prio elt a
      | PrioQueue.Node(prio, elt, left, PrioQueue.empty) -> 
          let queue = get_queue (PrioQueue.extract left) in

      | PrioQueue.Node(prio, elt, PrioQueue.empty, right) -> 
      | PrioQueue.Node(prio, elt, (Node(lprio, lelt, _, _) as left), (Node(rprio, relt, _, _) as right)) ->
          let (freq, c) = if lprio < rprio then (lprio, lelt) else (rprio, relt) in
          let new_freq = freq + prio in
            PrioQueue.insert a 

let q = huffman freqs;;





prio (PrioQueue.extract q);;
let a = get_queue (PrioQueue.extract q);;
let b = get_queue (PrioQueue.extract a);;
let c = get_queue (PrioQueue.extract b);;
let d = get_queue (PrioQueue.extract c);;
let e = get_queue (PrioQueue.extract d);;
let f = get_queue (PrioQueue.extract e);;



















