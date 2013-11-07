let is_prime n =
  match n with
    | 0 -> false
    | 1 -> false
    | 2 -> true
    | _ -> 
        let rec aux k =
          if n = k then true else if n mod k = 0 then false else aux (k+1)
        in
          aux 2;;

let gcd a b =
  let rec aux a b =
    if b mod a = 0 then a
    else aux (b mod a)  a
  in
    if a < b then aux a b else aux b a;;

let coprime a b = gcd a b = 1;;

coprime 13 27;;

not (coprime 20536 7826);;

(* euler's totient function *)
let phi m =
  let rec aux c a =
    if c = m then a else if (coprime c m) then aux (c+1) (c :: a) else
      aux (c+1) a
  in
  let d = aux 1 [] in
    List.length d;;

phi 10;;
phi 13;;

let factors m =
  let rec aux c a d =
    if c = m then a else
    if d mod c = 0 then aux 2 (c :: a) (d / c) else
      aux (c+1) a d
  in
  let result = aux 2 [] m in
    List.rev result;;

factors 315;;

let factors2 n =
  let rec aux d n =
    if n = 1 then [] else
    if n mod d = 0 then
      match aux d (n / d) with
        | (h, n) :: t when h = d -> (h, n + 1) :: t
        | l -> (d, 1) :: l
    else
      aux (d+1) n
  in
    aux 2 n;;

factors2 315;;

let rec power x n =
  match n with
    | 0 -> 1
    | 1 -> x
    | _ -> if n mod 2 = 0 then (power x (n/2)) * (power x (n/2))
        else x * (power x (n-1));;

power 10 3;;
power 10 4;;

let phi_improved m =
  let factors = factors2 m in
    List.fold_left (fun a (h, n) -> a * (h - 1) * (power h (n-1))) 1 factors;;

factors2 10;;
phi_improved 10;;
phi_improved 13;;

#load "unix.cma";;
open Unix;;

let timeit f a =
  let t0 = Unix.gettimeofday() in
    ignore(f a);
    let t1 = Unix.gettimeofday() in
      t1 -. t0;;

timeit phi 10090;;
timeit phi_improved 10090;;

#use "basic.ml";;

range 2 10;;
let all_primes a b =
  let l = range a b in
  let rec aux l a =
    match l with
      | [] -> a
      | h :: t -> if is_prime h then aux t (h :: a) else aux t a
  in
    aux l [];;

List.length (all_primes 2 7920);;

(* extremely inefficient algorithm for goldbach, gasp*)
let goldbach n =
  let primes = all_primes 2 n in
  let rec aux l a =
    match l with
      | [] -> a
      | h :: t -> let appended = List.map (fun x -> (h, x)) primes in
            aux t (appended @ a)
  in
  let l = aux primes [] in
  let rec find l acc =
    match l with
      | [] -> acc
      | (a, b) :: t -> if a + b = n then find [] ((a, b) :: acc) else find t acc
  in
    List.hd (find l []);;


factors 28;;
goldbach 10;;
goldbach 28;;

let goldbach2 n =
  let rec aux d =
    if is_prime d && is_prime (n-d) then (d, n-d)
    else aux (d+1) in
    aux 2;;

goldbach2 28;;

let goldbach_list a b =
  let ints = range a b in
  let even_ints = List.filter (fun x -> x mod 2 = 0) ints in
    List.map (fun x -> (x, goldbach2 x)) even_ints;;

goldbach_list 9 20;;

let goldbach_limit a b k =
  let l = goldbach_list a b in
    List.filter (fun (u, (v, w)) -> v > k && w > k) l;;

goldbach_limit 4 2000 50;;
