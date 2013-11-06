let is_prime n =
  match n with
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

let factors m =
  let rec aux c a d =
    if c = m then a else
    if d mod c = 0 then aux 2 (c :: a) (d / c) else
      aux (c+1) a d
  in
  let result = aux 2 [] m in
    List.rev result;;

factors 315;;


