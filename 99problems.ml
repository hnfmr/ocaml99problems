let kth n l =
  let rec aux l a =
    if a = n then
      begin
        match l with
          | [] -> None
          | h :: t -> Some h
      end
    else
      match l with
        | [] -> None
        | _ :: t -> aux t (a + 1)
  in
    aux l 1;;

assert(kth 1 [1;2;3] = Some 1);;
assert(kth 3 [1;2;3] = Some 3);;

let size l =
  let rec aux l a =
    match l with
      | [] -> a
      | h :: t -> aux t (a + 1)
  in
    aux l 0;;

assert(size [1;2;3] = 3);;

let rev l =
  let rec aux l a =
    match l with
      | [] -> a
      | h :: t -> aux t (h :: a)
  in
    aux l [];;

assert(rev [1;2;3] = [3;2;1]);;

let is_palindrome l = l = List.rev l;;

assert(is_palindrome [1;2;3;2;1]);;
assert(not (is_palindrome [1;2;3;4;5]));;

type 'a node = One of 'a | Many of 'a node list;;

let flatten l =
  let rec aux2 nl a =
    match nl with
      | [] -> a
      | (One o) :: t -> aux2 t (o :: a)
      | (Many m) :: t -> aux2 t (aux2 m a)
  in
  let rec aux l a =
    match l with
      | [] -> a
      | h :: t -> aux t ((aux2 [h] []) @ a)
  in
  let result = aux l [] in
    List.rev result;;

flatten [One 1; One 2; Many [One 3; One 4; Many [One 5; One 6]]];;

