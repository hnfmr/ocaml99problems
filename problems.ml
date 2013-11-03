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
  let rec aux a l =
    match l with
      | [] -> a
      | One o :: t -> aux (o :: a) t
      | Many m :: t -> aux (aux a m) t
  in
  let result = aux [] l in
    List.rev result;;

assert(flatten [One 1; One 2; Many [One 3; One 4; Many [One 5; One 6]]] = [1;2;3;4;5;6]);;

let compress l =
  let rec aux acc l =
    match l with
      | [] -> acc
      | h :: t -> match t with
        | [] -> h :: acc
        | th :: tt -> if h = th then aux acc t  else aux (h :: acc) t
  in
  let result = aux [] l in
    List.rev result;;

let rec compress2 l =
  match l with
    | a :: (b :: _ as t) -> if a = b then compress2 t else a :: compress2 t
    | smaller -> smaller;;

let compress3 l =
  let rec aux l acc =
    match l with
      | a :: (b :: _ as t) -> if a = b then aux t acc else aux t (a :: acc)
      | smaller -> smaller @ acc
  in
  let result = aux l [] in
    List.rev result;;

assert(compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
       ["a"; "b"; "c"; "a"; "d"; "e"]);;
assert(compress2 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
       ["a"; "b"; "c"; "a"; "d"; "e"]);;
assert(compress3 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
       ["a"; "b"; "c"; "a"; "d"; "e"]);;

let pack l =
  let rec aux l sacc bacc =
    match l with
      | a :: (b :: _ as t) -> if a = b then aux t (a :: sacc) bacc else aux t [] ((a :: sacc) :: bacc)
      | smaller -> match smaller with
        | [] -> bacc
        | _ -> (smaller @ sacc) :: bacc
  in
  let result = aux l [] [] in
    List.rev result;;

assert(pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] =
       [["a";"a";"a";"a"]; ["b"]; ["c";"c"]; ["a";"a"]; ["d";"d"]; ["e";"e";"e";"e"]]);;
assert(pack [] = []);;
assert(pack ["a"] = [["a"]]);;

let encode l =
  let packed = pack l in
  let rec aux acc l=
    match l with
      | [] -> acc
      | h :: t -> aux ( (List.length h, List.hd h) :: acc) t
  in
  let result = aux [] packed in
    List.rev result;;


assert(encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
       [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]);;
assert(encode ["a"] = [(1, "a")]);;
assert(encode [] = []);;

type 'a rle =
    | One of 'a
    | Many of (int * 'a);;

let encode2 l =
  let encoded = encode l in
  let rec aux acc l =
    match l with
      | [] -> acc
      | (a, b) :: t -> if a = 1 then aux ((One b) :: acc) t else aux (Many (a, b) :: acc) t
  in
  let result = aux [] encoded in
    List.rev result;;

assert(encode2 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
       [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
        Many (4, "e")]);;

let decode l =
  let rec expand a b c acc =
    if a = c then acc else expand a b (c+1) (b :: acc)
  in
  let rec aux l acc =
    match l with
      | [] -> acc
      | One a :: t -> aux t (a :: acc)
      | Many (a, b) :: t -> aux t ((expand a b 0 []) @ acc)
  in
  let result = aux l [] in
    List.rev result;;

assert(decode (encode2 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]) =
       ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]);;


let encode3 l =
  let rle c x = if c = 0 then One x else Many (c + 1, x) in
  let rec aux l c acc =
    match l with
      | [] -> acc
      | [x] -> (rle c x) :: acc
      | a :: (b :: _ as t) -> if a = b then aux t (c+1) acc else aux t 0 ((rle c a) :: acc)
  in
  let result = aux l 0 [] in
    List.rev result;;

assert(encode3 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
       [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
        Many (4, "e")]);;


let duplicate l =
  let rec aux l a =
    match l with
      | [] -> a
      | h :: t -> aux t (h :: (h :: a))
  in
  let result = aux l [] in
    List.rev result;;

duplicate ["a"; "b"; "c"; "c"; "d"];;

let replicate l n =
  let rec repeat x n a l =
    if a = n then l else repeat x n (a+1) (x :: l)
  in

  let rec aux l a =
    match l with
      | [] -> a
      | h :: t -> aux t ((repeat h n 0 []) @ a)
  in
  let result = aux l [] in
    List.rev result;;

replicate ["a";"b";"c"] 3;;

let drop_nth l n =
  let rec aux l c a =
    match l with
      | [] -> a
      | h :: t -> if c = n then aux t 1 a else aux t (c+1) (h :: a)
  in
  let result = aux l 1 [] in
    List.rev result;;

drop_nth ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;

let split l n =
  let rec aux l c a b=
    match l with
      | [] -> (a, b)
      | h :: t -> if c < n then aux t (c+1) (h :: a) b else aux t (c+1) a (h :: b)
  in
  let (a, b) = aux l 0 [] [] in
    (List.rev a, List.rev b);;

split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;

let slice l i k =
  let rec aux l c a =
    match l with
      | [] -> a
      | h :: t -> if (c >= i) && (c <= k) then aux t (c+1) (h :: a) else aux t (c+1) a
  in
  let result = aux l 0 [] in
    List.rev result;;

slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;

let rotate l n =
  (* adjusted n *)
  let m = if n < 0 then (List.length l) + n else n in
  let rec aux l c a =
    match l with
      | [] -> a
      | h :: t -> if c < m then aux t (c+1) (List.tl a @ [h]) else aux t (c+1) a
  in
  let result = aux l 0 l in
    result;;

rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;

rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;

let remove_at k l =
  let rec aux l c a =
    match l with
      | [] -> a
      | h :: t -> if c = k then aux t (c+1) a else aux t (c+1) (h :: a)
  in
  let result = aux l 0 [] in
    List.rev result;;

remove_at 1 ["a";"b";"c";"d"];;

let insert_at e k l =
  let rec aux l c a =
    match l with
      | [] -> a
      | h :: t -> if c = k then aux t (c+1) (h :: (e :: a)) else aux t (c+1) (h :: a)
  in
  let result = aux l 0 [] in
    List.rev result;;

insert_at "alfa" 1 ["a";"b";"c";"d"];;

let range a b =
  let step = let d = a - b in
      match d with
        | 0 -> 0
        | _ -> if d < 0 then 1 else -1
  in
  let rec aux c acc =
    if c = b then c :: acc else aux (c + step) (c :: acc)
  in
  let result = aux a [] in
    List.rev result;;

range 4 9;;
range 9 4;;








