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








