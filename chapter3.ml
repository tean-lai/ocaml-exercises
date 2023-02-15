(* https://cs3110.github.io/textbook/chapters/data/exercises.html *)

(* exercise: list expressions *)
let _ = [1; 2; 3; 4; 5]
let _ = 1 :: 2 :: 3 :: 4 :: 5 :: []
let _ = [1] @ [2] @ [3] @ [4] @ [5]

(* exercise: product *)
let rec product = function
  | [] -> 1
  | h :: t -> h * product t

let product_tr l = 
  let rec aux acc l = match l with
    | [] -> acc
    | h :: t -> aux (h * acc) t in 
  aux 1 l

(* exercise: concat *)
let rec concat = function 
  | [] -> ""
  | h :: t -> h ^ concat t

(* exercise: patterns *)
let first_bigred lst = match lst with
  | [] -> false
  | h :: _ -> h = "bigred"

let two_or_four lst = match lst with
  | [_; _] -> true
  | [_; _; _; _] -> true
  | _ -> false

let first_two_equal lst = match lst with
  | n1 :: n2 :: t -> n1 = n2
  | _ -> false
