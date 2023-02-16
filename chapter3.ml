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

(* exercise: library *)
let fifth_elem lst = if List.length lst < 5 then 0 else List.nth lst 4
let decr_sort lst = List.rev (List.sort compare lst)

(* exercise: library puzzle *)
let last_elem lst = List.nth lst (List.length lst - 1)
let any_zeroes lst = List.fold_right (fun x acc -> x = 0 || acc) lst false

(* exercise: take drop *)
let rec take n lst = match lst with
  | [] -> []
  | h :: t -> if n = 0 then [] else h :: take (n - 1) t

let rec drop n lst = match lst with
  | [] -> []
  | h :: t -> if n > 0 then drop (n - 1) t else h :: drop 0 t

(* exercise: student *)
type student = {first_name : string; last_name : string; gpa : float}
let student_name s = (s.first_name, s.last_name)
let new_student first_name last_name gpa = {first_name = first_name; 
  last_name = last_name; gpa = gpa}

(* exercise: date before *)
let date_before (a, b, c) (a', b', c') = c < c' || a < a' || b < b'

