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

(* exercise: pokerecord *)
type poketype = Normal | Fire | Water
type pokemon = {name : string; hp : int; ptype : poketype}
let charizard = {name = "charizard"; hp = 78; ptype = Fire}
let squirtle = {name = "squirtle"; hp = 44; ptype = Water}

(* exercise: pokefun *)
let rec max_hp = function 
  | [] -> 0
  | h :: t -> max h.hp (max_hp t)

(* exercise: cards *)
type suit = Spades | Hearts | Clubs | Diamonds
type rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten |
  Jack | Queen | King
type card = {suit : suit; rank : rank}
let ace_of_clubs = {suit = Clubs; rank = Ace}
let queen_of_hearts = {suit = Hearts; rank = Queen}
let two_of_diamonds = {suit = Diamonds; rank = Two}
let seven_of_spades = {suit = Spades; rank = Seven}

(* exercise: quadrant *)
type quad = I | II | III | IV 
type sign = Neg | Zero | Pos
let sign x = if x > 0 then Pos else if x < 0 then Neg else Zero
let quadrant : int*int -> quad option = fun (x, y) -> begin
  match (sign x, sign y) with
    | (Pos, Pos) -> Some I
    | (Neg, Pos) -> Some II
    | (Neg, Neg) -> Some III
    | (Pos, Neg) -> Some IV
    | (_, _) -> None
end

(* exercise: quadrant when *)
let quadrant_when : int*int -> quad option = begin function
  | x, y when sign x = Pos && sign y = Pos -> Some I
  | x, y when sign x = Neg && sign y = Pos -> Some II
  | x, y when sign x = Neg && sign y = Neg -> Some III
  | x, y when sign x = Pos && sign y = Neg -> Some IV
  | (_, _) -> None
end

(* exercise: depth *)
type 'a tree = 
  | Leaf
  | Node of 'a * 'a tree * 'a tree
let rec depth (tree : 'a tree) : int = match tree with
  | Leaf -> 0
  | Node (_, l ,r) -> 1 + max (depth l) (depth r)

(* exercise: safe hd and tl *)
let safe_hd : 'a list -> 'a option = function
  | [] -> None
  | h :: t -> Some h

let rec safe_tl : 'a list -> 'a option = function
  | [] -> None
  | [h] -> Some h
  | h :: t -> safe_tl t

(* exercise: assoc list; *)
let insert k v lst = (k, v) :: lst
let rec lookup k = function
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else lookup k t
