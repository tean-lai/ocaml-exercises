(* https://cs3110.github.io/textbook/chapters/hop/exercises.html *)

(* exercise: repeat *)
let rec repeat f n x = 
  if n > 0 then repeat f (n - 1) (f x)
  else x

(* exercise: exists *)
let rec exists p = function
  | [] -> false
  | h :: t -> p h || exists p t

(* exercise: library uncurried *)
let uncurried_nth (lst, n) = List.nth lst n
let uncurried_append (lst1, lst2) = List.append lst1 lst2
let uncurried_compare (char1, char2) = Char.compare char1 char2
let uncurried_max (x, y) = Stdlib.max x y

(* exercise: more list fun *)
let at_least_three lst = List.filter (fun s -> String.length s >= 3) lst
let add_one_float lst = List.map (fun x -> x +. 1.) lst
let join strs sep = match strs with
  | h :: t -> h ^ List.fold_right (fun x acc -> acc ^ sep ^ x) t ""
  | [] -> ""