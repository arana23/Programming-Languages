(* I pledge my honor that I have abided by the Stevens Honor System-
Aparajita Rana *)

type program = int list

let square : program = [0; 2; 2; 3; 3; 4; 4; 5; 5; 1]
let letter_e : program = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1]

let rec remove_vals: 'a -> 'a list -> 'a list = fun one two ->
match two with
 | [] -> []
 | x :: xs ->
	if one=x
	then remove_vals one xs
	else x :: remove_vals one xs


let rec dup_remove: 'a list -> 'a list = fun one ->
match one with
 | [] -> []
 | x :: xs -> x :: dup_remove (remove_vals x xs)


let rec color: int*int -> int list -> int -> (int*int) list = fun init one two ->
match init, one, two with
 | _, [], _ -> []
 | (a,b), x::xs,1 ->
	if x=0
	then color (a,b) (x::xs) 0
	else if x=2
	then color (a,b+1) xs 1
	else if x=3
	then color (a+1,b) xs 1
	else if x=4
	then color (a, b-1) xs 1
	else if x=5
	then color (a-1, b) xs 1
	else color (a,b) xs 1
 | (a,b), x::xs, 0 ->
	if x=0
	then (a,b) :: color(a,b) xs 0
	else if x=1
	then color(a,b) xs 1
	else if x=2
	then (a,b+1) :: color(a, b+1) xs 0
	else if x=3
	then (a+1,b) :: color(a+1,b) xs 0
	else if x=4
	then (a,b-1) :: color(a,b-1) xs 0
	else if x=5
	then (a-1, b) :: color(a-1,b) xs 0
	else color(a,b) xs 0
 | _,_,_ -> [] 


let rec colored : int*int -> program -> (int*int) list = fun one two ->
match two with
 | [] -> []
 | x::xs -> dup_remove(color one (x:: xs) 1)


let rec eq : (int*int) list -> (int*int) list -> bool = fun one two ->
  match one,two with
  | [],[] -> true
  | x::xs,[] -> false
  | [],y::ys -> false
  | x::xs,y::ys -> equivalence xs (remove_all x (y::ys))


let equivalent : program -> program -> bool = fun one two -> 
eq (colored (0,0) one) (colored (0,0) two)


let rec mirror_image : program -> program = fun one -> 
  match one with
  | [] -> []
  | x::xs ->
    if x = 0 || x=1
    then x :: mirror_image xs
    else if x = 2
    then 4 :: mirror_image xs
    else if x = 3
    then 5 :: mirror_image xs
    else if x = 4
    then 2 :: mirror_image xs
    else if x = 5
    then 3 :: mirror_image xs
    else mirror_image xs


let rec rotate_90 : program -> program = fun one ->
  match one with
  | [] -> []
  | x::xs -> 
    if x = 0
    then x :: rotate_90 xs
    else if x = 1
    then x :: rotate_90 xs
    else if x = 2
    then 3 :: rotate_90 xs
    else if x = 3
    then 4 :: rotate_90 xs
    else if x = 4
    then 5 :: rotate_90 xs
    else if x = 5
    then 2 :: rotate_90 xs
    else rotate_90 xs



let rec repeat : int -> 'a -> 'a list = fun one two ->
  match one with
  | 0 -> []
  | one -> two :: (repeat (one-1) two)



let rec pantograph : program -> int -> program = fun one two ->
  match one,two with
  | [],two -> []
  | _,0 -> []
  | x::xs,two -> 
    if x = 0 || x = 1
    then x :: (pantograph xs two)
    else if x > 1 || x < 6
    then (repeat two x) @ (pantograph xs two)
    else pantograph xs two



let rec delete : 'a list -> int -> 'a list = fun one two ->
  match one,two with
  | [],_ -> []
  | x::xs,0 -> x::xs
  | x::xs,two -> delete xs (two-1)


let rec iter: 'a list -> 'a -> int = fun one two ->
  match one with 
  | [] -> 0
  | x::xs ->
    if x = two
    then 1 + (iter xs two)
    else 0


let rec compress : program -> (int*int) list = fun one ->
  match one with
  | [] -> []
  | x::xs -> 
    if x > -1 || x < 6
    then (x,(1 + iter xs x)) :: compress (delete xs (iter xs x))
    else compress xs


let first_val : int*int -> int = fun (x, y) -> x
let second_val : int*int -> int = fun (x, y) -> y


let rec uncompress : (int*int) list -> program = fun one ->
  match one with
  | [] -> []
  | x::xs -> 
    if first_val x > -1 && first_val x < 6 && second_val x > 0 
    then (repeat (second_val x) (first_val x)) @ (uncompress xs)
    else uncompress xs
