(* Tail of a List
  Write a function last : 'a list -> 'a option that returns the last element of a list

  # last ["a" ; "b" ; "c" ; "d"];;
  - : string option = Some "d"
  # last [];;
  - : 'a option = None
*)

let rec last input = match input with
  | [] -> None
  | [ x ] -> Some x
  | _ :: tl -> last tl;;

last ["a" ; "b" ; "c" ; "d"];;
last [];;

(* Last Two Elements of a List
  Find the last two (last and penultimate) elements of a list.
  
  # last_two ["a"; "b"; "c"; "d"];;
  - : (string * string) option = Some ("c", "d")
  # last_two ["a"];;
  - : (string * string) option = None
*)

let rec last_two input = match input with
    | [] -> None
    | [ a; b ] -> Some (a, b)
    | _ :: tl -> last_two tl;;

last_two ["a" ; "b" ; "c" ; "d"];;
last_two ["a"];;

(* N'th Element of a List
  Find the N'th element of a list.

  Remark: OCaml has List.nth which numbers elements from 0 and raises an exception if the index is out of bounds.

  # List.nth ["a"; "b"; "c"; "d"; "e"] 2;;
  - : string = "c"
  # List.nth ["a"] 2;;
  Exception: Failure "nth".
*)

let rec nth input i = match input with
    | [] -> failwith "nth"
    | hd :: tl -> 
        if i = 0
        then hd
        else nth tl (i - 1);;

nth ["a"; "b"; "c"; "d"; "e"] 2;;
nth ["a"] 2;;

(* Length of a List
  Find the number of elements of a list.

  OCaml standard library has List.length but we ask that you reimplement it. Bonus for a tail recursive solution.

  # length ["a"; "b"; "c"];;
  - : int = 3
  # length [];;
  - : int = 0
*)

let length input = 
  let rec length' i input = match input with
      | [] -> i 
      | hd :: tl -> length' (i + 1) tl 
  in
  length' 0 input;;

length ["a"; "b"; "c"];;
length [];;

(* Reverse a List
  Reverse a list.

  OCaml standard library has List.rev but we ask that you reimplement it.

  # rev ["a"; "b"; "c"];;
  - : string list = ["c"; "b"; "a"]
*)

let rev input = 
  let rec rev' input reversed = match input with 
      | [] -> reversed
      | hd :: tl -> rev' tl (hd :: reversed)
  in
  rev' input [];;

rev ["a"; "b"; "c"];;

(* Palindrome
  Find out whether a list is a palindrome.

  Hint: A palindrome is its own reverse.

  # is_palindrome ["x"; "a"; "m"; "a"; "x"];;
  - : bool = true
  # not (is_palindrome ["a"; "b"]);;
  - : bool = true
*)

let is_palindrome input = 
  (rev input) = input;;

is_palindrome ["x"; "a"; "m"; "a"; "x"];;
not (is_palindrome ["a"; "b"]);;