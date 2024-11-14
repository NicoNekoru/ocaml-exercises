(* Flatten a List 
  Flatten a nested list structure.

  type 'a node =
    | One of 'a 
    | Many of 'a node list

  # flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;
  - : string list = ["a"; "b"; "c"; "d"; "e"]
*)

type 'a node =
  | One of 'a 
  | Many of 'a node list

let flatten input =
  let rec aux flattened = function
      | [] -> flattened
      | One hd :: tl -> aux (hd :: flattened) tl
      | Many hd :: tl -> aux (aux flattened hd) tl
  in
  List.rev (aux [] input);;

flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;

(* Eliminate Duplicates
  Eliminate consecutive duplicates of list elements.

  # compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
  - : string list = ["a"; "b"; "c"; "a"; "d"; "e"]
*)

(* ! Solution should be improved -- stop here *)
let compress input = match input with
    | [] -> []
    | hd :: tl ->
      let rec aux last compressed = function
          | [] -> List.rev compressed
          | hd :: tl ->
              if hd = last
              then aux hd compressed tl
              else aux hd (hd :: compressed) tl
      in
      aux hd [hd] tl;;

compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;