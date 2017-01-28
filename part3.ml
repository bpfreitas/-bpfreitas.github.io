(* Blake Freitas*)
(*range function*)
let rec range num =
  match num with
  |0->[0]
  |num -> range (num-1)@[num];;

(*reverse function*)
let rec reverse lists =
  match lists with
  |[] -> []
  |a :: [] -> [a]
  |beginning :: ending -> reverse ending @ [beginning];;

(*intersection function*)
let rec intersection list1 list2 =
  match list1 with
  |[] -> []
  |a :: ending -> if List.mem a list2
    then a::(intersection ending list2)
    else intersection ending list2;;

(*sum function*)
let rec sum one two =
  let rec iterate first second =
    match first with
    |[] -> []
    |a::ending ->(second,a)::iterate ending second in
  List.append(iterate one 0) (iterate two 1);;

let rec forall a lists =
  match lists with
  |[] -> false
  |b::[] -> if a b then true else false
  |b::ending -> if a b then forall a ending else false;;

let isEven num = num mod 2 = 0;;
