let print_list l =
  l |> List.map string_of_int |> String.concat "; " |> Printf.printf "[%s]\n"
;;

type 'a maybe =
  | Just of 'a
  | Nothing

exception Invalid_argument of string

let ( >>= ) m f =
  match m with
  | Just v -> f v
  | Nothing -> raise (Invalid_argument "maybe is Nothing")
;;

let safe_tail t =
  match t with
  | _ :: t -> Just t
  | [] -> Nothing
;;

let rec get_first_even_safe l =
  match l with
  | h :: t -> if h mod 2 = 0 then Just h else get_first_even_safe t
  | [] -> Nothing
;;

let map f l =
  let rec aux l acc =
    match l, acc with
    | [], [] -> Nothing
    | [], acc -> Just (List.rev acc)
    | h :: t, acc -> aux t (f h :: acc)
  in
  aux l []
;;

let () =
  safe_tail [ 1; 2; 3; 4 ] >>= print_list;
  map (fun x -> x * 2) [ 1; 2; 3; 4 ] >>= print_list;
  map (fun x -> x * 2) [ 1; 2; 3; 4 ] >>= get_first_even_safe >>= print_int
;;
