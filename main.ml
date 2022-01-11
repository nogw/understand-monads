let print_list l =
  l |> List.map string_of_int |> String.concat "; " |> Printf.printf "[%s]\n"
;;

module Maybe = struct
  type 'a maybe =
    | Just of 'a
    | Nothing

  exception Invalid_argument of string

  let then' f m =
    match m with
    | Nothing -> raise (Invalid_argument "maybe is Nothing")
    | Just v -> f v
  ;;

  let resolve v = Just v
  
  let ( >>= ) m f = then' f m

  let print m =
    match m with
    | Just m -> Printf.printf "Just %s" m
    | Nothing -> raise (Invalid_argument "maybe is Nothing")
  ;;
end

open Maybe

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

let a () =
  safe_tail [ 1; 2; 3; 4 ] >>= print_list;
  map (fun x -> x * 2) [ 1; 2; 3; 4 ] >>= print_list;
  map (fun x -> x * 2) [ 1; 2; 3; 4 ] >>= get_first_even_safe >>= print_int;
  map (fun x -> x * 2) [ 1; 2; 3; 4 ] >>= get_first_even_safe >>= print_int
;;

let b () =
  let monad =
    resolve "abc"
    |> then' (fun v -> resolve (v ^ "def"))
    |> then' (fun v -> resolve (v ^ "ghi"))
    |> then' (fun v -> resolve (v ^ "jkl"))
  in
  print monad
;;

type state = Locked | Unlocked
type output = Thank | Open | Tut

let coin = function _ -> (Thank, Unlocked)

let push state =
  match state with 
  | Locked -> (Tut, Locked) 
  | Unlocked -> (Open, Locked)

module type MONAD =
sig 
    type 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
end

module type STATE = 
sig
    type t
    val empty : t
end

module type STATE_MONAD = functor(State: STATE) ->
sig
    include MONAD
    val access : 'a t -> 'a
    val put    : State.t -> unit t
    val get    : State.t t
    (* val eval   : 'a * 'b -> 'a  *)
    (* val exec   : 'a * 'b -> 'b *)
end

module State: STATE_MONAD = functor(State : STATE) ->
struct
    type state = State.t
    type 'a t = state -> ('a * state)
    let bind m f = fun s -> match m s with | (x, s') -> f x s'        
    let return a = fun s -> (a, s)
    let access m = 
        match m State.empty with
        | (x, _) -> x
    let put s = fun _ -> ((), s)
    let get = fun s -> (s, s)

    (* TODO *)
    (* let eval = fun s -> fst s *)
    (* let exec = fun s -> snd s *)
end

module IntState = State(
  struct
    type t = int
    let empty = 0
  end
)

let () = 
  let state  = IntState.return 1 in 
  let state' = IntState.bind state (fun i -> IntState.return (succ i)) in
  print_endline (string_of_int (IntState.access state'))  