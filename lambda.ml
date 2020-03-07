(* ラムダ項 *)
type term =
| Var of string
| Abs of string * term
| App of term * term
| S | K | I

(* ラムダ項を文字列に変換 *)
let rec string_of_term t =
  let parenthesize s = "(" ^ s ^ ")" in
  let rec abs v t isBegin =
    let sep = ". " in (* 区切り文字 *)
    (if isBegin then "λ" else " ") ^ v ^
    match t with
    | Abs (v', t') -> abs v' t' false
    | App (t1', t2') -> sep ^ app t1' t2' true
    | _ as t' -> sep ^ string_of_term t'
  and app t1 t2 isBegin =
    (match t1 with
    | Abs (v', t') -> parenthesize (abs v' t' true)
    | App (t1', t2') -> app t1' t2' false
    | _ as t' -> string_of_term t') ^
    " " ^
    (match t2 with
    | Abs (v', t') ->
        let s = abs v' t' true in
        if isBegin then s else parenthesize s
    | App (t1', t2') -> parenthesize (app t1' t2' true)
    | _ as t' -> string_of_term t') in
  match t with
  | Var v' -> v'
  | Abs (v', t') -> abs v' t' true
  | App (t1', t2') -> app t1' t2' true
  | S -> "S"
  | K -> "K"
  | I -> "I"


(* 変数が項の自由変数であるかを求める *)
let rec is_free_variable v = function
  | S | K | I -> false
  | Var v' -> v = v'
  | App (t1, t2) -> (is_free_variable v t1) || (is_free_variable v t2)
  | Abs (v', t) -> if v = v' then false else is_free_variable v t

(* 抽象除去 *)
let rec eliminate_abstraction = function
  | S | K | I | Var _ as x -> x
  | Abs (v, App (t, Var v')) when v = v' && not (is_free_variable v t) -> eliminate_abstraction t
  | App (t1, t2) -> App (eliminate_abstraction t1, eliminate_abstraction t2)
  | Abs (v, t) when not (is_free_variable v t) -> App (K, eliminate_abstraction t)
  | Abs (v, Var v') when v = v' -> I
  | Abs (v, Abs (v', t)) -> eliminate_abstraction (Abs (v, eliminate_abstraction (Abs (v', t))))
  | Abs (v, App (t1, t2)) -> App (App (S, eliminate_abstraction (Abs (v, t1))), eliminate_abstraction (Abs (v, t2)))
  | _ -> assert false


(* テスト *)
(* let exps = [
  Abs ("x", Var "x");
  App (Abs ("x", App (Var "x", Var "x")), Abs ("x", App (Var "x", Var "x")));
  Abs ("f", App (Abs ("x", App (Var "f", App (Var "x", Var "x"))), Abs ("x", App (Var "f", App (Var "x", Var "x")))))
]
let show t = string_of_term t ^ " ==> " ^ string_of_term @@ eliminate_abstraction t
let () = print_endline @@ String.concat "\n" @@ List.map show exps *)

(* skiコンビネータ項として出力する *)
let rec string_of_ski = function
  | S -> "s"
  | K -> "k"
  | I -> "i"
  | App (t1, t2) -> "`" ^ string_of_ski t1 ^ string_of_ski t2
  | Abs _ -> failwith "Given expression contains lambda abstraction"
  | Var _ -> failwith "Given expression contains free variables"

(* Welcome to Lazy K *)

let nat n =
  let rec tailnat m t =
    if m = 0 then t
    else tailnat (m - 1) (App (Var "s", t)) in
  Abs ("s", Abs ("z", tailnat n (Var "z")))

let natc c = nat (int_of_char c)

let cons a d = Abs ("f", App (App (Var "f", a), d))

let program = App (K,
  cons (natc 'H') @@
  cons (natc 'e') @@
  cons (natc 'l') @@
  cons (natc 'l') @@
  cons (natc 'o') @@
  cons (natc ',') @@
  cons (natc ' ') @@
  cons (natc 'w') @@
  cons (natc 'o') @@
  cons (natc 'r') @@
  cons (natc 'l') @@
  cons (natc 'd') @@
  cons (natc '!') @@
  App (K, nat 256))

let code = string_of_ski (eliminate_abstraction program)
let () = print_string code