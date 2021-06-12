(* Gabriel Alexandre Araújo Ribeiro nº 41235 LEI UBI 2020/2021 *)

exception Excecao of int 

let tamanho = read_int ()  (* Lê inteiro do teclado *)

let rec ler_lista n=    (*Lê as moedas do teclado e guarda numa lista *)
  if n = 0 then []
  else [ read_int () ] @ ( ler_lista (n-1) )

let moedas = ler_lista tamanho

let rec guloso n moedas =
  if moedas = [] then 0
  else
    let x = List.hd moedas in
    let y = List.tl moedas in
      if x = n then 1
      else if x < n then guloso (n - x) moedas + 1
      else guloso n y

let dinamico n moedas =   
  let len = List.length moedas in
  let bot = List.hd (List.rev moedas) in
  let mat = Array.make_matrix len (n + 1) 0 in
  let rec executar i = function
    | [] -> ()
    | x :: y ->
      for j = bot to n do
        if i = len - 1 then mat.(i).(j) <- if j < bot then 0 else j / bot
        else if (x > j) then mat.(i).(j) <- mat.(i + 1).(j)
        else mat.(i).(j) <- min mat.(i + 1).(j) (mat.(i).(j - x) + 1)
      done;
      executar (i - 1) y
  in
  executar (len - 1) moedas;
  Array.fold_left (fun menor troco -> min menor troco.(n)) mat.(len - 1).(n) mat

let rec teste n moedas max = 
  if n > max then () 
  else if guloso n moedas <= dinamico n moedas then teste (n+1) moedas max
  else raise (Excecao n)

let () =
  try 
  teste (List.hd (List.rev moedas)) moedas (2 * List.hd moedas);
  Printf.printf "YES\n" 
  with Excecao x -> Printf.printf "%d\n" x


(* ALGORITMO GULOSO :
n -> 27
moedas -> [50; 20; 10; 5; 2; 1]
  x <- 50
  y <- [20; 10; 5; 2; 1]
  x = n ? não
  x < n ? não
    Return guloso 27 [20; 10; 5; 2; 1]

n -> 27
moedas -> [20; 10; 5; 2; 1]
  x <- 20
  y <- [10; 5; 2; 1]
  x = n ? não
  x < n ? sim
    Return 1 + guloso 7 [20; 10; 5; 2; 1]

n -> 7
moedas -> [20; 10; 5; 2; 1]
  x <- 20
  y <- [10; 5; 2; 1]
  x = n ? não
  x < n ? não
    Return guloso 7 [10; 5; 2; 1]

n -> 7
moedas -> [10; 5; 2; 1]
  x <- 10
  y <- [5; 2; 1]
  x = n ? não
  x < n ? não
    Return guloso 7 [5; 2; 1]

n -> 7
moedas -> [5; 2; 1]
  x <- 5
  y <- [2; 1]
  x = n ? não
  x < n ? sim
    Return 1 + guloso 2 [5; 2; 1]

n -> 2
moedas -> [5; 2; 1]
  x <- 5
  y <- [2; 1]
  x = n ? não
  x < n ? não
    Return guloso 2 [2; 1]

n -> 2
moedas -> [2; 1]
  x <- 2
  y <- [1]
  x = n ? sim
    Return 1

Backtrace:
  1 + 1 + 1 = 3
  (27 = 20 + 5 + 2)


ALGORITMO DINÂMICO :
n -> 6
moedas -> [4; 3; 1]
  bot <- 1
  len <- 3

Matriz mat:
    +----+----+----+----+----+----+
    |  1 |  2 |  3 |  4 |  5 |  6 | <-- Trocos de bot até n
+---+----+----+----+----+----+----+
| 4 |  1 |  2 |  1 |  2 |  3 |  2 |
| 3 |  1 |  2 |  1 |  2 |  3 |  2 |
| 1 |  1 |  2 |  3 |  4 |  5 |  6 |
+---+----+----+----+----+----+----+
  ^                             ^
  |                             |
moedas                   Mínimo = 2

Resultado: 2 passos
*)