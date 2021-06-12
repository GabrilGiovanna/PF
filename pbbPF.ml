(* Gabriel Alexandre Araújo Ribeiro, n° 41235 *)

(* Dado pelo enunciado *)
type color = W | B
type image =
  | L of color
  | N of image * image * image * image

(* Lê o valor N do input *)
let _ = read_line ()
let n = Scanf.sscanf (read_line ()) "%d %d" (fun x _ -> x)

(* Lê a matriz da imagem do input *)
let read_image n =
  let a = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    List.iteri (fun j x -> a.(i).(j) <- int_of_string x) (String.split_on_char ' ' (read_line ()))
  done;
  a

let img = read_image n

(* Verifica se a matriz é toda igual *)
let all_equal matrix =
  Array.for_all (
    fun a -> Array.for_all (
      fun x -> x = matrix.(0).(0)
    ) a
  ) matrix

(* Determina a cor da matriz *)
let color_of_matrix matrix =
  match matrix.(0).(0) with
  | 0 -> W
  | _ -> B

(* Quadrante NW *)
let nw matrix =
  let len = Array.length matrix in
  let a = Array.make_matrix (len / 2) (len / 2) 0 in
  for i = 0 to len / 2 - 1 do
    for j = 0 to len / 2 - 1 do
      a.(i).(j) <- matrix.(i).(j)
    done
  done;
  a

(* Quadrante SW *)
let sw matrix =
  let len = Array.length matrix in
  let a = Array.make_matrix (len / 2) (len / 2) 0 in
  for i = 0 to len / 2 - 1 do
    for j = 0 to len / 2 - 1 do
      a.(i).(j) <- matrix.(i + len / 2).(j)
    done
  done;
  a

(* Quadrante NE *)
let ne matrix =
  let len = Array.length matrix in
  let a = Array.make_matrix (len / 2) (len / 2) 0 in
  for i = 0 to len / 2 - 1 do
    for j = 0 to len / 2 - 1 do
      a.(i).(j) <- matrix.(i).(j + len / 2)
    done
  done;
  a

(* Quadrante SE *)
let se matrix =
  let len = Array.length matrix in
  let a = Array.make_matrix (len / 2) (len / 2) 0 in
  for i = 0 to len / 2 - 1 do
    for j = 0 to len / 2 - 1 do
      a.(i).(j) <- matrix.(i + len / 2).(j + len / 2)
    done
  done;
  a

(* Transforma a matriz em árvore, e conta as folhas e o nível da folha mais alta *)
let make_tree matriz =
  let leafs = ref 0 in
  let height = ref (-1) in
  let rec make matrix level =
    if all_equal matrix then begin
      leafs := !leafs + 1;
      if !height = (-1)
        then height := level
        else height := min !height level;
      L (color_of_matrix matrix)
    end else
      let m1 = make (nw matrix) (level + 1) in
      let m2 = make (sw matrix) (level + 1) in
      let m3 = make (ne matrix) (level + 1) in
      let m4 = make (se matrix) (level + 1) in
        N (m1, m2, m3, m4)
  in
    let tree = make matriz 0 in (tree, !leafs, !height)

let (tree, leafs, height) = make_tree img


(* Lê o valor P do input para poder calcular a thumbnail *)
let p = read_int ()

(* Calcula a cor de um nodo 
    n |  1 |  2 |  4 |  8 | ...
força |  1 |  4 | 16 | 64 | ...
  força = n^2
*)
let rec color_of_node node n =
  match node with
  | L W -> 0
  | L B -> n * n
  | N (a, b, c, d) ->
      let c1 = color_of_node a (n / 2) in
      let c2 = color_of_node b (n / 2) in
      let c3 = color_of_node c (n / 2) in
      let c4 = color_of_node d (n / 2) in
        c1 + c2 + c3 + c4

(* Transforma nodo em folha *)
let node_to_leaf node n =
  let c = color_of_node node n in
  if c >= n * n / 2 then L B else L W

(* Corta a árvore para fazer uma thumbnail de p×p *)
let rec make_cut tree n p =
  if p = 1 then
    match tree with
    | L c  -> L c
    | node -> node_to_leaf node n
  else
    match tree with
    | L c -> L c
    | N (a, b, c, d) -> 
        N (
          make_cut a (n / 2) (p / 2),
          make_cut b (n / 2) (p / 2),
          make_cut c (n / 2) (p / 2),
          make_cut d (n / 2) (p / 2)
        )

(* Transforma a árvore em matriz *)
let make_matrix tree p =
  let a = Array.make_matrix p p 0 in
  let rec fill posx posy len =
    for i = posx to posx + len - 1 do
      Array.fill a.(i) posy len 1
    done
  in
  let rec quadrant node posx posy len =
    match node with
    | L W -> ()
    | L B -> fill posx posy len
    | N (a, b, c, d) ->
        quadrant a posx posy (len / 2);
        quadrant b (posx + len / 2) posy (len / 2);
        quadrant c (posx) (posy + len / 2) (len / 2);
        quadrant d (posx + len / 2) (posy + len / 2) (len / 2)
  in
    quadrant tree 0 0 p;
    a

(* Faz a thumbnail e transforma logo em matriz *)
let result = make_matrix (make_cut tree n p) p

(* Faz output da matriz *)
let print_matrix matrix =
  let n = Array.length matrix.(0) in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      Printf.printf "%d" matrix.(i).(j);
      if j <> n - 1 then Printf.printf " "
    done;
    Printf.printf "\n"
  done


(* Faz output dos valores pedidos e da thumbnail *)
let () = Printf.printf "%d\n%d\n" height leafs
let () = print_matrix result