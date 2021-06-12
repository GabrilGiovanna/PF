let x= read_int() (* Lê inteiro do teclado *)

(* A minha solução do problema utiliza uma única lista consequentemente concatenada e mapeada, em que as três regras são usadas em simultâneo, até ficar, ou uma lista vazia
ou uma lista com o 42 lá dentro*)


 (* regra 1 do Problema, se o resto do inteiro com 2 for igual a 0, então é par, 
e aplica-se a divisão por 2, se o resultado da subtração do x(Dinheiro) com a mesma for igual ou superior a 42, e guarda-se o valor numa lista, senão retorna lista vazia *)

let regra1 (x : int) : int list = if x mod 2 = 0 then if x / 2 >= 42 then [x / 2] else [] else[] 

(* regra 2 do Problema, se o resto do inteiro com 3, ou 4 for igual a 0, então é multiplo de 3 ou 4 respectivamente, 
e se a multiplicaçao dos dois ultimos digitos for maior que 0 e a subtração do x(Dinheiro) com a mesma for igual ou superior a 42,
 aplica-se a regra, e guarda-se o valor numa lista, senão retorna lista vazia *)

let regra2 (x : int) : int list = if x mod 3 = 0 || x mod 4=0 then if ((x mod 10) * ((x / 10) mod 10)) > 0 then if
 (x - ((x mod 10) * ((x / 10) mod 10))) >= 42 then 
 [x - ((x mod 10) * ((x / 10) mod 10))] else [] else[] else[]


 (* regra 3 do Problema, se o resto do inteiro com 5 for igual a 0, então é multiplo de 5, e subtrai-se x(Dinheiro) por 42(se a subtração for igual ou superior a 42)
 e guarda-se o valor numa lista, senão retorna lista vazia *)

let regra3 (x : int) : int list = if x mod 5 = 0 then if x - 42 >= 42 then [x - 42] else[] else []

let regras (x : int) : int list  = (regra1 x) @ (regra2 x) @ (regra3 x) (* De forma a juntar as 3 regras numa única função, para as aplicar ao mesmo tempo na função recursiva*)



let rec pba passo lista =
  if lista = [] then -1  (* Se a lista estiver vazia a qualquer altura, é porque nenhuma regra conseguiu ser aplicada a certo ponto, portanto não é possivel devolver mais dinheiro*)
  else if List.mem 42 lista then passo  (* Se em qualquer ponto do algoritmo, existir um 42 na lista, então quer dizer que o caminho mais rápido já foi encontrado, e devolve o número de passos*)
  else pba (passo + 1) (List.concat (List.map regras lista)) (*Se nenhuma das condições anteriores for atingida, chamamos a função recursivamente incrementando o número de passos
  e aqui aplicamos simultaneamente as 3 regras na lista de inteiros, com a função map, o que irá devolver uma lista de listas de inteiros, e por isso concatenamos consecutivamente após mapear*)

(* Exemplo: 

pba 0 [250]

-> pba 1 [125; 208]
-> pba 2 [83; 104]
-> pba 3 [52]
-> pba 4 [42]
-> 4
*)

let () =
 let r= pba 0 [x] in 
 if r > -1 then Printf.printf "%d\n" r else print_endline "BAD LUCK" 
