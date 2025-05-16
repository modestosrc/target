
(* Esse programa faz parte de um teste para target sistemas *)

(* Possui 5 funcoes uma para cada desafio do teste *)

(*
1)	Observe o trecho de código abaixo: int INDICE = 13, SOMA = 0, K = 0; 
Enquanto K < INDICE faça { K = K + 1; SOMA = SOMA + K; }
Imprimir(SOMA); 
Ao final do processamento, qual será o valor da variável SOMA? 
*)
let rec um indice soma k =
    if k = indice then
        soma
    else
        um indice (soma + k) (k + 1)

(*
2) Dado a sequência de Fibonacci, onde se inicia por 0 e 1 e o próximo valor sempre será a soma dos 2 valores anteriores (exemplo: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34...), escreva um programa na linguagem que desejar onde, informado um número, ele calcule a sequência de Fibonacci e retorne uma mensagem avisando se o número informado pertence ou não a sequência. 

IMPORTANTE: Esse número pode ser informado através de qualquer entrada de sua preferência ou pode ser previamente definido no código;
*)
let rec dois n =
    if n = 0 then
        0
    else if n = 1 then
        1
    else
        dois (n - 1) + dois (n - 2)

(*
3) Dado um vetor que guarda o valor de faturamento diário de uma distribuidora, faça um programa, na linguagem que desejar, que calcule e retorne: 
• O menor valor de faturamento ocorrido em um dia do mês; 
• O maior valor de faturamento ocorrido em um dia do mês; 
• Número de dias no mês em que o valor de faturamento diário foi superior à média mensal. 

IMPORTANTE: 
a) Usar o json ou xml disponível como fonte dos dados do faturamento mensal; 
b) Podem existir dias sem faturamento, como nos finais de semana e feriados. Estes dias devem ser ignorados no cálculo da média;
*)
let tres =
    let json = Yojson.Basic.from_file "dados.json" in
    let faturamentos = Yojson.Basic.Util.(json |> member "faturamentos" |> to_list) in

    let valores =
        faturamentos
        |> List.map (fun obj -> Yojson.Basic.Util.(obj |> member "valor" |> to_float))
        |> List.filter (fun x -> x > 0.0)
    in

    let media = List.fold_left (+.) 0.0 valores /. float_of_int (List.length valores) in
    let menor = List.fold_left min (List.hd valores) valores in
    let maior = List.fold_left max (List.hd valores) valores in
    let acima_media = List.filter (fun x -> x > media) valores in

    (menor, maior, List.length acima_media)

(*
4) Dado o valor de faturamento mensal de uma distribuidora, detalhado por estado: 
•	SP – R$67.836,43 
•	RJ – R$36.678,66 
•	MG – R$29.229,88 
•	ES – R$27.165,48 
•	Outros – R$19.849,53 

Escreva um programa na linguagem que desejar onde calcule o percentual de representação que cada estado teve dentro do valor total mensal da distribuidora.
*)
let quatro =
    let estados = [("SP", 67836.43); ("RJ", 36678.66); ("MG", 29229.88); ("ES", 27165.48); ("Outros", 19849.53)] in
    let total = List.fold_left (fun acc (_, valor) -> acc +. valor) 0.0 estados in
    List.map (fun (estado, valor) -> (estado, (valor /. total) *. 100.0)) estados

(*
5) Escreva um programa que inverta os caracteres de um string. 
IMPORTANTE: 

a) Essa string pode ser informada através de qualquer entrada de sua preferência ou pode ser previamente definida no código; 
b) Evite usar funções prontas, como, por exemplo, reverse;
*)
let cinco str =
    let rec aux str acc =
        match str with
        | "" -> acc
        | _ ->
            let last_char = String.sub str (String.length str - 1) 1 in
            aux (String.sub str 0 (String.length str - 1)) (acc ^ last_char)
    in
    aux str ""

let () =
    Printf.printf "\n";
    Printf.printf "Soma: %d\n" (um 13 0 0);

    Printf.printf "\n";
    Printf.printf "Fibonacci: %d\n" (dois 13);

    Printf.printf "\n";
    let menor, maior, acima_media = tres in
    Printf.printf "Menor: %.2f \t Maior: %.2f \t Acima da media: %d\n" menor maior acima_media;

    Printf.printf "\n";
    List.iter (fun (estado, percentual) ->
        Printf.printf "%s: %.2f%%\n" estado percentual
    ) quatro;

    Printf.printf "\n";
    Printf.printf "Inversao: %s\n" (cinco "Teste 123 Mateus Felipe da Silva");
