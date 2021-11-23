
solucoes(T,Q,S) :- findall(T,Q,S).

% Cliente: #idCliente, Morada -> {V,F}

cliente(1, 'Rua do Meio').
cliente(2, 'Rua do Verde').
cliente(3, 'Rua do Amarelo').
cliente(4, 'Rua do Amarelo').


estafeta(0, 4.1 ,'Daniel').
estafeta(1, 3.9 ,'Nuno').
estafeta(2, 3.5 ,'Guilherme').
estafeta(3, 2.3 ,'Rodrigo').
estafeta(4, 5.0,'Nunão').




entrega(data(13, 12, 2021), 0, 1, 2, 4, 'Bicicleta',  19.99). % # 0 = imediato
entrega(data(13, 12, 2021), 24.0, 1, 2, 4, 'Bicicleta',  19.99).
entrega(data(19, 05, 2021), 16.0, 4, 8, 4, 'Bicicleta', 19.99).
entrega(data(16, 11, 2021), 2.0, 2, 9, 3, 'Mota', 29.99).
entrega(data(18, 09, 2021), 6.0, 3, 5, 2, 'Carro', 39.99).

veiculo(1,'Bicicleta').
veiculo(2,'Mota').
veiculo(3,'Carro').



encomenda(0, 2.5, 10).
encomenda(1, 5.2, 15).
encomenda(2, 19.1, 30).
encomenda(5, 7.4, 30).


% Cliente: #idCliente, Morada -> {V,F}
% Estafeta: #idEstafeta, Pontuacao, Nome -> {V,F}
% Encomenda: #IdEncomenda, Peso, Volume -> {V,F}
% Entrega: # Prazo, #IdEntrega, #IdCliente, #IdEstafeta, MeioTransporte, Custo  -> {V,F}

entregas_de_estafeta(IdEst,R) :- 
    solucoes(entrega(Data,Prazo,IdEntrega,IdC,IdEst,Mt,Custo), (estafeta(IdEst,_,_),entrega(Data,Prazo,IdEntrega,IdC,IdEst,Mt,Custo)), R).


indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),
  !,
  Index is Index1+1.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Concatenar uma lista
append([ ], L, L).
append([H|L1], L2, [H|L3]):- append(L1, L2, L3).