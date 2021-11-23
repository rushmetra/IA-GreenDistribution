%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - LEI/3 - FASE 1

% GRUPO 41
% Rodrigo Pires Rodrigues  - A93201
% Daniel Torres Azevedo - A93324
% Rui Guilherme Monteiro  - A93179
% Nuno Miguel Sanches Rocha Peixoto - A93244


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag(discontiguous_warnings, off).
:- set_prolog_flag(single_var_warnings, off).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic cliente/2.
:- dynamic estafeta/3.
:- dynamic encomenda/3.
:- dynamic entrega/8.

solucoes(T,Q,S) :- findall(T,Q,S).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%-------------------------- Invariantes ------------------------------
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes estruturais: nao permitir a insercao de conhecimento
%                          repetido nem inváilido




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes referenciais: nao permite relacionar uma entidade a outra
%                           que nao exista (aquando da insercao)





%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%----------------------- Base de Conhecimento  - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%-------- Cliente ----------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Cliente: #idCliente, Morada -> {V,F}

cliente(1, 'Rua do Meio').
cliente(2, 'Rua do Verde').
cliente(3, 'Rua do Amarelo').
cliente(4, 'Rua do Amarelo').


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%-------- Estafeta ---------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Estafeta: #idEstafeta, Pontuacao, Nome -> {V,F}

estafeta(0, 4.1 ,'Daniel').
estafeta(1, 3.9 ,'Nuno').
estafeta(2, 3.5 ,'Guilherme').
estafeta(3, 2.3 ,'Rodrigo').
estafeta(4, 5.0,'Nunão').

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%-------- Encomenda --------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Encomenda: #IdEncomenda, Peso, Volume -> {V,F}

encomenda(0, 2.5, 10).
encomenda(1, 5.2, 15).
encomenda(2, 19.1, 30).
encomenda(5, 7.4, 30).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%-------- Entrega --------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Entrega: #Data, Prazo, #IdEntrega, #IdCliente, #IdEstafeta, IdVeiculo, Custo  -> {V,F}
entrega(data(13, 12, 2021), 0, 1, 2, 4, 1,  19.99). % # 0 = imediato
entrega(data(13, 12, 2021), 24.0, 1, 2, 4, 1,  13.99).
entrega(data(19, 05, 2021), 16.0, 4, 1, 4, 2, 15.99).
entrega(data(16, 11, 2021), 2.0, 2, 9, 3, 2, 29.99).
entrega(data(18, 09, 2021), 6.0, 3, 5, 2, 3, 39.99).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%-------- Veiculo --------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Veiculo: #IdVeiculo, TipoVeiculo -> {V,F}
veiculo(1,'Bicicleta').
veiculo(2,'Mota').
veiculo(3,'Carro').



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Inserir predicados

inserir(Termo) :- assert(Termo).
inserir(Termo) :- retract(Termo), !, fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que permite a evolucao do conhecimento
% solucoes(Invariante,+Termo::Invariante,Lista),

evolucao( Termo ) :- solucoes(,,Lista),
		     inserir(Termo),
                     teste(Lista).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicado teste
teste([]).
teste([R|LR]) :- R, teste(LR).

%------------------------------ Registos ------------------------------
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Registar Entrega
registaEntrega(Data, Prazo, IdE, IdC, IdEst, P, Mt, C) :-
             evolucao(entrega(Data, Prazo, IdE, IdC, IdEst, P, Mt, C)).

% Registar Cliente
registaCliente(IdCliente, Morada) :- inserir(cliente(IdCliente, Morada)).

% Registar Estafeta
registaEstafeta(IdEstafeta, Nome) :- inserir(estafeta(IdEstafeta, Nome)).

% Registar Encomenda
registaEncomenda(IdEncomenda, Peso, Volume) :- inserir(encomenda(IdEncomenda, Peso, Volume)).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Mostrar registos
mostrarRegistos(P) :- listing(P).

% Lista de veiculos incluídos na fase 1
veiculos(['Bicicleta','Mota','Carro']).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%---------------------- Predicados Solicitados - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%---------- 1. identificar estafeta que utilizou mais vezes um meio de transporte mais ecológico   -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

entregas_de_estafeta_bicicleta(IdEst,R) :- 
    solucoes(entrega(Data,Prazo,IdEntrega,IdC,IdEst,1,Custo), (estafeta(IdEst,_,_),entrega(Data,Prazo,IdEntrega,IdC,IdEst,1,Custo)), R).

entregas_de_estafeta_mota(IdEst,R) :- 
    solucoes(entrega(Data,Prazo,IdEntrega,IdC,IdEst,2,Custo), (estafeta(IdEst,_,_),entrega(Data,Prazo,IdEntrega,IdC,IdEst,2,Custo)), R).

calcular_escologia_estafeta(IdEstafeta, R):-
    entregas_de_estafeta_bicicleta(IdEstafeta, X),
    comprimento(X,L1),
    entregas_de_estafeta_mota(IdEstafeta,Y),
    comprimento(Y,L2),
    R is (L1*2 + L2 * 1).


calcula_ecologia_recursive(L):- solucoes([estafeta(IdEst, Pont, N),Y], (estafeta(IdEst, Pont, N), calcular_escologia_estafeta(IdEst,Y)), L).

lista_de_estafetas(L):- solucoes(estafeta(IdEst,Pont,N),(estafeta(IdEst, Pont, N)), L).

lista_de_ecologias(L):- solucoes(Y, (estafeta(IdEst, Pont, N), calcular_escologia_estafeta(IdEst,Y)), L).


query1(R):- lista_de_estafetas(L1),
            lista_de_ecologias(L2),
            max_list(L2,M),
            indexOf(L2,M,I),
            nth0(I, L1, R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%---------- 3. identificar os clientes servidos por um determinado estafeta  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
query3(IdEst,L) :- solucoes(cliente(IdC,Morada), (estafeta(IdEst,_,_),cliente(IdC,Morada),entrega(Data,Prazo,IdEntrega,IdC,IdEst,IdV,Custo)), X),
    diferentes(X,L).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Obter elemento num indice de uma lista

indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),
  !,
  Index is Index1+1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Cabeça de uma lista
head([H],H).
head([H|_],H).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Concatenar uma lista
append([ ], L, L).
append([H|L1], L2, [H|L3]):- append(L1, L2, L3).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Comprimento de uma lista
comprimento([],0).
comprimento([H|T],R) :- comprimento(T,N), R is N+1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% remove os elementos repetidos de uma lista
% Extensao do predicado diferentes: L1, L2 -> {V,F}

diferentes( [],[] ).
diferentes( [X|L],[X|NL] ) :- removerElemento( L,X,TL ), diferentes( TL,NL ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% remove um elemento de uma lista
% Extensao do predicado removerElemento: L1, Y, L2 -> {V,F}

removerElemento( [],_,[] ).
removerElemento( [X|L],X,NL ) :- removerElemento( L,X,NL ).
removerElemento( [X|L],Y,[X|NL] ) :- X \== Y, removerElemento( L,Y,NL ).
