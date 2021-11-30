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
:- dynamic entrega/9.

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
% Cliente: #idCliente, idFreguesia -> {V,F}

cliente(1, 0).
cliente(2, 1).
cliente(3, 1).
cliente(4, 3).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%-------- Freguesia ----------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Freguesia: #id, nome -> {V,F}

freguesia(0,'Braga').
freguesia(1,'Porto').
freguesia(2,'Famalição').
freguesia(3,'Viana do Castelo').

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
encomenda(3, 15.3, 12).
encomenda(5, 7.4, 30).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%-------- Entrega --------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Entrega: #Data, Prazo, #IdEntrega, #IdEncomenda, #IdCliente, #IdEstafeta, IdVeiculo, Custo, PontuacaoEntrega  -> {V,F}
entrega(data(13, 12, 2021), 14.0, 0, 1, 2, 4, 1,  19.99,3). %query2(2,[1,2],L).
entrega(data(13, 12, 2021), 24.0, 1, 2, 2, 3, 1,  13.99,4). 
entrega(data(19, 05, 2021), 16.0, 2, 3, 1, 4, 2, 15.99,2).
entrega(data(16, 11, 2021), 20.0, 3, 2, 3, 3, 2, 29.99,5).
entrega(data(18, 09, 2021), 60.0, 4, 3, 4, 2, 3, 39.99,4).


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

%------------------------------ Registos ------------------------------
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Registar Entrega
registaEntrega(Data, Prazo, IdE, IdEnc, IdC, IdEst, P, Mt, C,Pe) :-
             inserir(entrega(Data, Prazo, IdE,IdEnc, IdC, IdEst, P, Mt, C,Pe)).

% Registar Cliente
registaCliente(IdCliente, IdFreguesia) :- inserir(cliente(IdCliente, IdFreguesia)).

% Registar Estafeta
registaEstafeta(IdEstafeta, Pontuacao, Nome) :- inserir(estafeta(IdEstafeta,Pontuacao, Nome)).

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
    solucoes(entrega(Data,Prazo,IdEntrega,IdEnc,IdC,IdEst,1,Custo,Pe), (estafeta(IdEst,_,_),entrega(Data,Prazo,IdEntrega,IdEnc,IdC,IdEst,1,Custo,Pe)), R).

entregas_de_estafeta_mota(IdEst,R) :- 
    solucoes(entrega(Data,Prazo,IdEntrega,IdEnc,IdC,IdEst,2,Custo,Pe), (estafeta(IdEst,_,_),entrega(Data,Prazo,IdEntrega,IdEnc,IdC,IdEst,2,Custo,Pe)), R).

calcular_escologia_estafeta(IdEstafeta, R):-
    entregas_de_estafeta_bicicleta(IdEstafeta, X),
    comprimento(X,L1),
    entregas_de_estafeta_mota(IdEstafeta,Y),
    comprimento(Y,L2),
    R is (L1*2 + L2 * 1).


calcula_ecologia_recursive(L):- solucoes([estafeta(IdEst, Pont, N),Y], (estafeta(IdEst, Pont, N), calcular_escologia_estafeta(IdEst,Y)), L).

lista_de_estafetas(L):- solucoes(estafeta(IdEst,Pont,N),(estafeta(IdEst, Pont, N)), L).

lista_de_ecologias(L):- solucoes(Y, (estafeta(IdEst, _, _), calcular_escologia_estafeta(IdEst,Y)), L).


query1(R):- lista_de_estafetas(L1),
            lista_de_ecologias(L2),
            max_list(L2,M),
            indexOf(L2,M,I),
            nth0(I, L1, R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%---------- 2. identificar que estafetas entregaram determinada(s) encomenda(s) a um determinado cliente;  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%Entrega: #Data, Prazo, #IdEntrega, #IdEncomenda, #IdCliente, #IdEstafeta, IdVeiculo, Custo,PontuacaoEntrega  -> {V,F}

query2(IdCliente,[X],L) :- solucoes(estafeta(IdEst,Pont,N),(estafeta(IdEst,Pont,N),encomenda(X,_,_),entrega(_,_,_,X,IdCliente,IdEst,_,_,_)),L).
query2(IdCliente,[X|H],L) :- solucoes(estafeta(IdEst,Pont,N),(estafeta(IdEst,Pont,N),encomenda(X,_,_),entrega(_,_,_,X,IdCliente,IdEst,_,_,_)),L1),
                            query2(IdCliente,H,L2),
                            append(L1,L2,L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%---------- 3. identificar os clientes servidos por um determinado estafeta  -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
query3(IdEst,L) :- solucoes(cliente(IdC,Freguesia), (estafeta(IdEst,_,_),cliente(IdC,Freguesia),entrega(_,_,_,_,IdC,IdEst,_,_,_)), X),
    diferentes(X,L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%---------- 4. calcular o valor faturado pela Green Distribution num determinado dia  -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

query4(data(DD,MM,AA),R):- solucoes(Custo,(entrega((data(DD,MM,AA)),_,_,_,_,_,_,Custo,_)),L),
                            sum_list(L, R).                        

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%---------- 5. Identificar as zonas com maior volume de entregas  -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
             %função iguais/repetidos calculo número de ocorrencias de uma freguesia (cumprimento + função diferentes + cumprimento)
             %criar lista de tuplo(freguesia,n_ocorencias)
             %take N busca N freguesias com mais numero de ocorrencias
             %percorrer lista de diferentes e contar ocorrencias na outra lista.
query5(N,R):-solucoes(freguesia(IdF,Cidade),(freguesia(IdF,Cidade),entrega(_, _, _,_, IdCliente, _, _, _,_),cliente(IdCliente, IdF)),LFreg1),
            diferentes( LFreg1,LFreg2),
            lista_ocorrencias(LFreg1,LFreg2,Ocorrencias),
            busca(N,Ocorrencias,LFreg2,R).

busca(0,_,_,[]).
busca(N,Ocorrencias,Lista,[X|T]):- comprimento(Lista,C), N =< C ,max_list(Ocorrencias,M),
                            nth0(I,Ocorrencias,M), 
                            nth0(I,Lista,X),
                            away(Ocorrencias,I,Ocorrencias2),
                            away(Lista,I,Lista2),
                            N1 is N-1,
                            busca(N1,Ocorrencias2,Lista2,T).

%Calcula as ocorrencias dos elemento de uma lista(L2) em outra(L1)   
%lista_ocorrencias(L1,L2,R).
lista_ocorrencias(_,[],[]).
lista_ocorrencias(L,[X|T1],[R|T2]) :- count(X, L, R),lista_ocorrencias(L,T1,T2).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%---------- 6.calcular a classificação media de satisfação de cliente para um determinado estafeta  -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
lista_de_pontuacoes_estafeta(IdEst,L) :- solucoes(Pe, (estafeta(IdEst,_,_), entrega(_,_,_,_,_,IdEst,_,_,Pe)), L).%problema na pontuação

query6(IdEst,R):- lista_de_pontuacoes_estafeta(IdEst,X),
    comprimento(X,L),
    sum_list(X,S),
    R is S / L.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%---------- 7. identificar o número total de entregas pelos diferentes meios de transporte, num determinado intervalo de tempo  -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
query7(data(DD1,MM1,AA1),data(DD2,MM2,AA2),Bicicleta,Mota,Carro) :-solucoes(D,(entrega(D,_,_,_,_,_,1,_,_)),EBicicleta),
                                                                    solucoes(D,(entrega(D,_,_,_,_,_,2,_,_)),EMota),
                                                                    solucoes(D,(entrega(D,_,_,_,_,_,3,_,_)),ECarro),
                                                                    filtra_lista(EBicicleta,data(DD1,MM1,AA1),data(DD2,MM2,AA2),X1),
                                                                    filtra_lista(EMota,data(DD1,MM1,AA1),data(DD2,MM2,AA2),X2),
                                                                    filtra_lista(ECarro,data(DD1,MM1,AA1),data(DD2,MM2,AA2),X3),
                                                                    comprimento(X1,Bicicleta),
                                                                    comprimento(X2,Mota),
                                                                    comprimento(X3,Carro).
                                                                    
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%---------- 8. identificar o número total de entregas pelos estafetas, num determinado intervalo de tempo  -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

query8(data(DD1,MM1,AA1),data(DD2,MM2,AA2),R) :-solucoes(D,(entrega(D,_,_,_,_,_,_,_,_)),X1),
                                                filtra_lista(X1,data(DD1,MM1,AA1),data(DD2,MM2,AA2),X2),
                                                comprimento(X2,R).

entre_datas(D1,D2,D3):-anterior(D1,D3),posterior(D2,D3).

filtra_lista([],_,_,[]).
filtra_lista([X|T],D1,D2,[X|T2]) :- entre_datas(D1,D2,X),
                            filtra_lista(T,D1,D2,T2).
filtra_lista([X|T],D1,D2,R) :- not(entre_datas(D1,D2,X)), filtra_lista(T,D1,D2,R).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%---------- 9. calcular o número de encomendas entregues e não entregues, num determinado periodo de tempo  -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
query9(data(DD1,MM1,AA1),data(DD2,MM2,AA2),NE,E):-solucoes(D,(entrega(D,_,_,_,_,_,_,_,_)),All),
                                                    filtra_lista(All,data(DD1,MM1,AA1),data(DD2,MM2,AA2),X1),
                                                    comprimento(X1,A),
                                                    solucoes(Data,(entrega(Data,_,_,IdEnc,_,_,_,_,_),encomenda(IdEnc,_,_)),Entregues),
                                                    filtra_lista(Entregues,data(DD1,MM1,AA1),data(DD2,MM2,AA2),X2),
                                                    comprimento(X2,E),
                                                    NE is A-E.
                                                


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%---------- 10. calcular o peso total transporte por estafeta num determinado dia  -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
query10(IdEst,data(DD,MM,AA),R) :- solucoes(P,(entrega(data(DD,MM,AA),_,_,IdEnc,_,IdEst,_,_,_),estafeta(IdEst,_,_),encomenda(IdEnc,P,_)),X1),
                            sum_list(X1,R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%verificar se uma data é igual
igual(data(DD,MM,AA),data(DD,MM,AA)).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Verificar se uma data é anterior a outra
anterior(data(_,_,A1),data(_,_,A2)) :- A1 < A2.
anterior(data(_,M1,A1),data(_,M2,A2)) :- A1 == A2, M1 < M2.
anterior(data(D1,M1,A1),data(D2,M2,A2)) :- A1 == A2, M1 == M2, D1 =< D2.
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%Verificar se uma data é posterior a outra
posterior(data(_,_,A1),data(_,_,A2)) :- A1 > A2.
posterior(data(_,M1,A1),data(_,M2,A2)) :- A1 == A2, M1 > M2.
posterior(data(D1,M1,A1),data(D2,M2,A2)) :- A1 == A2, M1 == M2, D1 >= D2.
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Contar numero de ocorrencia de um elemento numa lista 
count(_, [], 0).
count(X, [X | T], N) :-!, count(X, T, N1),N is N1 + 1.
count(X, [_ | T], N) :- count(X, T, N).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Somatório dos elementos de uma lista
sum_list([], 0).
sum_list([H|T], Sum) :-
   sum_list(T, Rest),
   Sum is H + Rest.
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Pertencer a uma Lista
pertence(H,[H|_]):-!,true.
pertence(X,[H|T]) :-
    X \= H,
    pertence(X,T).

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
comprimento([_|T],R) :- comprimento(T,N), R is N+1.

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

% Remove o elemento presente num determinado index
away([_|H],0,H):-!.
away([G|H],N,[G|L]):- N >= 1, N1 is N - 1,!,away(H,N1,L). 