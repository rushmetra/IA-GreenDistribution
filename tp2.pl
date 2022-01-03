%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - LEI/3 - FASE 1

% GRUPO 41
% Rodrigo Pires Rodrigues  - A93201
% Daniel Torres Azevedo - A93324
% Rui Guilherme Monteiro  - A93179
% Nuno Miguel Sanches Rocha Peixoto - A93244


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciaisS

:- set_prolog_flag(discontiguous_warnings, off).
:- set_prolog_flag(single_var_warnings, off).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic goal/1.
:- dynamic freguesia/4.
:- dynamic cliente/2.
:- dynamic estafeta/3.
:- dynamic encomenda/3.
:- dynamic entrega/9.
:- dynamic veiculo/2.

solucoes(T,Q,S) :- findall(T,Q,S).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%----------------------- Base de Conhecimento  - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% aresta: Origem, Destino, #custoDistancia, #custoTempo
aresta(gualtar,adaufe,5).
aresta(gualtar,saovitor,2).
aresta(gualtar,nogueiro,3).
aresta(adaufe,saovicente,3).
aresta(adaufe,palmeira,7).
aresta(saovitor,saovicente,5).
aresta(saovitor,sjsl,3).
aresta(saovitor,lamacaes,3).
aresta(nogueiro,lamacaes,1).
aresta(lamacaes,sjsl,4).
aresta(sjsl,se,2).
aresta(se,real,4).
aresta(se,saovicente,4).
aresta(real,saovicente,4).
aresta(real,palmeira,7).
aresta(palmeira,saovicente,4).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%-------- Freguesia ----------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Freguesia: #id, nome, Latitude, Longitude -> {V,F}

freguesia(0,gualtar,41.57082270408355,-8.385132957697323).
freguesia(1,adaufe,41.58822016025725,-8.39769439943325).
freguesia(2,saovitor,41.55811981119368,-8.406098832097905).
freguesia(3,nogueiro,41.55064380159762,-8.388788519075284).
freguesia(4,saovicente,41.55643189968566,-8.423919635155087).
freguesia(5,palmeira,41.59702501943494,-8.434789209553655).
freguesia(6,sjsl,41.550581002256486,-8.41930439867474).
freguesia(7,lamacaes,41.546340875881725,-8.394738304278034).
freguesia(8,se,41.55367358737278,-8.428914196332162).
freguesia(9,real,41.5589672332343,-8.44476031167318).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%-------- Cliente ----------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Cliente: #idCliente, idFreguesia -> {V,F}

cliente(1, 0).
cliente(2, 1).
cliente(3, 2).
cliente(4, 3).
cliente(5, 4).
cliente(6, 5).
cliente(7, 2).
cliente(8, 1).
cliente(9, 3).
cliente(10, 9).
cliente(11, 4).
cliente(12, 0).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%-------- Estafeta ---------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Estafeta: #idEstafeta, Pontuacao, Nome -> {V,F}

estafeta(0, 4.1 ,'Daniel').
estafeta(1, 3.9 ,'Nuno').
estafeta(2, 3.5 ,'Guilherme').
estafeta(3, 2.3 ,'Rodrigo').
estafeta(4, 5.0 ,'João').

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%-------- Encomenda --------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Encomenda: #IdEncomenda, Peso, Volume -> {V,F}

encomenda(0, 26.5, 10).
encomenda(1, 5.2, 15).
encomenda(2, 22.1, 30).
encomenda(3, 15.3, 12).
encomenda(4, 22, 16).
encomenda(5, 7.4, 30).
encomenda(6, 10, 19).
encomenda(7, 2, 14).
encomenda(8, 25, 33).
encomenda(9, 3, 12).
encomenda(10, 26, 42).
encomenda(11, 5.5,4).
encomenda(12, 7,11).
encomenda(13, 22, 40).
encomenda(14, 1, 2).
encomenda(15, 7, 4.5).
encomenda(16, 25,55).
encomenda(17, 16, 22).
encomenda(18, 1.5, 2.5).
encomenda(19, 18, 14).
encomenda(20, 5.5, 12). 

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%-------- Entrega --------------- - - - - - - - - - -  -  -  -  -   - 
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Entrega: #Data, Prazo, #IdEntrega, #IdEncomenda, #IdCliente, #IdEstafeta, IdVeiculo, Custo, PontuacaoEntrega  -> {V,F}
entrega(data(12,2,2021), 2.0,  1,  20,  2,1,2,11,4).
entrega(data(3,6,2021),  6.5,  2,  19,  3,2,2,7,3).
entrega(data(28,6,2021), 9.0,  3,  18,  1,4,1,5,4).
entrega(data(1,8,2021),  5.0,  4,  17,  6,3,2,7,2).
entrega(data(5,4,2021),  7.5,  5,  16,  8,0,3,9,4).
entrega(data(1,1,2021),  1.5,  6,  15,  9,2,2,11,3).
entrega(data(5,7,2021),  24.0, 7,  14,  10,0,1,5,2).
entrega(data(12,2,2021), 12.0, 8,  13,  11,1,3,8,4).
entrega(data(29,12,2021),4.5,  9,  12,  5,3,2,9,1).
entrega(data(20,10,2021),3.0,  10, 11,  4,4,2,9,5).
entrega(data(4,4,2021),  5.5,  11, 10,  7,1,3,9,3).
entrega(data(1,7,2021),  0.5,  12, 9,   12,3,1,10,4).
entrega(data(4,6,2021),  6.0,  13, 8,   7,3,3,9,3).
entrega(data(19,11,2021),9.0,  14, 7,   3,0,1,5,4).
entrega(data(7,10,2021), 1.5,  15, 6,   12,4,2,9,5).
entrega(data(5,9,2021),  48.0, 16, 5,   4,1,2,6,5).
entrega(data(22,4,2021), 1.0,  17, 4,   1,2,3,13,3).
entrega(data(19,8,2021), 6.5,  18, 3,   7,4,2,7,4).
entrega(data(9,9,2021),  14.0, 19, 2,   4,0,3,8,5).
entrega(data(27,6,2021), 7.0,  20, 1,   6,1,3,9,1).


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
% Remover predicados

remover(Termo) :- retract(Termo).
remover(Termo) :- assert(Termo), !, fail.


%--------------------------------
% Cálculo dos Custos de Entrega
preco_veiculo(IdV,R) :- IdV==1, R is 2.
preco_veiculo(IdV,R) :- IdV==2, R is 3.
preco_veiculo(IdV,R) :- IdV==3, R is 5.

preco_prazo(Prazo,R) :- Prazo =<2, R=8.
preco_prazo(Prazo,R) :- Prazo >2, Prazo =< 5, R=6.
preco_prazo(Prazo,R) :- Prazo >5, Prazo =< 8, R=4.
preco_prazo(Prazo,R) :- Prazo >8, R=3.

busca_peso(IdEnc,R) :- solucoes(Peso,(encomenda(IdEnc, Peso,_)),List),
                        head(List,R).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%-------- Registos --------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Registar Entrega
registaEntrega(Data, Prazo, IdE, IdEnc, IdC, IdEst,Pe):-
            busca_peso(IdEnc,Peso), det_veiculo(Peso,IdVeiculo), 
            preco_prazo(Prazo,R1),preco_veiculo(IdVeiculo,R2), C is R1+R2,
            inserir(entrega(Data, Prazo, IdE,IdEnc, IdC, IdEst, IdVeiculo, C,Pe)).

% Registar Cliente
registaCliente(IdCliente, IdFreguesia) :- inserir(cliente(IdCliente, IdFreguesia)).

% Registar Estafeta
registaEstafeta(IdEstafeta, Pontuacao, Nome) :- inserir(estafeta(IdEstafeta,Pontuacao, Nome)).

% Registar Encomenda
registaEncomenda(IdEncomenda, Peso, Volume) :- inserir(encomenda(IdEncomenda, Peso, Volume)).

% Registar Freguesia
registaFreguesia(Id, Nome,Latitude,Longitude) :- inserir(freguesia(Id,Nome,Latitude,Longitude)).

% Registar Objetivo
registaGoal(Freguesia) :- inserir(goal(Freguesia)).

% Remover Objetivo
removerGoal(Freguesia) :- remover(goal(Freguesia)).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Mostrar registos
mostrarRegistos(P) :- listing(P).

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
query5(N,R):-solucoes(freguesia(IdF,Nome,Latitude,Longitude),(freguesia(IdF,Nome,Latitude,Longitude),entrega(_, _, _,_, IdCliente, _, _, _,_),cliente(IdCliente, IdF)),LFreg1),
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
lista_de_pontuacoes_estafeta(IdEst,L) :- solucoes(Pe, (estafeta(IdEst,_,_), entrega(_,_,_,_,_,IdEst,_,_,Pe)), L).

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

%Função que pega numa lista de datas de filtras as que pertencem a um determinado intervalo
filtra_lista([],_,_,[]).
filtra_lista([X|T],D1,D2,[X|T2]) :- entre_datas(D1,D2,X),
                            filtra_lista(T,D1,D2,T2).
filtra_lista([X|T],D1,D2,R) :- not(entre_datas(D1,D2,X)), filtra_lista(T,D1,D2,R).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%---------- 9. calcular o número de encomendas entregues e não entregues, num determinado periodo de tempo  -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
query9(data(DD1,MM1,AA1),data(DD2,MM2,AA2),NE,E):-solucoes(D,(entrega(D,_,_,_,_,_,_,_,_)),All),
                                                    comprimento(All,A),
                                                    solucoes(Data,(entrega(Data,_,_,IdEnc,_,_,_,_,_),encomenda(IdEnc,_,_)),Entregues),
                                                    filtra_lista(Entregues,data(DD1,MM1,AA1),data(DD2,MM2,AA2),X2),
                                                    comprimento(X2,E),
                                                    NE is A-E.
                                                


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%---------- 10. calcular o peso total transporte por estafeta num determinado dia  -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
query10(IdEst,data(DD,MM,AA),R) :- solucoes(P,(entrega(data(DD,MM,AA),_,_,IdEnc,_,IdEst,_,_,_),estafeta(IdEst,_,_),encomenda(IdEnc,P,_)),X1),
                            sum_list(X1,R).



%------------------------------------------------------------------
%---------------------------FASE 2---------------------------------
%------------------------------------------------------------------

% Determinar o veiculo a utilizar com base nos indicadores de produtividade
det_veiculo(Peso,Prazo,Distancia,R) :- Peso =<5, determinaTempo(1,Distancia,Peso,T0),
   T0 =< Prazo, R is 1.
det_veiculo(Peso,Prazo,Distancia,R) :- Peso =<5, determinaTempo(2,Distancia,Peso,T0),
   T0 =< Prazo, R is 2.
det_veiculo(Peso,Prazo,Distancia,R) :- Peso =<5, determinaTempo(3,Distancia,Peso,T0),
   T0 =< Prazo, R is 3.

det_veiculo(Peso,Prazo,Distancia,R) :- Peso >5, Peso =< 20, determinaTempo(2,Distancia,Peso,T1),
   T1 =< Prazo, R is 2.
det_veiculo(Peso,Prazo,Distancia,R) :- Peso >5, Peso =< 20, determinaTempo(3,Distancia,Peso,T1),
   T1 =< Prazo, R is 3.

det_veiculo(Peso,_,_,R) :- Peso > 20, Peso =< 100, R is 3.

velocida_veiculo(Id,R) :- Id ==1, R is 10.
velocida_veiculo(Id,R) :- Id ==2, R is 35.
velocida_veiculo(Id,R) :- Id ==3, R is 25.

% Determinar tempo de entrega com base nas restrições
determinaTempo(Veiculo,Distancia,Peso,T) :-
Veiculo == 1, T is (Distancia / 10 - (0.7 * Peso)).
determinaTempo(Veiculo,Distancia,Peso,T) :-
    Veiculo == 2, T is (Distancia / 35 - (0.5 * Peso)).
determinaTempo(Veiculo,Distancia,Peso,T) :-
    Veiculo == 3, T is (Distancia / 25 - (0.1 * Peso)).
    



%---------------------------------------------------------------
%-----------------------QUERIES FASE 2--------------------------
%---------------------------------------------------------------

caminhoK(A,B,P, K) :- caminhoK1(A,[B],P,K).
caminhoK1(A,[A|P1],[A|P1],0).
caminhoK1(A,[Y|P1],P,K1) :- adjacente(X,Y,Ki),
                            nao(membro(X,[Y|P1])),
                            caminhoK1(A,[X,Y|P1],P,K),
                            K1 is K + Ki.

cicloK(A,P, K1) :- 
    adjacente(A,B,Ki),
    caminhoK(A,B,P1,K),
    length(P1,L), L > 1,
    append(P1,[A],P),
    K1 is K + Ki.

%------------------------------------------------
%--------------QUERY1----------------------------
% Gerar os circuitos de entrega, caso existam, que cubram um determinado território (e.g. rua ou freguesia);

fase2Query1(Freguesias,R,L) :- 
    findall(P,cicloK(gualtar,P,_),S),
    filtraCiclo(Freguesias,S,R),
    length(R,L).
    

filtraCiclo(F,[C],[C]):- pertenceTodos(F,C).
filtraCiclo(F,[C],[[]]) :- nao(pertenceTodos(F,C)).
filtraCiclo(F, [C | Cs],[C|T]) :-
    pertenceTodos(F,C),
    filtraCiclo(F,Cs,T).

filtraCiclo(F,[C | Cs], T) :-
    nao(pertenceTodos(F,C)),
    filtraCiclo(F,Cs,T).

pertenceTodos([],_).
pertenceTodos([H|T], L) :-
    pertence(H,L),
    pertenceTodos(T,L).



%--------------------------------------------------------------------
%-----------------------PESQUISAS------------------------------------
%---------------------------------------------------------------------



% PESQUISA NAO-INFORMADA
%-------------------------------------------------------------------
%---------------PROFUNDIDADE PRIMEIRO------------------------------------
%-------------------------------------------------------------------

adjacente(Nodo,ProxNodo,K):- aresta(Nodo,ProxNodo,K).
adjacente(Nodo,ProxNodo,K):- aresta(ProxNodo,Nodo,K).


resolve_pp(Nodo,Destino,R,C):-
    registaGoal(Destino),
    profundidadeprimeiro1(Nodo,[Nodo],Caminho),
    append([Nodo],Caminho,S),
    reverse_list(S,CaminhoInverso),
    removeCabeca(CaminhoInverso,C2),
    append(S,C2,R),
    calculaCusto(R,C),
    removerGoal(Destino).

profundidadeprimeiro1(Nodo,_,[]):-
    goal(Nodo).
profundidadeprimeiro1(Nodo, Historico,[ProxNodo|Caminho]):-
    adjacente(Nodo,ProxNodo,_),
    nao(membro(ProxNodo,Historico)),
    profundidadeprimeiro1(ProxNodo,[ProxNodo|Historico], Caminho).

entrega_encomenta_profundidade(Id,Cam,Dist,Time):-
    findall((Freg,(Peso,Vei)),(encomenda(Id,Peso,_),entrega(_,_,_,Id,IdC,_,Vei,_,_),cliente(IdC,IdF),freguesia(IdF,Freg,_,_)),S),
    extrai_primeiro(S,Dest),
    extrai_segundo(S,Inf),
    colocaList(Inf,Li),
    extrai_primeiro(Li,P),
    extrai_segundo(Li,V),
    resolve_pp(gualtar,Dest,Cam,Dist),
    Aux is Dist/2,
    velocida_veiculo(V,Velo),
    determinaTempo(V,Aux,P,Time1),
    Time is Time1+(Aux/Velo).
    
%-------------------------------------------------------------------
%---------------LARGURA PRIMEIRO------------------------------------
%-------------------------------------------------------------------

calculaCusto([_],0).
calculaCusto([H|T],C) :-
    head(T,Head),
    adjacente(H,Head,K),
    calculaCusto(T,C1),
    C is C1 + K.


resolve_largura(Inicio,Destino, S3,C) :-
    registaGoal(Destino),
    busca_largura( [ [Inicio] ], R),
    reverse_list(R,Solucao),
    removeCabeca(R,S2),
    append(Solucao,S2,S3),
    calculaCusto(S3,C),
    removerGoal(Destino).


busca_largura( [ [N|Caminho] | _], [N|Caminho]):-
    goal(N).
busca_largura([ [N|Caminho]| Caminhos ], Solucao) :-
    bagof([M,N| Caminho], (aresta(N,M,_),not(membro(M,[N|Caminho]))) , NovosCaminhos),
    append(Caminhos, NovosCaminhos, Caminhos1), !,
    (busca_largura(Caminhos1, Solucao); busca_largura(Caminhos, Solucao)).

entrega_encomenta_largura(Id,Cam,Dist,Time):-
    findall((Freg,(Peso,Vei)),(encomenda(Id,Peso,_),entrega(_,_,_,Id,IdC,_,Vei,_,_),cliente(IdC,IdF),freguesia(IdF,Freg,_,_)),S),
    extrai_primeiro(S,Dest),
    extrai_segundo(S,Inf),
    colocaList(Inf,Li),
    extrai_primeiro(Li,P),
    extrai_segundo(Li,V),
    resolve_largura(gualtar,Dest,Cam,Dist),
    Aux is Dist/2,
    velocida_veiculo(V,Velo),
    determinaTempo(V,Aux,P,Time1),
    Time is Time1+(Aux/Velo).


colocaList(A,[A]).

%-------------------------------------------------------------------
%---------------PESQUISA ITERATIVA EM PROFUNDIDADE------------------
%-------------------------------------------------------------------

solve_DFID( Node, Destino,Solucao,C) :- 
    registaGoal(Destino),
    depthFirstIterativeDeepening(Node, R),
    reverse_list(R,S1),
    removeCabeca(R,S2),
    append(S1,S2,Solucao),
    calculaCusto(Solucao,C),
    removerGoal(Destino).


path(Node,Node,[Node]). 
path(FirstNode,LastNode,[LastNode|Path]) :-
path(FirstNode, OneButLast,Path), 
adjacente(OneButLast,LastNode,_),
 \+ member(LastNode,Path). 

 depthFirstIterativeDeepening(Node, Solution) :-
    path(Node,GoalNode,Solution),
    goal(GoalNode).

entrega_encomenta_DFID(Id,Cam,Dist,Time):-
    findall((Freg,(Peso,Vei)),(encomenda(Id,Peso,_),entrega(_,_,_,Id,IdC,_,Vei,_,_),cliente(IdC,IdF),freguesia(IdF,Freg,_,_)),S),
    extrai_primeiro(S,Dest),
    extrai_segundo(S,Inf),
    colocaList(Inf,Li),
    extrai_primeiro(Li,P),
    extrai_segundo(Li,V),
    solve_DFID(gualtar,Dest,Cam,Dist),
    Aux is Dist/2,
    velocida_veiculo(V,Velo),
    determinaTempo(V,Aux,P,Time1),
    Time is Time1+(Aux/Velo).


% PESQUISA INFORMADA
%-------------------------------------------------------------------
%---------------ALGORITMO A* (A ESTRELA)----------------------------
%-------------------------------------------------------------------

solve_astar(Node,Dest ,Path/Cost) :-%adicionar goal
    registaGoal(Dest),
    distancia_entre_freguesias(Node,Dest,Estimate),
    astar(Dest,[[Node]/0/Estimate], RevPath/Cost1/_),
    Cost is 2*Cost1,
    reverse_list(RevPath, Path1),
    removeCabeca(RevPath,S2),
    append(Path1,S2,Path),
    removerGoal(Dest).

astar(_,Paths, Path) :-
    get_best(Paths, Path),
    Path = [Node|_]/_/_,
    goal(Node).
astar(Dest,Paths, SolutionPath) :-
    get_best(Paths, BestPath),
    seleciona(BestPath, Paths, OtherPaths),
    expand_astar(Dest,BestPath, ExpPaths),
    append(OtherPaths, ExpPaths, NewPaths),
    astar(Dest,NewPaths, SolutionPath).

get_best([Path], Path) :- !.
get_best([Path1/Cost1/Est1,_/Cost2/Est2|Paths], BestPath) :-
Cost1 + Est1 =< Cost2 + Est2, !,
get_best([Path1/Cost1/Est1|Paths], BestPath).
get_best([_|Paths], BestPath) :-
get_best(Paths, BestPath).

expand_astar(Dest,Path, ExpPaths) :-
    findall(NewPath, move_astar(Dest,Path,NewPath), ExpPaths).

move_astar(Dest,[Node|Path]/Cost/_, [NextNode,Node|Path]/NewCost/Est) :- 
    adjacente(Node, NextNode, StepCost),
    \+ member(NextNode, Path),
    NewCost is Cost + StepCost,
    distancia_entre_freguesias(NextNode,Dest,Est).

entrega_encomenta_aStar(Id,Cam,Dist,Time):-
    findall((Freg,(Peso,Vei)),(encomenda(Id,Peso,_),entrega(_,_,_,Id,IdC,_,Vei,_,_),cliente(IdC,IdF),freguesia(IdF,Freg,_,_)),S),
    extrai_primeiro(S,Dest),
    extrai_segundo(S,Inf),
    colocaList(Inf,Li),
    extrai_primeiro(Li,P),
    extrai_segundo(Li,V),
    solve_astar(gualtar,Dest,Cam/Dist),
    Aux is Dist/2,
    velocida_veiculo(V,Velo),
    determinaTempo(V,Aux,P,Time1),
    Time is Time1+(Aux/Velo).

%-------------------------------------------------------------------
%----------Predicados que calculam a estima da distancia------------
%-------------------------------------------------------------------
distancia_entre_freguesias(NomeFreguesia1,NomeFreguesia2,L):-
    findall((Lat,Long),(freguesia(Id,NomeFreguesia1,Lat,Long)),D1),
    findall((Lat,Long),(freguesia(Id,NomeFreguesia2,Lat,Long)),D2),
    extrai_primeiro(D1,Lat1), extrai_primeiro(D2,Lat2),
    extrai_segundo(D1,Long1), extrai_segundo(D2,Long2),
    distance(Lat1,Long1,Lat2,Long2,L).

distance(Lat1, Lon1, Lat2, Lon2, Dis):-
    P is 0.017453292519943295,
    A is (0.5 - cos((Lat2 - Lat1) * P) / 2 + cos(Lat1 * P) * cos(Lat2 * P) * (1 - cos((Lon2 - Lon1) * P)) / 2),
    Dis is (12742 * asin(sqrt(A))).

%--------------------------------------------------------
% Extensao do predicado que remove a cabeça a uma lista
removeCabeca([],[]).
removeCabeca([_|T],T).

%--------------------------------------------------------------------
% Reverse list
reverse_list(L,L1):-reverse_list(L,[],L1).
reverse_list([],ACC,ACC).
reverse_list([X|L], ACC,L1):- reverse_list(L,[X|ACC],L1).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao(_).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado membro
membro(X, [X|_]).
membro(X, [_|Xs]):-
	membro(X, Xs).

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

%------------------------
% Obtem primeiro elemento dum tuplo 
extrai_primeiro([(X,_)],X).

%------------------------
% Obtem segundo 
extrai_segundo([(_,X)],X).

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



seleciona(E,[E|Xs],Xs).
seleciona(E,[X|Xs],[X|Ys]) :- seleciona(E,Xs,Ys).