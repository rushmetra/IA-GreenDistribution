
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
% Estafeta: #idEstafeta, Nome -> {V,F}

estafeta(0, 4.1 ,'Daniel').
estafeta(1, 3.9 ,'Nuno').
estafeta(2, 3.5 ,'Guilherme').
estafeta(3, 2.3 ,'Rodrigo').

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
% Entrega: #Data, Prazo, #IdEntrega, #IdCliente, #IdEstafeta, Pontuacao, MeioTransporte, Custo  -> {V,F}
entrega(data(13, 12, 2021), 0, 1, 2, 4, 'Bicicleta',  19.99). % # 0 = imediato
entrega(data(13, 12, 2021), 24.0, 1, 2, 4, 'Bicicleta',  19.99).
entrega(data(16, 11, 2021), 2.0, 2, 9, 3, 'Mota', 29.99).
entrega(data(18, 09, 2021), 6.0, 3, 5, 2, 'Carro', 39.99).
entrega(data(19, 05, 2021), 16.0, 4, 8, 4, 'Bicicleta', 19.99).




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

getEntregasEstafeta(estafeta(Id, _,_), [entrega(_, _, _, _, IdEstafeta, _,  _)|T], R) :-
					Id == IdEstafeta,
					append(R,[entrega(_, _, _, _, IdEstafeta, _,  _)], R1)
					getEntregasEstafeta(estafeta(Id, _,_), T, R1).



calcularEcoEstafeta(Estafeta, [NB, NM|T]):- 
		
		




identificarEstafetaEco():-
	
		NB is calculaBicicleta(Estafeta, [Entrega|T]),
		NM is calculaMota(Estafeta, [Entrega|T]),
		Sol is NB + NM.
			


calculaBicicleta(Estafeta, [Entrega|T], Sol):-

calculaMota(Estafeta, [Entrega|T], Sol):-



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Concatenar uma lista
append([ ], L, L).
append([H|L1], L2, [H|L3]):- append(L1, L2, L3).

















