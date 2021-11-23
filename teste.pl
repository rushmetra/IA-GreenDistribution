




%  teste([1,2,3],S). 

calculaHead([Head|T], Sol):-
		Sol is Head.


teste([Head1|T], Boda, Sol):-
		calculaHead([Head1|T], Sol).
		Boda is 1.


estafeta(0, 4.1 ,'Daniel').
estafeta(1, 3.9 ,'Nuno').
estafeta(2, 3.5 ,'Guilherme').
estafeta(3, 2.3 ,'Rodrigo').
estafeta(4, 5.0,'Nunão').

entrega( 0, 1, 2, 4, 'Bicicleta',  19.99). % # 0 = imediato
entrega( 24.0, 1, 2, 4, 'Bicicleta',  19.99).
entrega( 2.0, 2, 9, 3, 'Mota', 29.99).
entrega( 6.0, 3, 5, 2, 'Carro', 39.99).
entrega( 16.0, 4, 8, 4, 'Bicicleta', 19.99).


getEntregasEstafeta(estafeta(Id, _,_), [entrega(_, _, _, IdEstafeta, _,  _)|T], R) :-
					Id == IdEstafeta,
					append(R,[entrega( _, _, _, IdEstafeta, _,  _)], R1)
					getEntregasEstafeta(estafeta(Id, _,_), T, R1).







%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Concatenar uma lista
append([ ], L, L).
append([H|L1], L2, [H|L3]):- append(L1, L2, L3).

