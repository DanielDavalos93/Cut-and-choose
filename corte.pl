/*
 
 --------- Cut & Choose --------

ui(p1,p2,p3) representa la valoración de cada jugador i=1,2 para la elección de la porción respectivamente
A cada jugador se le pregunta qué tanto le gusta cada sabor en una escala del 0 al 10
## Tabla de preferencias ##
Jugador1 : u([3,5,8]).
Jugador2 : u([6,0,10]).
*/

% ### Cláusulas auxiliares ###
% Divide una lista en head (H) y tail (T)
partir([H|T],[H],T).

% Dada una lista L se obtiene la suma Res como resultado
suma([],0).
suma(L,Res):- partir(L,[H],T), suma(T,R), Res is R+H,!.

posicion(L,0,H):- partir(L,[H],_),!.
posicion(L,N,R):- N1 is N-1, partir(L,_,T), posicion(T,N1,R).

maxima_lista([N],N).
maxima_lista([N1,N2],Max):- (N1 >= N2 -> Max = N1 ; Max = N2 ).
maxima_lista(L,Max):- length(L1,2), append(L1,L2,L), maxima_lista(L1,M), maxima_lista([M|L2],Max),!.

% Tamaños aleatorios: T es el tamaño de la torta y N es la Iteración
leer(T,N):- random_between(1,20,T),random_between(3,20,N).

agregar_sabor(T,[H|T]):- random_between(1,3,H).

normalizar(u([V1,V2,V3]),N):- S is V1+V2+V3, P1 is V1/S, P2 is V2/S, P3 is V3/S, N = [P1,P2,P3].

gen(N, L, Torta) :-
    ( N==0 -> Torta = L;
      N1 is N -1, agregar_sabor(L,L1), gen(N1,L1, Torta)
    ).

% ### Tabla de pagos normalizados ###
u1(N):- normalizar(u([3,5,8]),N).
u2(N):- normalizar(u([6,0,10]),N).

/* si Torta=[1,2,1,3]
 Opcion1: [] [1,2,1,3]
 mu1([]) = (0)          mu1([1,2,1,3])=1.1875
 mu2([]) = 0          mu2([1,2,1,3]) = (1.375)

 Opcion2: [1] [2,1,3] 
 mu1([1]) = (0.1875)    mu1([2,1,3]) = 1 
 mu2([1]) = 0.375     mu2([2,1,3]) = (1) 

 Opcion3: [1,2] [1,3]                         <---
 mu1([1,2])=(0.5)       mu1([1,3]) = 0.6875 
 mu2([1,2]) = 0.375   mu2([1,3]) = (1)

 Opcion4: [1,2,1] [3]                         <---
 mu1([1,2,1]) = 0.6875  mu1([3]) = (0.5)
 mu2([1,2,1]) = (0.75)    mu2([3]) = 0.625
 
 Opcion5: [1,2,1,3] []
 mu1([1,2,1,3]) = 1.1875  mu1([]) = (0)
 mu2([1,2,1,3]) = (1.375)   mu2([]) = 0 */

% La idea de esta parte del código es cortar la torta en cualquier par de trozos (P1,P2) ⊆ (P(N),P(N)), 
% donde P1∪P2 = Torta, es decir P1 y P2 son subconjuntos disjuntos del conjunto de partes de N y se 
% obtiene la suma de sus respectivos pagos S1 = mu1(P1)=mu1(P11)+...+mu1(P1T) y S2 = mu2(P21)+...+mu2(P2T). 
% Al hacer la consulta en prolog, tenemos 2^T soluciones

suma_de_valores(_,[],0).
suma_de_valores(N,P,S):- partir(P,[H],T), 
  posicion(N,0,V1), posicion(N,1,V2), posicion(N,2,V3), 
  suma_de_valores(N,T,Si),
  (
    H == 1 -> S is V1+Si;
    H == 2 -> S is V2+Si;
    H == 3 -> S is V3+Si
  ).

elegir(Torta,(P1,P2),(S1P1,S1P2),(S2P1,S2P2)):- u1(N1),u2(N2), append(P1,P2,Torta), 
  suma_de_valores(N1,P1,S1P1),
  suma_de_valores(N1,P2,S1P2), 
  suma_de_valores(N2,P1,S2P1),
  suma_de_valores(N2,P2,S2P2).

/*
 De entre estos posibles pares hay que elegir las que satisface que: 
 (i) J2 elija la parte que más le convenga
 (ii) Como J1 corta debe beneficiarse (S1P1>=1/2 y S2P2>S1P2 ; o S1P2>=1/2 y S2P1>S2P2).
## Construir el mayor de las respuestas posibles ##
  Se elige la mejor elección para el jugador 2, pero si la partición restante para el jugador 1 es 
  mayor o igual que la parte elegida, siempre que sea posible. 
  Es importante que la partición restante para el jugador 1 no sea vacío.
*/

mejor_eleccion(L,(P1,P2),S1,S2):- 
  elegir(L,(P1,P2),(S1P1,S1P2),(S2P1,S2P2)), 
  length(P1,L1), length(P2,L2),
  (                                                           
    S2P1 >= S2P2, (L2 >= L1 ), P2 \= [] -> S1 = S1P2, S2 = S2P1; 
    S2P1 >= S2P2, P2 \= [] -> S1 = S1P2, S2 = S2P1;
    S2P1 < S2P2, (L1 >= L2 ), P1 \= [] -> S1 = S1P1, S2 = S2P2
    % S2P1 < S2P2, P1 \= [] -> S1 = S1P1, S2 = S2P2
  ),!.

ciclo(T,N,L_ant1,L_ant2):- T>=0,I is T-1, gen(N,[],Torta),J is N-T,
  write('Iteración '),write(J),write(': '),write(Torta),nl,
  mejor_eleccion(Torta,(P1,P2),S1,S2),
  write('Elección J1: '),write(P1),write('  Elección J2: '),write(P2),nl,
  write('Beneficio J1 = '),write(S1),write('  Beneficio J2 = '),write(S2),nl,
  append([S1],L_ant1,L1),suma(L1,R1),append([S2],L_ant2,L2),suma(L2,R2),
  write('Suma de ganancias: J1 = '),write(R1),write('  J2 = '),write(R2),nl,
  write('-----------------------'),nl,
  ciclo(I,N,L1,L2).

% Aquí se lanza el programa que contabiliza las ganancias de ambos jugadores
% donde el tamaño de la torta (N) y la iteración (T) son aleatorios. 

go:- leer(T,N),nl,
  write('T = '),write(T),write('   N = '),write(N),nl,
  L1=[],L2=[],    % Inicializa en vacío
  ciclo(T,N,L1,L2),!.
