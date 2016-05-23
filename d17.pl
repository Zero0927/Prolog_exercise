%Siyuan Zhou(Scott)
contains(katarina, olga).
contains(olga, natasha).
contains(natasha, irina).

in(X, Y):- contains(X, Y).
in(X, Y):- contains(X, Z),in(Z,Y).

directTrain(saarbruecken,dudweiler). 
directTrain(forbach,saarbruecken). 
directTrain(freyming,forbach). 
directTrain(stAvold,freyming). 
directTrain(fahlquemont,stAvold). 
directTrain(metz,fahlquemont). 
directTrain(nancy,metz).

travelFromTo(X, Y):- directTrain(X, Y).
travelFromTo(X, Y):- directTrain(Y, X).
travelFromTo(X, Y):- directTrain(X, Z), travelFromTo(Z,Y).
travelFromTo(X, Y):- directTrain(Z, X), travelFromTo(Y,Z).

second(X,[_,X|_]).

swap12([],[]).
swap12([X, Y | List], [Y, X | List]).

tran(eins,one). 
tran(zwei,two). 
tran(drei,three). 
tran(vier,four). 
tran(fuenf,five). 
tran(sechs,six). 
tran(sieben,seven). 
tran(acht,eight). 
tran(neun,nine).

listtran([], []).
listtran([X | A], [Y | B]) :-
  tran(X, Y),
  listtran(A, B).

pway(a, b, 10).
pway(b, c, 15).
pway(d, c, 5).
pway(d, b, 10).

% If the user types solve(a, b, P, N) then 
% P = [a, b]
% N = 10
% If the user types solve(a, d, P, N) then 
% P = [a, b, c, d]
% N = 30;
% P = [a, b, d]
% N = 20

solve(X,Y,P,N) :- solve(X,Y,[X],N,P).
solve(X,X,P,0,P).
solve(X,Y,P,N,O) :-
    pway(X,E,C),
    not(member(E, P)),
    solve(E,Y,[E|P],M,O),N is M+C.
solve(X,Y,P,N,O) :-
    pway(E,X,C),
    not(member(E, P)),
    solve(E,Y,[E|P],M,O),N is M+C.

	