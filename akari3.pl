:-dynamic size/2.

:-dynamic wall/2.
:-dynamic wall_num/3.
:-dynamic light/2.
:-dynamic emptyCell/2.
:-dynamic prohibited/2.

:-dynamic ups/4.
:-dynamic lefts/4.
:- dynamic rights/4.
:- dynamic downs/4.


neighbour(X,Y,N,M):-((X is N-1,Y is M) , (emptyCell(X,Y);light(X,Y))) ; ((X is N+1,Y is M),(emptyCell(X,Y);light(X,Y))) ; ((X is N,Y is M-1),(emptyCell(X,Y);light(X,Y))) ;( (X is N,Y is M+1), (emptyCell(X,Y);light(X,Y))).

neighboursOfCell(N,M,Z):- setof([X,Y],neighbour(X,Y,N,M),Z).


upCells(N,M,Z):-upCell(N,M,N,M),setof((emptyCell(X,Y);light(X,Y)),ups(X,Y,N,M),Z).
upCell(N,M,W,V):-(X is N-1, Y is M , (emptyCell(X,Y);light(X,Y)))->(assert(ups(X,Y,W,V)),upCell(X,Y,W,V));true.


downCells(N,M,Z):-downCell(N,M,N,M),setof((emptyCell(X,Y);light(X,Y)),downs(X,Y,N,M),Z).
downCell(N,M,W,V):-(X is N+1, Y is M , (emptyCell(X,Y);light(X,Y)))->(assert(downs(X,Y,W,V)),downCell(X,Y,W,V));true.


leftCells(N,M,Z):-leftCell(N,M,N,M),setof((wall(X,Y);light(X,Y)),lefts(X,Y,N,M),Z).
leftCell(N,M,W,V):-(X is N, Y is M-1 , (emptyCell(X,Y);light(X,Y)))->(assert(lefts(X,Y,W,V)),leftCell(X,Y,W,V));true.


rightCells(N,M,Z):-rightCell(N,M,N,M),setof((emptyCell(X,Y);light(X,Y)),rights(X,Y,N,M),Z).
rightCell(N,M,W,V):-(X is N, Y is M+1 , (emptyCell(X,Y);light(X,Y)))->(assert(rights(X,Y,W,V)),rightCell(X,Y,W,V));true.

allCells(N,M,Z):-upCell(N,M,N,M),downCell(N,M,N,M),leftCell(N,M,N,M),rightCell(N,M,N,M),setof([X,Y],(lefts(X,Y,N,M);rights(X,Y,N,M);downs(X,Y,N,M);ups(X,Y,N,M)),Z).


lenLight([],0).
lenLight([H|T],N) :- lenLight(T,X),((isLight(H))-> N is X+1; N is X).

isLight([N,M|_]):-light(N,M).

isLighted(X,Y):-light(X,Y);(emptyCell(X,Y),allCells(X,Y,Z),lenLight(Z,B),B > 0).

cellLightNum(X,Y,N,Z):-allCells(X,Y,Z),lenLight(Z,N).
wallNumLightNum(X,Y,N,Z):-neighbours(X,Y,Z),lenLight(Z,N),wall_num(X,Y,B),B=:=N.
wallNumRight(X,Y):-wallNumLightNum(X,Y,_,_).

allIsLighted:-emptyCell(X,Y),(isLighted(X,Y)->fail;true).
allLightsAlone:-light(X,Y), allCells(X,Y,Z),lenLight(Z,B),(B=:=0->fail;true).
allWallNumRight:-wall_num(X,Y,_), (wallNumRight(X,Y)->fail;true).

checkBoard :- \+allIsLighted,\+allLightsAlone,\+allWallNumRight.

printBoard(X,Y):- (size(M,N),P is N+1,(X is M,Y is P)->nl,nl,fail;(size(M,N),P is N+1,(Y is P)->(A is X+1,B is 1,nl);(A is X, B is Y))),(emptyCell(A,B)->write('- ');(wall(A,B)->write('W ');(wall_num(A,B,C)->write(C),write(' ');(light(A,B)->write('* '))))),K is B+1,printBoard(A,K).


solve:- wall_num(X,Y,Z)->((Z is 4)->(neighbour(X,Y,A,B),assert(light(A,B)));(Z is 0)->(neighbour(X,Y,A,B),assert(prohibited(A,B)));true),backtrack.

backtrack:- printBoard(1,1);
	checkBoard->true;
	emptyCell(X,Y),
	(   \+isLighted(X,Y),\+prohibited(X,Y))->
	assert(light(X,Y)),
	    retract(emptyCell(X,Y)),
	 solve->true;
	assertz(emptyCell(X,Y)),
	    retract(light(X,Y)),fail.


