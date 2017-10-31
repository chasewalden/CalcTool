%Edward Tunnard, 28203743
%matrix is represened by list of list
%[[1,2,3],[4,5,6],[7,8,9]] is shown as:
%
% 1,2,3
% 4,5,6
% 7,8,9
%
% addMatrix(Matrix1, Matrix2, Rresult)
%True when matrix1 + matrix2 = result
addMatrix([],[],[]).
addMatrix([H1|T1],[H2|T2],[Hr|Tr]):-
    helpAddMatrix(H1,H2,Hr),
    addMatrix(T1,T2,Tr).
% Helper Function for add matrix. It goes through the two lists and adds
% each pair together
helpAddMatrix([],[],[]).
helpAddMatrix([H1|T1],[H2|T2],[Hr|Tr]):-
    helpAddMatrix(T1,T2,Tr), Hr is H1+H2.

% subMatrix(Matrix1, Matrix2, Result).
% True when Matrix1 - Matrix2 = Result.
subMatrix([],[],[]).
subMatrix([H1|T1],[H2|T2],[Hr|Tr]):-
    helpSubMatrix(H1,H2,Hr), subMatrix(T1,T2,Tr).
% Helper funciton for subMatrix. It does through the two given lists and
% uses subtract on each pair
helpSubMatrix([],[],[]).
helpSubMatrix([H1|T1],[H2|T2],[Hr|Tr]):-
    helpSubMatrix(T1,T2,Tr), Hr is H1-H2.


% rowMatrix(Matrix, Position, Row).
% True when Row is the Position-th row in Matrix.
% This is 1 based, so 1 is the 1st position
rowMatrix([H|_],1,H).
rowMatrix([_|T],N,E):-
    rowMatrix(T,N1,E), N is N1+1.

% colMatrix(Matrix, Position, Coloumn).
% True when Coloumn is the Position-th Coloumn in Matrix.
% This is 1 based, so 1 is the 1st position
colMatrix([],_,[]):-!.
colMatrix([H|T],M,[Val|E]):-
    rowMatrix(H,M,Val), colMatrix(T,M,E).

% nthValue(List,Position,Element).
nthValue([H|_],1,H).
nthValue([_|T],N,E):-
    nthValue(T,N1,E), N is N1 + 1.

% dotProductMatrix(Matrix1,Matrix2,Result)
% True when Matrix1.Matrix2=Result
dotProductMatrix([H1|T1],[H2|T2],R):-
    validMatrix([H1|T1]), validMatrix([H2|T2]),
    length(H1,Len1), length(H2,Len2),
    colMatrix([H2|T2],Len2,Col1),
    length(Col1,Len3),
    =(Len1,Len3),
%Above Validates the two matrices
    length(H2,Len),
    helpDotProductMatrix([H1|T1],[H2|T2],R,Len).

%Helper functions for dotProductMatrix
% helpDotProductMatrix(Matrix1, Matrix2, Result, Number_of_rows)
% Iterated over each Row, and calls the Coloumn helper
helpDotProductMatrix([],_,[],_).
helpDotProductMatrix([H|T],M2,[R|Tr],Len):-
    helpDotProductMatrixCol(H,M2,Ri,Len),
    reverseList(Ri,[],R),
    helpDotProductMatrix(T,M2,Tr,Len).
%Helper function: (Row, Matrix2, Result, counter).
% Iterates over each coloum in matrix2 and calls Multiply helper.
helpDotProductMatrixCol(_,_,[],0).
helpDotProductMatrixCol(M1,M2,[R1|R2],Count):-
    colMatrix(M2,Count,Col),
    helpDotProductMatrixMult(M1,Col,R1),
    Count2 is Count-1,
    helpDotProductMatrixCol(M1,M2,R2,Count2).
% helpDotProductMatrixMult(Row, Coloumn, Result).
% multiplies each row and coloumn pair, the result is the sum.
helpDotProductMatrixMult([],[],0).
helpDotProductMatrixMult([H1|R1],[H2|R2],Total):-
    helpDotProductMatrixMult(R1,R2,T2),
    Total is (H1*H2)+T2.


% reverseList(List, List2, Result).
% Result is List in reverse order followed by List2
reverseList([],R,R).
reverseList([H|T],A,R):-
    reverseList(T,[H|A],R).

% validMatrix(Matrix).
% Checks that Matrix is fully populated with data
validMatrix([H|T]):-
    length(H,L),
    colMatrix([H|T],L,_).

% printMatrix(Matrix).
% prints Matrix out with a new line between each row to make it more
% readable
printMatrix([]).
printMatrix([H|T]):-
    write(H),nl,
    printMatrix(T).

% identityMatrix(length of side, result).
% Creates a identity matrix of the given size
identityMatrix(N, Result):-
    ( N > 0),
    helpIdentityMatrix(N,N,Result),!.

% Helper functions for identityMatrix
% helpIdentityMatrix(Size, Counter, Result).
% This calls the row helper and passes which row it is on
helpIdentityMatrix(_,0,[]).
helpIdentityMatrix(N, M, [Row|R]):-
    helpIdentityMatrixRow(N,M,Row),
    M2 is M - 1,
    helpIdentityMatrix(N,M2,R).
% helpIdentityMatrixRow(Length, Position_of_1, Result).
% Creates a list of 0's, appart from when the interation is the same as
% the row its creating then 1,
helpIdentityMatrixRow(0,_,[]).
helpIdentityMatrixRow(N,N,[1|R]):-
    N2 is N - 1,
    helpIdentityMatrixRow(N2,N,R).
helpIdentityMatrixRow(N,M,[0|R]):-
    N2 is N - 1,
    helpIdentityMatrixRow(N2,M,R).


% firstNonZero(List,Pos,Number)
% Find the first non-zero number or false, List is the List, Pos is the
% poition in the List, and Number is the non-zero number.
firstNonZero([0|T],R1,N):-
    firstNonZero(T,R,N),
    R1 is R+1.
firstNonZero([0.0|T],R1,N):-
    firstNonZero(T,R,N),
    R1 is R+1.
firstNonZero([H|_],1,H):-
    not(=(H,0)).

% allZero(List)
% True if the list contains only 0's
allZero([]).
allZero([0.0|T]):-
    allZero(T).
allZero([0|T]):-
    allZero(T).

% multiplyList(List,Multiplier,Result)
% True when Result is List with all values multiplied by Multiplyer
multiplyList([],_,[]).
multiplyList([H|T],Mul,[R1|R]):-
    R1 is H*(Mul),
    multiplyList(T,Mul,R).

% addListMultiples(List1,Mul,List2,Result)
% True when for each value in lists: List1*Mul + List2 = Result
addListMultiples([],_,[],[]).
addListMultiples([H1|T1],Mul,[H2|T2],[Hr|Tr]):-
    Hr is (H1*(Mul))+H2,
    addListMultiples(T1,Mul,T2,Tr).

% reducedEchelon(Matrix, Result).
% True when the Result is the reduced echelon form of Matrix
reducedEchelon(M,R1):-
    length(M,Len),
    Len1 is Len+1,
    helpReducedEchelonLoop(M,1,Len1,R),
    helpZeroToEnd(R,R1),!.
% helper function for reducedEchelon
% reducedEchelon2(Matrix, Matrix, line counter, Result)
% This is the main loop, iterates over each row and calls row helper
helpReducedEchelonLoop(R,C,C,R).
helpReducedEchelonLoop(M,Count,Max,R):-
    rowMatrix(M,Count,NewH),
    helpReducedEchelonRow(M, NewH, Count, NewM),
    Count1 is Count + 1,
    helpReducedEchelonLoop(NewM,Count1,Max,R).
% helpReducedEchelonRow(Matrix, Chosen_row, row_position, Result)
% if the row is all 0's skips
% otherwise sets the first non-zero to 1 and calls zero coloumn
helpReducedEchelonRow(M, NewH, _, M):-
    allZero(NewH).
helpReducedEchelonRow(M, H, Count, NewM):-
    firstNonZero(H,Pos,N),
    N1 is 1/N, multiplyList(H,N1,NewH),
    helpZeroColoum(M,Pos,NewH,Count,NewM).
% zeroColoum(Matrix, Coloumn_position, Chosen_row, Row_position, Result)
% adds multipuls of Choses_row to all other rows in Matrix so that only
% Chosen_row has a non-zero in coloumn_poition
helpZeroColoum([],_,_,_,[]).
helpZeroColoum([_|T],Col,NewH,1,[NewH|R]):-
    helpZeroColoum(T,Col,NewH,0,R).
helpZeroColoum([H|T],Col,NewH,Pos,[NewRow|R]):-
    not(=(Pos,1)),
    nthValue(H,Col,Diff),
    NDiff is -Diff,
    addListMultiples(NewH,NDiff,H,NewRow),
    Pos1 is Pos - 1,
    helpZeroColoum(T,Col,NewH,Pos1,R).
% zeroToEnd(Matrix, Result)
% Moves all rows containing all 0's to the end
helpZeroToEnd(M,R):- helpZeroToEnd2(M,[],R).
helpZeroToEnd2([],H,H).
helpZeroToEnd2([H|T],A,R):-
    allZero(H),
    helpZeroToEnd2(T,[H|A],R).
helpZeroToEnd2([H|T],A,[H|R]):-
    not(allZero(H)),
    helpZeroToEnd2(T,A,R).


% invertMatrix(Matrix, Result).
% True when Result is the inverse of Matrix
invertMatrix(M,R):-
    length(M,Len),
    identityMatrix(Len,IM),
    augmentMatrix(M,IM,Aug),
    reducedEchelon(Aug,Ech),
    augmentMatrix(IM,R,Ech),!.

% augmentMatrix(Left, Right, Result)
% Augments Left matrix with Right matrix so:
% [Left|Right] = Result
% a custom append is used because 0 \= 0.0
augmentMatrix([],[],[]).
augmentMatrix([H1|T1],[H2|T2],[Hr|Tr]):-
    appendList(H1,H2,Hr),
    augmentMatrix(T1,T2,Tr).


% appendList(List1, List2, Result).
% A custom append so that float values are seen as the same as their
% interger values.
appendList([],L,L).
appendList([H|T],L,[H|R]):-
    appendList(T,L,R).
appendList([H1|T],L,[Hr|R]):-
    H2 is float(H1),
    H2 is float(Hr),
    appendList(T,L,R).
