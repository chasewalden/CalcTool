
mk_string([], "").
mk_string([S], S).
mk_string([S|SL], O) :- mk_string(SL, O1), string_concat(S, O1, O).

/*OPERATIONS*/

simplify(A+B,R):-add(A,B,R).
simplify(A*B,R):-multiply(A,B,R).
simplify(A/B,R):-divide(A,B,R).
simplify(A-B,R):-sub(A,B,R).

/*add*/
add(X,0,X).
add(0,X,X).
add(X,X,2*X).
add(X,C*X,R):-

    atomic(C),
   add(C,1,C1),
  R is C1*X.
add(C*X,X,R):-
    atomic(C),
   add(C,1,C1),
   R is C1*X.
add(B*X,C*X,R):-
    atomic(C),
    atomic(B),
   add(C,B,C1),
   R is C1*X.
add(X,Y,R):-
   number(X),
   number(Y),
   R is X+Y.


add(X,Y,X+Y).
/*subtract*/
sub(X,0,X).
sub(0,X,-X).
sub(X,C*X,R):-
    atomic(C),
   sub(1,C,C1),
   multiply(C1,X,R).
sub(C*X,X,R):-
    atomic(C),
   sub(C,1,C1),
   multiply(C1,X,R).
sub(B*X,C*X,R):-
    atomic(C),
    atomic(B),
   sub(B,C,C1),
   multiply(C1,X,R).
sub(X,Y,R):-
   number(X),
   number(Y),
   R is X-Y.
sub(X,X,0).
sub(X,Y,X-Y).
/*multiply*/
multiply(0,_,0).
multiply(_,0,0).
multiply(1,X,X).
multiply(X,1,X).
multiply(X,Y,X):-X==1.
multiply(X,C*X,C*X*X):-
   atomic(C).
multiply(C*X,X,C*X*X):-
   atomic(C).
multiply(B*X,C*X,R):-
   atomic(B),atomic(C),
   multiply(B,C,A),
    R is A*X*X.
multiply(X,Y,R):-
   number(X),
   number(Y),
   R is X*Y.
multiply(A,B,A*B).

/*divison operation*/

divide(X,0,R):-nowrite("Division by zero error!"),!,fail.
divide(0,X,0).
divide(X,X,1).
divide(X,Y,1):-X==Y.
divide(X,Y,R):-
   number(X),
   number(Y),
   R is X/Y.
divide(X,Y,X/Y).


/*Differentiation functions*/

derive( X, X, 1 ).                /* d(X) w.r.t. X is 1      */

derive( C, X, 0 ):- atomic(C).          /* If C is a constant then */
                                   /* d(C)/dX is 0            */
/*not the same variable e=e2*/
derive(E,E2,0):-
   atom(E),
   E \==E2.

derive( U + V, X, R ):-                 /* d(U+V)/dX = A+B where
                     A = d(U)/dX and         */
   derive( V, X, E2 ),
   derive( U, X, E1 ),
   nowrite("U="),nowrite(U),nonl,nowrite("V="),nowrite(V),nonl,
   nowrite("Applying the rule d(U+V)/dX = A+B where A = d(U)/dX and       B=d(V)/dX)"),nonl,
   add(E1,E2,R11),
   simplify(R11,R),
   nowrite("Result of the step="),nowrite(R),nonl.

derive( U-V, X, R ):-
   derive( U, X, E1),
   derive( V, X, E2 ),
   nowrite("U="),nowrite(U),nonl,nowrite("V="),nowrite(V),nonl,
    nowrite("Applying the rule d(U-V)/dX = A-B where A = d(U)/dX and       B=d(V)/dX)"),nonl,
   sub(E1 , E2,R11),
   simplify(R11,R),
   nowrite("Result of the step="),nowrite(R),nonl.

/* constant multiplied to variable d(C*X)/d(x) */
derive( C*U, X, R ):-
   atomic(C),
   C \= X,
   derive( U, X,E1 ),
   nowrite("U="),nowrite(U),nonl,nowrite("C="),nowrite(C),nonl,
    nowrite("Applying the rule d(C*U)/dX =C*dU/dX"),nonl,
   multiply(C,E1,R11),
   simplify(R11,R),
   nowrite("Result of the step="),nowrite(R),nonl,
   !.

derive( U*V, X, R):-           /* d(U*V)/dX = B*U+A*V where */
   derive( U, X, E1 ),                 /* A = d(U)/dX and           */
   derive( V, X, E2 ),
   nowrite("U="),nowrite(U),nonl,nowrite("V="),nowrite(V),nonl,
   nowrite("Applying the formula d(U*V)/dX = B*U+A*V"),nonl,
   nowrite("A=dU/dx="),nowrite(E1),nonl, nowrite("B=dV/dx="),nowrite(E2),nonl,
   /*U*E2+V*E1 */
   multiply(U,E2,R1),
   multiply(V,E1,R2),
   add(R1,R2,R11),
   simplify(R11,R),
   nowrite("Result of the step="),nowrite(R),nonl.

/* B = d(V)/dX               switch to R is e1+e2*/

derive( U/V, X, R ):- /* d(U/V)/dX = (A*V-B*U)/(V*V) */
   derive( U, X, A),                /* where A = d(U)/dX and       */
   derive( V, X, B),
   nowrite("U="),nowrite(U),nonl,nowrite("V="),nowrite(V),nonl,
   nowrite("Using division rule-d(U)/dx="),nowrite(A),nowrite(" d(V)/dx="), nowrite(B),nonl,
   multiply(A,V,R1),
   multiply(B,U,R2),
   sub(R1,R2,R3),
   divide(R3,V,R4),
   nowrite("Applying the formula  d(U/V)/dX = (A*V-B*U)/(V*V)"),nonl,
   divide(R4,V,R11),
   simplify(R11,R),
   nowrite("Result of the step="),nowrite(R),nonl.



derive(log(U), X, R):-
   derive(U,X,R1),
   nowrite("U="),nowrite(U),nonl,
   divide(R1,U, R11),
   simplify(R11,R),
   nowrite("Applying the rule d(ln(U))/dx=(dU/dx)/U."),nonl,
   nowrite("Result of the step="),nowrite(R),nonl.


derive(sin(U),X,R):-
   derive(U,X,R1),
    nowrite("U="),nowrite(U),nonl,
   nowrite("Applying the rule d(sin(U))/dx=cos(U)*dU/dx."),nonl,
   multiply(cos(U),R1,R11),
   simplify(R11,R),
   nowrite("Result of the step="),nowrite(R),nonl.

derive(cos(U), X, R):-
   derive(U,X,R1),
    nowrite("U="),nowrite(U),nonl,
   nowrite("Applying the rule d(cos(U))/dx=-sin(U)*dU/dx."),nonl,
   multiply(sin(U),R1,R2),
   sub(0, R2, R11),
   simplify(R11,R),
   nowrite("Result of the step="),nowrite(R),nonl.

derive(tan(U), U, R):-
   derive(U, X, R1),
    nowrite("U="),nowrite(U),nonl,
   nowrite("Applying the rule d(tan(U))/dx=cos(U)^2*dU/dx."),nonl,
   multiply(cos(U), cos(U), R2),
   divide(1,R2, R3),
   multiply(R3, R1, R11),
   simplify(R11,R),
   nowrite("Result of the step="),nowrite(R),nonl.

/*d/dx( f(x)^g(x) ) = f(x)^g(x) * d/dx( g(x) ) * ln( f(x) )
                     + f(x)^( g(x)-1 ) * g(x) * d/dx( f(x) )*/
evaluate(U^V,U):-
   V==1.
evaluate(U^V,U^V).

derive(U ^ V,X,R):-
   derive(U,X,R1),

   derive(V,X,R2),
    nowrite("U="),nowrite(U),nonl,nowrite("V="),nowrite(V),nonl,
   evaluate(U^V,Z),
   multiply(Z, R2,R3),

   multiply(R3, log(U), R4),

   sub(V,1,R5),
   evaluate(U^R5, Z2),

   multiply(Z2, V,R6),

   multiply(R6, R1, R7),

   add(R4, R7, R11),
   simplify(R11,R),
   nowrite("Applying the rule-d/dx( f(x)^g(x) ) = f(x)^g(x) * d/dx( g(x) ) * ln( f(x) ) + f(x)^( g(x)-1 ) * g(x) * d/dx( f(x) )"),
   nowrite("Result of the step="),nowrite(R),nonl.


/*Integration*/
integrate(0 , X , 0).

integrate(C , X , C * X ):-
        atomic(C),
        C \= X,  !.

integrate(X, X , 0.5 * X^2 ):- !.

integrate(1 / X , X , log(X)):- !.

integrate(C * X , X , C * R):-
        atomic(C),
        integrate(X , X , R).

integrate(X * C , X , R):-
        atomic(C),
        integrate(X , X , R).

integrate(U + V , X , R):-
        integrate( U , X , A ),
        integrate( V , X , B ),
        add(A,B,R).

integrate(U - V , X , R):-
        integrate( U , X , A ),
        integrate( V , X , B ),
        sub(A,B,R).

integrate(U * V , X , R):-
        atomic(U),
        U \= X,
        integrate(V , X , M),
        multiply(U,M,R).

integrate(B^X , X , B^X / log(B) ):-
        atomic(B),
        B \= X,
        !.

integrate(X ^ N , X , R):-
        atomic(N),
        add(N,1,W),
        divide(1,W,W1),
        evaluate(X^W,R1),
        multiply(W1,R1,R).
/* (1 / (N + 1) ) * X ^ (N + 1)*/
integrate( X^N , X ,R ):-
        atomic(N),
        add(N,1,N1),
        divide(1,N1,E1),
        evaluate(X^N1, E2),
        multiply(E1,E2,R),
        !.

integrate( U / X , X , R ):-
        atomic(U),
        U \= X,
        multiply(U,log(X),R),
        !.

integrate(( (A*X+B)^(-1) ) * X , X , R):-nowrite("Divide by zero error!"),!,fail.

integrate(( (A*X+B)^N ) * X , X, R):-
        add(N,1,N1),
        evaluate((A*X+B)^N1,E1),
        multiply(A,N1,E2),
        divide(E1,E2,R).

integrate( exp(X) , X , exp(X) ):- !.
/* ln cxdx= x* ln cx- x */
integrate(log(C*X),X,R):-
   atomic(C),
   multiply(X,log(C*X),R1),
   sub(R1,X,R).

integrate(log(X)^N,X,R):-
   atomic(N),
   add(N,1,N1),
   divide(log(X)^N1, N1,R).

integrate(1/(A*X+B) , X , (1/A) * log(A*X+B)):- !.

integrate(exp(A*X+B) , X , (1/A) * exp(A*X+B)):- !.


integrate(sin(A*X+B), X , -(1/A) * cos(A*X+B)):- !.

integrate( cos(A*X+B),X , (1/A) * sin(A*X+B)):- !.

integrate(sin(X), X ,-cos(X)):- !.

integrate(cos(X), X , sin(X)):- !.

integrate(tan(X), X , log(cos(X))):- !.

integrate(sec(X), X , log(sec(X)+tan(X))):- !.

integrate(cosec(X), X , log(cosec(X)-cot(X))):-!.

integrate(cot(X), X , log(sin(X)) ):- !.

integrate(sec(X)^2, X , tan(X) ):- !.

integrate(cosec(X)^2, X , -cot(X) ):- !.

integrate(sec(X) * tan(X), X , sec(X) ):- !.

integrate(cosec(X) * cot(X), X , -cosec(X) ):- !.

integrate(1/(sin(X)*cos(X)), X, log(tan(X))).

integrate(cos(X)^3,X, sin(X)-(1/3)*sin(X)^3).
integrate(sin(X)^3,X,(1/3)*cos(X)^3-cos(X)).
integrate(sin(A*X),X,-(1/A)*cos(A*X)).









































