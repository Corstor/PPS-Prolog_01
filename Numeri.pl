sum(zero, N, N).
sum(s(N), X, s(S)) :- sum(N, X, S).

dec(s(X), X).

greater(s(X), s(Y)) :- greater(X, Y).
greater(s(_), zero).

nextprev(s(X), X, s(s(X))).