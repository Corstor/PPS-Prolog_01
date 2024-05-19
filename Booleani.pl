my_not(my_true, false).
my_not(false, my_true).
my_and(false, _, false).
my_and(my_true, X, X).
my_or(X, Y, O) :- my_not(X, NX), my_not(Y, NY), my_and(NX, NY, AO), my_not(AO, O).