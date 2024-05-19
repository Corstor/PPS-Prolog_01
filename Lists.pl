search(E, cons(E, _)).
search(E, cons(_, T)) :- search(E, T).

position(cons(E, _), zero, E).
position(cons(H, T), s(X), E) :- position(T, X, E).

concat(nil, L, L).
concat(cons(H, T), L, cons(H, O)) :- concat(T, L, O).

search2(X, cons(X, cons(X, _))).
search2(X, cons(_, Xs)) :- search2(X, Xs).

search_two(X, cons(X, cons(_, cons(X, _)))).
search_two(X, cons(_, Xs)) :- search_two(X, Xs).

search_anytwo(X, cons(X, T)) :- search(X, T).
search_anytwo(X, cons(H, T)) :- X \= H, search_anytwo(X, T).

size(nil, zero).
size(cons(_, T), s(O)) :- size(T, O).

sum_list(nil, zero).
sum_list(cons(zero, T), O) :- sum_list(T, O).
sum_list(cons(s(H), T), s(O)) :- sum_list(cons(H, T), O).

greater(s(X), s(Y)) :- greater(X, Y).
greater(s(_), zero).

greater_equal(s(X), s(Y)) :- greater_equal(X, Y).
greater_equal(s(_), zero).
greater_equal(zero, zero).

max(cons(H, T), N) :- max(T, H, N).
max(nil, H, H).
max(cons(H, T), O, Z) :- greater_equal(H, O), max(T, H, Z).
max(cons(H, T), O, Z) :- greater_equal(O, H), max(T, O, Z).

min(cons(H, T), N) :- min(T, H, N).
min(nil, H, H).
min(cons(H, T), O, Z) :- greater_equal(O, H), min(T, H, Z).
min(cons(H, T), O, Z) :- greater_equal(H, O), min(T, O, Z).

min-max(L, Min, Max) :- max(L, Max), min(L, Min).

equal(s(X), s(Y)) :- equal(X, Y).
equal(zero, zero).

same(nil, nil).
same(cons(H1, T1), cons(H1, T2)) :- same(T1, T2).

all_bigger(cons(E1, nil), cons(E2, nil)) :- greater(E1, E2).
all_bigger(cons(H1, T1), cons(H2, T2)) :- greater(H1, H2), all_bigger(T1, T2).

sublist(nil, _).
sublist(cons(H, T), List) :- search(H, List), sublist(T, List).

seq(zero, _, nil).
seq(s(N), E, cons(E, T)) :- seq(N, E, T).

seqR(zero, nil).
seqR(s(N), cons(N, T)) :- seqR(N, T).

put_last(nil, E, L1, L2) :- concat(L1, cons(E, nil), L2).
put_last(cons(E, T), X, L1, L2) :- put_last(T, X, L1, L2).
put_last(nil, X, cons(X, nil)).
put_last(cons(E, T), X, L) :- put_last(T, X, cons(E, T), L).

seqR2(zero, nil).
seqR2(s(N), T) :- seqR2(N, T1), put_last(T1, N, T).

% last(List, E) -> is E the last element of List
last(cons(E, nil), E).
last(cons(E, T), X) :- last(T, X).
% last(cons(a, cons(b, cons(c, nil))), c) -> yes
% last(cons(a, cons(b, nil)), X) -> yes, X / b
% last(X, a) -> X / lists that have a as last element

% map(List, List2) -> List2 is just as List in which every element is increased by 1
map(nil, L1, L1).
map(cons(H, T), L1, L2) :- concat(L1, cons(s(H), nil), Temp), map(T, Temp, L2).
map(L1, L2) :- map(L1, nil, L2).
% map(cons(zero, cons(s(s(zero)), cons(s(zero), nil))), cons(s(zero),cons(s(s(s(zero))), cons(s(s(zero)), nil)))) -> yes
% map(cons(zero, cons(s(zero), nil)), X) -> yes, X / cons(s(zero),cons(s(s(zero)),nil))

% filter(List, List2) -> List2 is a list with all the elements of List1 greater than 0
filter(nil, L1, L1).
filter(cons(H, T), L1, L2) :- greater(H, zero), concat(L1, cons(H, nil), Temp), filter(T, Temp, L2).
filter(cons(H, T), L1, L2) :- greater_equal(zero, H), filter(T, L1, L2).
filter(L1, L2) :- filter(L1, nil, L2).
% filter(cons(zero, cons(s(s(zero)), cons(s(zero), nil))), cons(s(s(zero)), cons(s(zero), nil))) -> yes
% filter(cons(zero, cons(s(zero), nil)), X) -> yes, X / cons(s(zero), nil)

% count(L, N) -> N is the number of element of L that are greater than 0
count(nil, N, N).
count(cons(H, T), E, N) :- greater(H, zero), count(T, s(E), N).
count(cons(H, T), E, N) :- greater_equal(zero, H), count(T, E, N).
count(L, N) :- count(L, zero, N).
% count(cons(zero, cons(s(zero), cons(s(s(zero)), cons(s(zero), nil)))), s(s(s(zero)))) -> yes
% count(cons(zero, cons(s(zero), cons(s(s(zero)), nil))), X) -> yes, X / s(s(zero))

% find(L, E) -> E is the first element greater than 0 inside of L
find(cons(H, T), H) :- greater(H, zero).
find(cons(H, T), E) :- greater_equal(zero, H), find(T, E).
% find(cons(zero, cons(s(s(s(zero))), cons(s(s(zero)), cons(s(zero), nil)))), s(s(s(zero)))) -> yes
% find(cons(zero, cons(s(zero), cons(s(s(zero)), nil))), X) -> yes, X / s(zero)

% dropRight(L, N, L2) -> L2 is the L list without the last N elements
dropRight(cons(_, T), N, L, L) :- size(T, X), greater(N, X).
dropRight(cons(H, T), N, L1, L2) :- size(T, X), greater_equal(X, N), concat(L1, cons(H, nil), Temp), dropRight(T, N, Temp, L2).
dropRight(L, N, L2) :- dropRight(L, N, nil, L2).
% dropRight(cons(zero, cons(s(zero), cons(s(s(zero)), nil))), s(s(zero)), X) -> yes, X / cons(zero, nil)
% dropRight(cons(zero, cons(s(zero), cons(s(s(zero)), nil))),  X, cons(zero,nil)) -> X / s(s(zero))

% dropWhile(L1, L2) -> L2 is the L1 list without the first consecutive elements that are greater than 0
dropWhile(nil, nil).
dropWhile(cons(H, T), cons(H, T)) :- greater_equal(zero, H).
dropWhile(cons(H, T), L) :- greater(H, zero), dropWhile(T, L).
% dropWhile(cons(s(zero), cons(zero ,cons(s(zero), cons(s(s(zero)), nil)))), X). -> yes, X / cons(zero,cons(s(zero),cons(s(s(zero)),nil)))

% partition(L, L1, L2) -> L1 is a list of all elements greater than 0 of L, L2 is the opposite.
% Reuse of filter(L, L1) to get L1
filter_two(nil, L1, L1).
filter_two(cons(H, T), L1, L2) :- greater_equal(zero, H), concat(L1, cons(H, nil), Temp), filter_two(T, Temp, L2).
filter_two(cons(H, T), L1, L2) :- greater(H, zero), filter_two(T, L1, L2).
filter_two(L1, L2) :- filter_two(L1, nil, L2).
partition(L, L1, L2) :- filter(L, L1), filter_two(L, L2).
% partition(cons(s(zero), cons(zero ,cons(s(zero), cons(s(s(zero)), nil)))), X, Y) -> X / cons(s(zero),cons(s(zero),cons(s(s(zero)),nil))), Y / cons(zero,nil)

% reversed(L, L1) -> L1 is the reverse of L
reversed(nil, L, L).
reversed(cons(H, T), L, L1) :- concat(cons(H, nil), L, Temp), reversed(T, Temp, L1).
reversed(L, L1) :- reversed(L, nil, L1).
% reversed(cons(a, cons(b, cons(c, nil))), X) -> X / cons(c,cons(b,cons(a,nil)))
% reversed(X, cons(c,cons(b,cons(a,nil)))) -> X / cons(a, cons(b, cons(c, nil))), loop after
% reversed(X, Y) ->  X / some list, Y / reversed of X

% drop(L, N, L1) -> L1 is the list L without the first N elements
drop(L, N, N, L).
drop(cons(H, T), N, E, L) :- drop(T, N, s(E), L).
drop(L, N, L1) :- drop(L, N, zero, L1).
% drop(cons(a, cons(b, cons(c, nil))), X, Y) -> X / zero, Y / cons(a, cons(b, cons(c, nil))) - X / s(zero), Y / cons(b, cons(c, nil)) - ecc.
% drop(cons(a, cons(b, cons(c, nil))), s(s(zero)), X) -> X / cons(c, nil)

% take(L, N, L1) -> L1 is a list with the first N elements of L
take(_, N, L1, L1) :- size(L1, N).
take(cons(H, T), N, L, L1) :- concat(L, cons(H, nil), Temp), take(T, N, Temp, L1).
take(L, N, L1) :- take(L, N, nil, L1).
% take(cons(a, cons(b, cons(c, nil))), s(s(zero)), Y). -> Y / cons(a,cons(b,nil))
% take(cons(a, cons(b, cons(c, nil))), X, Y). -> X / zero, Y / nil - X / s(zero), Y / cons(a, nil) - ecc.
% take(X, Y, X). -> Y is always the size of X

% zip(L, L1, L2) -> L2 is the list of all combined elements of L1 and L e.g if L = cons(a, cons(b, nil)) and L1 = cons(c, cons(d, nil)) -> L2 = cons((a, c), cons((b, d), nil))
zip(nil, cons(_, _), L2, L2).
zip(cons(_, _), nil, L2, L2).
zip(nil, nil, L2, L2).
zip(cons(H, T), cons(H2, T2), L, L2) :- concat(L, cons((H, H2), nil), Temp), zip(T, T2, Temp, L2).
zip(L, L1, L2) :- zip(L, L1, nil, L2).
% zip(cons(a, cons(b, nil)), cons(c, cons(d, nil)), X). -> X / cons((a,c),cons((b,d),nil))
