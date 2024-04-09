:- module(middle_snake, [
        middle_snake/4
      ]).
:- use_module(library(clpfd)).
:- use_module(library(lists)).

% Util

% GOOD
% init_list_except_nth0(+Length, +Index, +Value, -List)
% Initializes a list of given length with the specified value at the given index, and zeros elsewhere.
init_list_except_nth0(Length, Index, Value, List) :-
  PrevL is Index, PostL is Length-Index-1,
  length(Prev, PrevL),
  maplist(zero, Prev),
  length(Post, PostL),
  maplist(zero, Post),
  append(Prev, [Value|Post], List).
zero(0).

%% Helper Functions: 

% Good
% Diagonal(K) to coordinates. k = X+Y
diagonal_to_xy(K,X,Y) :- 
  Y #= X - K.

% Good
% Retrieves the K+offset-th element of List and unifies it with X.
get_fr(Offset, K, FRArray, X) :- 
  I #= K+Offset, 
  lists:nth0(I, FRArray, X).

% Good
% Diagonal(K) to furthest reaching point(FR) of neigboring diagonal on D differences search 
% This is called "making a snake" (turn right or turn down when forwards, up or left when backwards)
% The result coordinate is retreived from the FR Array
diagonal_to_fr(Offset, K, D, FRArray, X) :-
  K #= 0 - D,
  KPlus #= K+1,
  get_fr(Offset, KPlus, FRArray, X).
diagonal_to_fr(Offset, K,D,FRArray, X) :-
  K #\= D, 
  KPlus #= K+1, KMinus #= K-1,
  get_fr(Offset, KPlus, FRArray, KRight),
  get_fr(Offset, KMinus, FRArray, KLeft),
  KLeft < KRight,
  X is KRight.
diagonal_to_fr(Offset, K,_,FRArray, X) :-
  KMinus = K-1,
  get_fr(Offset, KMinus, FRArray, KLeft),
  X is KLeft+1.

diagonal_to_fr_reverse(Delta,Offset, K, D, FRArray, X) :-
  K #= Delta - D,
  KMinus #= K - 1,
  get_fr(Offset, KMinus, FRArray, X).

diagonal_to_fr_reverse(Delta,Offset, K, D, FRArray, X) :-
  K #\= Delta + D, 
  KPlus #= K + 1, KMinus #= K - 1,
  get_fr(Offset, KPlus, FRArray, KRight),
  get_fr(Offset, KMinus, FRArray, KLeft),
  KRight < KLeft, % Note the condition change to reflect reverse direction
  X is KLeft.

diagonal_to_fr_reverse(Delta,Offset, K, _, FRArray, X) :-
  KPlus #= K + 1,
  get_fr(Offset, KPlus, FRArray, KRight),
  X is KRight + 1.

% diagonal_to_fr_reverse(Offset, Delta, K, D, FRArray, X) :-
%   K #= Delta - D,
%   KPlus #= K + 1,
%   get_fr(Offset, KMinus, FRArray, X).
% diagonal_to_fr_reverse(Offset, Delta, K, D, FRArray, X) :-
%   K #\= Delta + D, 
%   KPlus #= K + 1, KMinus #= K - 1,
%   get_fr(Offset, KPlus, FRArray, KRight),
%   get_fr(Offset, KMinus, FRArray, KLeft),
%   KLeft > KRight,
%   X is KRight - 1.
% diagonal_to_fr_reverse(Offset, Delta, K, _, FRArray, X) :-
%   KMinus #= K - 1,
%   get_fr(Offset, KMinus, FRArray, KLeft),
%   X is KLeft.

% Good
% Strcmp moving forward: while a[i] == b[i]; i++
search_forward([], _, 0) :- true.
search_forward(_, [], 0) :- true.
search_forward([A|Body0], [A|Body1], Offset) :- 
  Next #= Offset-1,
  search_forward(Body0, Body1, Next).
search_forward([A|_], [B|_], 0) :- 
  dif(A, B).

% Good
% Strcmp moving backward: while a[i] == b[i]; i--
search_backward([], _, 0) :- true. 
search_backward(_, [], 0) :- true.
search_backward(A, B, 0) :- 
  append(_, [T0], A), append(_, [T1], B),
  dif(T0, T1).
search_backward(A, B, Offset) :- 
  append(HA, [T], A), append(HB, [T], B),
  Next #= Offset-1,
  search_backward(HA, HB, Next). 

% Good
% Extending a coordinate to furthest reaching point on that diagonal 
% Usually this will be invoked after the search making a middle-snake shift
% The search starts at X on diagonal K, moving alone with the diagonal until 
% From[X] differs To[Y]. We do this in both directions.
% 
% Offset: Offset into the FRArray. For a diagonal K, the actual point is stored 
%         in FRArray[K + Offset]
% From, To: Input String
%  
extend_fr_forward(_, [], [], X, K, X, FRArray, FRArray) :- true.
extend_fr_forward(Offset, From, To, X, K, XOut, FRArrayIn, FRArrayOut) :-
  Index #= Offset + K,
  diagonal_to_xy(K,X,Y),
  append(L, S0, From), length(L, X),
  append(R, S1, To), length(R, Y),
  search_forward(S0, S1, Move),
  XOut #= X+Move,
  append(H, [_|T], FRArrayIn), length(H, Index),
  append(H, [XOut|T], FRArrayOut).

% Good
% Backward Version
extend_fr_backward(_, [], [], X, K, X, FRArray, FRArray) :- true. 
extend_fr_backward(Offset, From, To, X, K, XOut, FRArrayIn, FRArrayOut) :- 
  Index #= Offset + K,
  diagonal_to_xy(K,X,Y),
  append(S0, L, From), length(S0, X),
  append(S1, R, To), length(S1, Y),
  search_backward(S0, S1, Move),
  XOut #= X-Move,
  append(H, [_|T], FRArrayIn), length(H, Index),
  append(H, [XOut|T], FRArrayOut).

%% Inner K-Loop: 
%
%  The inner k loop terminates when: 
%  If searching forward, after making a snake (calling diagonal_to_fr) and search (extend_fr_forward)
%    We check_finish_forward (return x,y if clash with backward searching)
%  If searching backward, after making a snake (calling diagonal_to_fr) and search (extend_fr_backward)
%    We check_finish_backward (return x,y if clash with forward searching)

% Good
%% Forward Search Inner Loop Implementation: 
%% Search through diagonals -d < k < d with a step size of 2 
check_finish_forward(Offset, Delta, D, K, BackwardIn, X) :- 
  % Delta is odd && k in [delta-d+1, delta+d-1]
  Delta mod 2 #= 1, 
  Delta + 1 - D < K + 1, 
  K < Delta + D, 
  Index #= K + Offset, 
  nth0(Index, BackwardIn, X).
   
kloop_iter_forward(Offset, From, To, Delta, D, K, Max, ForwardIn, ForwardIn, BackwardIn, -1, -1) :- 
  K > D. 
kloop_iter_forward(Offset, From, To, Delta, D, K, Max, ForwardIn, ForwardOut, BackwardIn, XOut, YOut) :- 
  diagonal_to_fr(Offset, K, D, ForwardIn, X), % X is FR on diagonal K in previous iteration
  extend_fr_forward(Offset, From, To, X, K, XExt, ForwardIn, ForwardOut), 
  check_finish_forward(Offset, Delta, D, K, BackwardIn, XExt), 
  XOut =:= XExt, diagonal_to_xy(K, XOut, YOut). 
kloop_iter_forward(Offset, From, To, Delta, D, K, Max, ForwardIn, ForwardOut, BackwardIn, XOut, YOut) :- 
  diagonal_to_fr(Offset, K, D, ForwardIn, X), % X is FR on diagonal K in previous iteration
  extend_fr_forward(Offset, From, To, X, K, XExt, ForwardIn, ForwardExt),
  Next #= K+2,
  kloop_iter_forward(Offset, From, To, Delta, D, Next, Max, ForwardExt, ForwardOut, BackwardIn, XOut, YOut). 

% Good
%% Backward Search Inner Loop Implementation: 
%% Search through diagonals -d < k < d with a step size of 2 
check_finish_backward(Offset, Delta, D, K, ForwardIn, X) :- 
  % Delta is even && k+delta in [-d, d]
  Delta mod 2 #= 0, 
  0 - D < K + Delta + 1, 
  K + Delta < D + 1, 
  Index #= K + Offset, 
  nth0(Index, ForwardIn, X).
  
kloop_iter_backward(Offset, From, To, Delta, D, K, Max, BackwardIn, BackwardIn, ForwardIn, -1, -1) :- 
  K > D. 
kloop_iter_backward(Offset, From, To, Delta, D, K, Max, BackwardIn, BackwardOut, ForwardIn, XOut, YOut) :- 
  diagonal_to_fr_reverse(Offset, Delta, K, D, BackwardIn, X), % X is FR on diagonal K in previous iteration
  KPlusDelta #= K + Delta,
  extend_fr_backward(Offset, From, To, X, KPlusDelta, XExt, BackwardIn, BackwardOut),
  check_finish_backward(Offset, Delta, D, K, ForwardIn, XExt), 
  XOut =:= XExt. 
kloop_iter_backward(Offset, From, To, Delta, D, K, Max, BackwardIn, BackwardOut, XOut, YOut) :- 
  diagonal_to_fr_reverse(Offset, Delta, K, D, BackwardIn, X), % X is FR on diagonal K in previous iteration
  extend_fr_backward(Offset, From, To, X, KPlusDelta, XExt, BackwardIn, BackwardExt),
  Next #= K+2,
  kloop_iter_backward(Offset, From, To, Delta, D, Next, Max, BackwardExt, BackwardOut, ForwardIn, XOut, YOut). 

% :- kloop_iter_forward(3, [a, b, c], [a, b, d], 0, 0, 0, [0,0,0,0,0,0], O, [0,0,0,0,0,0], XOut,YOut) 

% Outer D Loop:
% searching by increasing number of differences where d = 0,1, ..., (max+1)/2

dloop_proc_1(From, To, Delta, D, Max, ForwardIn, BackwardIn, XOut,ForwardOut, BackwardOut) :- 
  XOut > -1.
dloop_proc_1(From, To, Delta, D, Max, ForwardIn, BackwardIn, XOut,ForwardOut, BackwardOut) :- 
  kloop_iter_backward(From, To, Delta, D, 0-D, Max, BackwardIn, BackwardExt, ForwardIn, XOut), 
  dloop_proc_2(From, To, Delta, D+1, Max, ForwardIn, BackwardExt, XOut,ForwardOut, BackwardOut). 

dloop_proc_2(From, To, Delta, D, Max, ForwardIn, BackwardExt, XOut,ForwardOut, BackwardOut) :- 
  XOut > -1. 
dloop_proc_2(From, To, Delta, D, Max, ForwardIn, BackwardExt, XOut,ForwardOut, BackwardOut) :- 
  dloop(From, To, Delta, D, Max, ForwardIn, BackwardIn, XOut, ForwardOut, BackwardOut).

dloop(From, To, Delta, D, Max, ForwardIn, BackwardIn, -1, -1,ForwardOut, BackwardOut) :- 
  D > div(Max+1, 2)+1, 
  true. 
dloop(From, To, Delta, D, Max, ForwardIn, BackwardIn, XOut, YOut, ForwardOut, BackwardOut) :- 
  kloop_iter_forward(From, To, Delta, D, 0-D, Max, ForwardIn, ForwardExt, BackwardIn, XOut, YOut), 
  dloop_proc_1(From, To, Delta, D, Max, ForwardExt, BackwardIn, XOut,ForwardOut, BackwardOut).

middle_snake([], [], ReturnX, ReturnY) :- 
  ReturnX = 0, ReturnY = 0.
middle_snake(From, To, ReturnX, ReturnY) :- 
  length(From, M),
  length(To, N),
  Delta is M - N, 
  Max is M + N,
  Offset is (Max+1) // 2,

  L is Offset * 2 + 1, 
  P is Offset + 1,

  % Buffers for furthest reaching point 
  init_list_except_nth0(L, P, 1, Forward),
  init_list_except_nth0(L, P, 1, Backward),

  dloop(From, To, Delta, 0, Max, Forward, Backward, ReturnX, ReturnY, _, _). 




:- begin_tests(middle_snake).

test(middle_snake) :-
        middle_snake([], [], 0, 0),
        middle_snake(['A'], ['A'], 1, 1),
        middle_snake(['A'], ['B'], 0, 1),
        middle_snake(['A'], [], 0, 1), 
        middle_snake(['A'], ['A','B'], 1, 2),
        middle_snake(['A'], ['B','B'], 1, 1),
        middle_snake(['A','B','A'], ['A','B','A'], 3, 3), 
        middle_snake(['A','B','A'], ['A','B'], 2, 2),
        middle_snake(['A','B','A'], ['A','B', 'A', 'A'], 3, 3),
        middle_snake(['A','B','A'], ['C','B', 'A'], 0, 1),
        middle_snake(['A','B','A'], ['A','C', 'A'], 1, 2),
        middle_snake(['A','B','A'], ['A','A', 'C'], 3, 2),
        middle_snake(['A','B','B', 'C'], ['A','A', 'C','B'], 3, 4),
        middle_snake(['A','B','B','B'], ['A','A', 'C','B'], 3, 4).

         
  
:- end_tests(middle_snake).
