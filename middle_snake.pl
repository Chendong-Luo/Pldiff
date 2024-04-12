:- module(middle_snake, [
        middle_snake/7
      ]).
:- use_module(library(clpfd)).
:- use_module(library(lists)).

% Util

% Initializes a list of given length with the specified value as init value
init_list(Length, Value, List) :-
  length(List, Length),
  maplist(=(Value), List).

%% Helper Functions: 

% Diagonal(K) to coordinates. 
diagonal_to_xy(K,X,Y) :- 
  Y #= X - K.

% Retrieves the (K+offset)-th element of List and unifies it with X.
get_fr(Offset, K, FRArray, X) :- 
  I #= K+Offset, 
  lists:nth0(I, FRArray, X).

% Get furthest reaching point on Diagonal K by "making a snake move"
% Suppose we are on Diagonal K, we look for the two neighboring diagonals where:
%    XLeft is the current furthest reaching X on diagonal k-1 (to the left of K)
%    XRight is the current furthest reaching X on diagonal k+1 (to the right of K)
% In Forward search, we look which one is GREATER 
%    If we choose XLeft, then our new X for diagonal K is XLeft+1 (moving right)
%    If we choose XRight, then our new X for diaonal K is XRight (moving down)
mksnake_forward(Offset, K, D, FRArray, X) :-
  K #= 0 - D,
  KPlus #= K+1,
  get_fr(Offset, KPlus, FRArray, X).
mksnake_forward(Offset, K,D,FRArray, X) :-
  K #\= D, 
  KPlus #= K+1, KMinus #= K-1,
  get_fr(Offset, KPlus, FRArray, XRight),
  get_fr(Offset, KMinus, FRArray, XLeft),
  XLeft < XRight,
  X is XRight.
mksnake_forward(Offset, K,_,FRArray, X) :-
  KMinus = K-1,
  get_fr(Offset, KMinus, FRArray, XLeft),
  X is XLeft+1.

% Get furthest reaching point on Diagonal K by "making a snake move"
% Suppose we are on Diagonal K, we look for the two neighboring diagonals where:
%    XLeft is the current furthest reaching X on diagonal k-1 (to the left of K)
%    XRight is the current furthest reaching X on diagonal k+1 (to the right of K)
% In Backward search, we look which one is LESS
%    If we choose XLeft, then our new X for diagonal K is XLeft (moving up)
%    If we choose XRight, then our new X for diaonal K is XRight-1 (moving left)
% Backward Search takes an extra parameter "Delta" since the search starts at K=Delta
% NOTE: This K contains Delta offset already
mksnake_backward(Delta,Offset, K, D, FRArray, X) :-
  % Moving Up from Left as there is no diagonal on the right
  K #= Delta + D,
  KMinus #= K - 1,
  get_fr(Offset, KMinus, FRArray, X).
mksnake_backward(Delta,Offset, K, D, FRArray, X) :-
  % Moving Up from Left as Left diagonal is closer to (0,0)
  K #\= Delta - D, 
  KPlus #= K + 1, KMinus #= K - 1,
  get_fr(Offset, KPlus, FRArray, XRight),
  get_fr(Offset, KMinus, FRArray, XLeft),
  XLeft < XRight, % NOTE: if XRight == XLeft, we should still use XRight since we can move left 
  X is XLeft.
mksnake_backward(_,Offset, K, _, FRArray, X) :-
  % Moving Left from Right as there is no diaognal on the left
  KPlus #= K + 1,
  get_fr(Offset, KPlus, FRArray, XRight),
  X is XRight - 1.

% Searching forward and comparing two lists 
% Returns the length of longest common prefix 
search_forward([], [], 0) :- true.
search_forward([], _, 0) :- true.
search_forward(_, [], 0) :- true.
search_forward([A|Body0], [A|Body1], Offset) :- 
  Next #= Offset-1,
  search_forward(Body0, Body1, Next).
search_forward([A|_], [B|_], 0) :- 
  dif(A, B).

% Searching backward and comparing two lists 
% Returns the length of longest common suffix
search_backward([], _, 0) :- true. 
search_backward(_, [], 0) :- true.
search_backward(A, B, 0) :- 
  append(_, [T0], A), append(_, [T1], B),
  dif(T0, T1).
search_backward(A, B, Offset) :- 
  append(HA, [T], A), append(HB, [T], B),
  Next #= Offset-1,
  search_backward(HA, HB, Next). 

% Extending a coordinate to furthest reaching point on that diagonal 
% Usually this will be invoked after the search making a middle-snake shift
% The search starts at X on diagonal K, moving alone with the diagonal until 
% From[X] differs To[Y]. We do this in both directions.
% 
% NOTE: in forward version, we compare from the NEXT string:
%   keep searching while from[x+1+offset] == to[y+1+offset]
%
%
extend_fr_forward(_, A, _, X, _, X, _) :- 
  length(A, L), X #> L-1.
extend_fr_forward(_, _, A, X, K, X, _) :- 
  Y #= X-K,
  length(A, L), Y #> L-1.
extend_fr_forward(_, A, _, X, _, X, _) :- 
  X #< 0.
extend_fr_forward(_, A, _, X, K, X, _) :- 
  Y #= X-K,
  Y #< 0. 
extend_fr_forward(_, [], _, X, _, X, FRArray) :- true.
extend_fr_forward(_, _, [], X, _, X, FRArray) :- true.
extend_fr_forward(Offset, _, To, X, K, X, FRArrayIn) :-
  diagonal_to_xy(K,X,Y),
  length(To, L),
  Y #> L-2, 
  Index #= Offset + K.
extend_fr_forward(Offset, From, _, X, K, X, FRArrayIn) :-
  length(From, L),
  X #> L-2,
  Index #= Offset + K.
extend_fr_forward(Offset, From, To, X, K, XOut, FRArrayIn) :-
  Index #= Offset + K,
  diagonal_to_xy(K,X,Y),
  XX #= X+1,
  YY #= Y+1,
  append(L, S0, From), length(L, XX),
  append(R, S1, To), length(R, YY),
  search_forward(S0, S1, Move),
  XOut #= X+Move.

% Good
% Backward Version
extend_fr_backward(_, A, _, X, _, X, _) :- 
  length(A, L), X #> L-1.
extend_fr_backward(_, _, A, X, K, X, _) :- 
  Y #= X-K,
  length(A, L), Y #> L-1.
extend_fr_backward(_, A, _, X, _, X, _) :- 
  X #< 0.
extend_fr_backward(_, A, _, X, K, X, _) :- 
  Y #= X-K,
  Y #< 0. 
extend_fr_backward(_, [], A, X, K, X, _) :- 
  Y #= X-K, 
  length(A, LA), Y #> LA-1. 
extend_fr_backward(_, _, _, 0, _, 0, _).
extend_fr_backward(_, [], _, X, _, X, _).
extend_fr_backward(_, _, [], X, _, X, _).
extend_fr_backward(Offset, [], [], X, K, X, FRArrayIn) :- 
  X #< 1,
  Index #= Offset + K.
extend_fr_backward(Offset, [], [], X, K, X, FRArrayIn) :- 
  diagonal_to_xy(K,X,Y),
  Y #< 1,
  Index #= Offset + K.
extend_fr_backward(Offset, From, To, X, K, XOut, FRArrayIn) :- 
  Index #= Offset + K,
  diagonal_to_xy(K,X,Y),
  XX #= X+1,
  YY #= Y+1,
  append(S0, _, From), length(S0, XX),
  append(S1, _, To), length(S1, YY),
  search_backward(S0, S1, Move),
  (
    X-Move #< 0
  -> XOut #= 0
  ;  XOut #= X-Move
  ).

%% Inner K-Loop: 
%
%  The inner k loop terminates when: 
%  If searching forward, after making a snake (calling diagonal_to_fr) and search (extend_fr_forward)
%    We check_finish_forward (return x,y if clash with backward searching)
%  If searching backward, after making a snake (calling diagonal_to_fr) and search (extend_fr_backward)
%    We check_finish_backward (return x,y if clash with forward searching)

% Check Forward Search termination
check_finish_forward(LX, LY, _, _, _, K, _, X) :- 
  Y #= X-K, 
  Y #= LY-1,
  X #= LX-1.
check_finish_forward(_, _, Offset, Delta, D, K, BackwardIn, X) :- 
  % Delta is odd && k in [delta-d+1, delta+d-1]
  Delta mod 2 #= 1, 
  Delta + 1 - D < K + 1, 
  K < Delta + D, 
  Index #= K + Offset, 
  nth0(Index, BackwardIn, BackwardX),
  X #= BackwardX.

% Main K-Loop Iteration 
% kloop_iter_forward(_, _, _, _, D, K, _, ForwardIn, ForwardIn, _, -1, -1, -1, -1) :-
%   length(ForwardIn, LF), K #> LF-1, 
%   format("K=~w - Endof Forward ForwardArray = ~w ~n", [K, ForwardIn]).
kloop_iter_forward(_, _, _, _, D, K, _, ForwardIn, ForwardIn, _, -1, -1, -1, -1) :- 
  K #> D, 
  format("    K=~w - Endof Forward ForwardArray = ~w ~n~n", [K, ForwardIn]).
kloop_iter_forward(Offset, From, To, Delta, D, K, Max, ForwardIn, ForwardOut, BackwardIn, XOut, YOut, U, V) :- 
  K #< D+1, 
  % KIndex #= K+Offset, 
  % KIndex #> -1, length(ForwardIn, LF), KIndex #< LF,
  Next #= K+2,
  once(mksnake_forward(Offset, K, D, ForwardIn, X)), % X is FR on diagonal K in previous iteration
  once(extend_fr_forward(Offset, From, To, X, K, XExt, ForwardIn)), 
  Index #= Offset + K,
  put(ForwardIn, Index, XExt, ForwardExt),
  format("    K=~w - Forward ~n ~w ~n ~w ~n", [K, ForwardIn, ForwardExt]),
  length(From, LX), length(To, LY),
  (
    check_finish_forward(LX, LY, Offset, Delta, D, K, BackwardIn, XExt)
  -> XOut #= X, diagonal_to_xy(K, XOut, YOut), U #= XExt, V #= XExt - K
  ; once(kloop_iter_forward(Offset, From, To, Delta, D, Next, Max, ForwardExt, ForwardOut, BackwardIn, XOut, YOut, U, V))
  ).

%% Backward Search Inner Loop Implementation: 
%% Search through diagonals -d < k < d with a step size of 2 
check_finish_backward(Offset, Delta, D, K, ForwardIn, X) :- 
  % Delta is even && k+delta in [-d, d]
  Delta mod 2 #= 0, 
  0 - D < K + 1, 
  K < D + 1, 
  Index #= K + Offset, 
  nth0(Index, ForwardIn, ForwardX),
  format("Backward Checking K=~w overlapping? ~w ~w ~w !!!!! ~n", [K, X, ForwardX, ForwardIn]),
  X #= ForwardX.
  
kloop_iter_backward(_, _, _, _, D, K, _, BackwardIn, BackwardIn, _, -1, -1, -1, -1) :- 
  K #> D, 
  format("    K=~w - Endof BackwardArray = ~w ~n~n", [K, BackwardIn]).
kloop_iter_backward(Offset, From, To, Delta, D, K, Max, BackwardIn, BackwardOut, ForwardIn, XOut, YOut, U, V) :- 
  K #< D+1, 
  KPlusDelta #= K + Delta, 
  % KIndex #= KPlusDelta+Offset, 
  % KIndex > -1, length(BackwardIn, LB), KIndex #< LB,
  Next #= K+2,
  once(mksnake_backward(Delta, Offset, KPlusDelta, D, BackwardIn, X)), % X is FR on diagonal K in previous iteration
  once(extend_fr_backward(Offset, From, To, X, KPlusDelta, XExt, BackwardIn)),
  Index #= Offset+KPlusDelta,
  put(BackwardIn, Index, XExt, BackwardExt),
  format("    K=~w - Backward ~w ~n ~w ~n", [KPlusDelta, BackwardIn, BackwardExt]),
  (
     check_finish_backward(Offset, Delta, D, KPlusDelta, ForwardIn, X)
   -> XOut #= XExt, YOut #= XExt - K - Delta, U #= X, V #= X-K,
     diagonal_to_xy(KPlusDelta, XOut, YOut)
  ; once(kloop_iter_backward(Offset, From, To, Delta, D, Next, Max, BackwardExt, BackwardOut, ForwardIn, XOut, YOut, U, V))
  ).

put(List, N, X, ListOut) :- 
  append(L0, [_|L1], List), length(L0, N),
  append(L0, [X|L1], ListOut).

% Outer D Loop:
% searching by increasing number of differences where d = 0,1, ..., (max+1)/2

% dloop_proc_1(From, To, Delta, D, Max, ForwardIn, BackwardIn, XOut,ForwardOut, BackwardOut) :- 
%   XOut > -1.
% dloop_proc_1(From, To, Delta, D, Max, ForwardIn, BackwardIn, XOut,ForwardOut, BackwardOut) :- 
%   kloop_iter_backward(From, To, Delta, D, 0-D, Max, BackwardIn, BackwardExt, ForwardIn, XOut), 
%   dloop_proc_2(From, To, Delta, D+1, Max, ForwardIn, BackwardExt, XOut,ForwardOut, BackwardOut). 
%
% dloop_proc_2(From, To, Delta, D, Max, ForwardIn, BackwardExt, XOut,ForwardOut, BackwardOut) :- 
%   XOut > -1. 
% dloop_proc_2(From, To, Delta, D, Max, ForwardIn, BackwardExt, XOut,ForwardOut, BackwardOut) :- 
%   dloop(From, To, Delta, D, Max, ForwardIn, BackwardIn, XOut, ForwardOut, BackwardOut).

dloop(_, _, _, _, D, Max, _, _, -1, -1,_, _, -1, -1, 0) :- 
  D #> div(Max+1, 2)+1,
  format("D Loop Finished ~n",[]).
dloop(Offset, From, To, Delta, D, Max, ForwardIn, BackwardIn, XOut, YOut, ForwardOut, BackwardOut, U, V, DOut) :- 
  D #< div(Max+1, 2)+2, 
  format("~n D = ~w Loop ~n ~w ~n ~w ~n",[D, ForwardIn, BackwardIn]),
  DNext #= D+1,
  DNeg  #= 0-D,
  once(kloop_iter_forward(Offset, From, To, Delta, D, DNeg, Max, ForwardIn, ForwardExt, BackwardIn, XOut0, YOut0, U0, V0)), 
  (
    XOut0 =:= -1
  -> once(kloop_iter_backward(Offset, From, To, Delta, D, DNeg, Max, BackwardIn, BackwardExt, ForwardExt, XOut1, YOut1, U1, V1)),
     ( 
       XOut1 =:= -1
     -> dloop(Offset, From, To, Delta, DNext, Max, ForwardExt, BackwardExt, XOut ,YOut, ForwardOut, BackwardOut, U, V, DOut)
     ; XOut is XOut1, YOut is YOut1, U is U1, V is V1, DOut #= D
     )
  ; XOut is XOut0, YOut is YOut0, U is U0, V is V0, DOut #= D
  ).

middle_snake([], [], ReturnX, ReturnY, 0,0,0) :- 
  ReturnX #= 0, ReturnY #= 0.
middle_snake([], S, ReturnX, ReturnY, U, V, Diff) :- 
  length(S, Diff),
  ReturnX #= 0, ReturnY #= 0, U #= 0, V #= Diff.
middle_snake(S, [], ReturnX, ReturnY, U, V, DOut) :- 
  length(S, Diff),
  ReturnX #= 0, ReturnY #=0, U #= 0, V #= Diff.
middle_snake(S0, S1, ReturnX, ReturnY, U, V, DOut) :- 
  append(['x'], S0, From),
  append(['y'], S1, To),

  length(From, M),
  length(To, N),
  Delta #= M - N, 
  Max #= M + N,
  Offset #= (Max+1) // 2,
  BufferSize #= 2*Max + 1,

  MMinus #= M-1,

  % Buffers for furthest reaching point 
  init_list(BufferSize, 0, Forward),
  init_list(BufferSize, MMinus, Backward),

  dloop(Max, From, To, Delta, 0, Max, Forward, Backward, ReturnX, ReturnY, _, _, U, V, DOut). 


%% Unit Tests 

:- begin_tests(middle_snake).

test("Get RF, 0 offset") :- 
  get_fr(3, 3, [0,1,2,3,4,5,6], 6).
test("Get RF, 3 offset") :- 
  get_fr(0, 3, [0,1,2,3,4,5,6], 3).

test("Searching forward produces correct movement (a, b) ") :- 
  once(search_forward([a],[b], 0)).
test("Searching forward produces correct movement (a, ) ") :- 
  once(search_forward([a],[], 0)).
test("Searching forward produces correct movement ( ,a) ") :- 
  once(search_forward([],[a], 0)).
test("Searching forward produces correct movement (a, a) ") :- 
  once(search_forward([a],[a], 1)).
test("Searching forward produces correct movement (ab, b) ") :- 
  once(search_forward([a,b],[a], 1)).
test("Searching forward produces correct movement (a, ab) ") :- 
  once(search_forward([a],[a,b], 1)).
test("Searching forward produces correct movement (ab, ab) ") :- 
  once(search_forward([a,b],[a,b], 2)).

test("Searching backwards produces correct movement (a, b) ") :- 
  once(search_backward([a],[b], 0)).
test("Searching backwards produces correct movement (a, ) ") :- 
  once(search_backward([a],[], 0)).
test("Searching backwards produces correct movement ( ,a) ") :- 
  once(search_backward([],[a], 0)).
test("Searching backwards produces correct movement (a, a) ") :- 
  once(search_backward([a],[a], 1)).
test("Searching backwards produces correct movement (ba, a) ") :- 
  once(search_backward([b,a],[a], 1)).
test("Searching backwards produces correct movement (a, ba) ") :- 
  once(search_backward([a],[b,a], 1)).
test("Searching backwards produces correct movement (ab, a) ") :- 
  once(search_backward([a,b],[a], 0)).
test("Searching backwards produces correct movement (b, ab) ") :- 
  once(search_backward([b],[a,b], 1)).
test("Searching backwards produces correct movement (ab, ab) ") :- 
  once(search_backward([a,b],[a,b], 2)).

test('Getting the correct snake move (Forward, K=0):') :- 
  once(mksnake_forward(3,0,0,[0,0,0,0,0,0], 0)). 
test('Getting the correct snake move (Forward, K=-1, Left Boundary):') :- 
  once(mksnake_forward(3,-1,1,[0,0,0,1,0,0], 1)). 
test('Getting the correct snake move (Forward, K=1, Right Boundary):') :- 
  once(mksnake_forward(3,1,1,[0,0,0,1,0,0], 2)). 
test('Getting the correct snake move (Forward, K=-2, Left Boundary User Right):') :- 
  once(mksnake_forward(3,-2,2,[0,0,3,1,2,0], 3)). 
test('Getting the correct snake move (Forward, K=-2, Left Boundary Use Right):') :- 
  once(mksnake_forward(3,-2,2,[0,0,1,1,2,0], 1)). 
test('Getting the correct snake move (Forward, K=0, Use Left):') :- 
  once(mksnake_forward(3,0,2,[0,0,2,1,2,0], 3)). 
test('Getting the correct snake move (Forward, K=0, Use Right):') :- 
  once(mksnake_forward(3,0,2,[0,0,1,1,2,0], 2)). 
test('Getting the correct snake move (Forward, K=2, Right Boundary User Left):') :- 
  once(mksnake_forward(3,2,2,[0,0,1,1,2,0], 3)). 
test('Getting the correct snake move (Forward, K=2, Right Boundary User Left):') :- 
  once(mksnake_forward(3,2,2,[0,0,1,1,1,0], 2)). 

test('Getting the correct snake move (Backward, Even Delta):') :- 
  once(mksnake_backward(0, 3, 0, 0, [6,6,6,6,6,6], 6)).
test('Getting the correct snake move (Backward, Even Delta, Moving Left):') :- 
  once(mksnake_backward(0, 3, -1, 1, [6,6,6,5,6,6], 4)).
test('Getting the correct snake move (Backward, Even Delta, Moving Up):') :- 
  once(mksnake_backward(0, 3, 1, 1, [6,6,6,5,6,6], 5)).
test('Getting the correct snake move (Backward, Even Delta, Taking Smaller One and Move Up):') :- 
  once(mksnake_backward(0, 3, 0, 2, [6,6,4,5,6,6], 4)).
test('Getting the correct snake move (Backward, Even Delta, Taking Smaller One and Move Left):') :- 
  once(mksnake_backward(0, 3, 0, 2, [6,6,5,5,4,6], 3)).
test('Getting the correct snake move (Backward, Odd Delta):') :- 
  once(mksnake_backward(1, 3, 1, 0, [6,6,5,6,6,6], 6)).
test('Getting the correct snake move (Backward, Odd Delta, Taking Smaller One and Move Up):') :- 
  once(mksnake_backward(1, 3, 1, 1, [6,6,5,4,6,5], 4)).
test('Getting the correct snake move (Backward, Odd Delta, Taking Smaller One and Move Left):') :- 
  once(mksnake_backward(1, 3, 1, 1, [6,6,5,5,4,3], 2)).
test('Getting the correct snake move (Backward, Odd Delta, Right Boundary):') :- 
  once(mksnake_backward(1, 3, 2, 1, [6,6,5,5,4,6], 4)).
test('Getting the correct snake move (Backward, Odd Delta, Left Boundary):') :- 
  once(mksnake_backward(1, 3, 0, 2, [6,6,5,5,4,6], 3)).

test('Check Finish Forward -- should finish') :- 
  check_finish_forward(5,5,3, 1, 4, 0, [0,1,2,3,4], 3).
test('Check Finish Forward -- should not finish -- delta is even') :- 
  not(check_finish_forward(6,5,3, 0, 4, 0, [0,1,2,3,4], _)).
test('Check Finish Forward -- should not finish -- no match') :- 
  not(check_finish_forward(5,5,3, 1, 4, 0, [0,1,2,2,4], 3)).

test('Check Finish Backward -- should finish') :- 
  check_finish_backward(3, 0, 4, 0, [0,1,2,3,4,5], 3).
test('Check Finish Backward -- should not finish -- delta is odd') :- 
  not(check_finish_backward(3, 1, _, _, [0,1,2,3,4,5], _)).
test('Check Finish Backward -- should not finish -- no match') :- 
  not(check_finish_backward(3, 0, 4, 0, [0,1,2,2,2,2,2], 3)).

% kloop_iter_forward(Offset, From, To, Delta, D, K, Max, ForwardIn, ForwardIn, BackwardIn, -1, -1) :- 
test('KLoop Simulation Forward: D=0') :- 
  once(kloop_iter_forward(4, [f,a,b,b], [f,a,c,b], 0, 0, 0, 8, [0,0,0,0,0,0,0,0], [0,0,0,0,1,0,0,0], [0,0,0,0,0,0,0,0], -1, -1)).
test('KLoop Simulation Forward: D=1') :- 
  once(kloop_iter_forward(4, [f,a,b,b], [f,a,c,b], 0, 1, -1, 8, [0,0,0,0,1,0,0,0], [0,0,0,2,1,2,0,0], [0,0,0,0,0,0,0,0], -1, -1)).
test('KLoop Simulation Finish: D=1 Delta=1 Finish') :- 
  once(kloop_iter_forward(4, [f,a,b,b], [f,a,c], 1, 2, -2, 7, [0,0,0,1,1,2,0], [0,0,1,1,2,2,0], [3, 3, 3, 3, 2, 3, 3], 2, 2)).
test('KLoop Simulation Finish: D=1 Delta=0') :- 
  once(kloop_iter_forward(3, [f,a,b],[f,a,a], 0, 1, -1, 6, [0,0,0,1,0,0],[0,0,1,1,2,0],[2,2,2,2,2,2],-1,-1)).

test('KLoop Simulation Backward: D=0') :- 
  once(kloop_iter_backward(4, [f,a,b,b], [f,a,c,b], 0, 0, 0, 8, [3,3,3,3,3,3,3,3], [3,3,3,3,2,3,3,3], [0,0,0,0,0,0,0,0], -1, -1)).
test('KLoop Simulation Backward: D=1') :- 
  once(kloop_iter_backward(4, [f,a,b,b], [f,a,c,b], 0, 1, -1, 8, [3,3,3,3,2,3,3,3], [3,3,3,1,2,2,3,3], [0,0,0,0,0,0,0,0], -1, -1)).
test('KLoop Simulation Backward: D=1 Finish') :- 
  once(kloop_iter_backward(4, [f,a,b,b], [f,a,c,b], 0, 1, -1, 8, [3,3,3,3,2,3,3,3], [3,3,3,1,2,3,3,3], [0, 0, 0, 2, 1, 2, 0, 0], 2, 1)).

test('DLoop Simulation: a .. a') :- 
  once(dloop(2, [f,a], [f,a], 0, 0, 4, [0,0,0,0], [1,1,1,1], 1, 1, _, _)).
test('DLoop Simulation: aa .. aa') :- 
  once(dloop(3, [f,a,a], [f,a,a], 0, 0, 6, [0,0,0,0,0,0], [2,2,2,2,2,2], 2, 2, _, _)).
test('DLoop Simulation: ab .. aa') :- 
  once(dloop(3, [f,a,b], [f,a,a], 0, 0, 6, [0,0,0,0,0,0], [2,2,2,2,2,2], 1, 2, _, _)).


test(middle_snake_0) :-
        once(middle_snake([], [], 0, 0)).
test(middle_snake_k) :-
        middle_snake(['A'], ['A'], 1, 1).
test(middle_snake_2) :-
        middle_snake(['A'], ['B'], 0, 1).
test(middle_snake_3) :-
        once(middle_snake(['A'], [], 1, 0)). 
test(middle_snake_4) :-
        middle_snake(['A'], ['A','B'], 1, 2).
test(middle_snake_5) :-
        middle_snake(['A'], ['B','B'], 0, 1).
test(middle_snake_6) :-
        middle_snake(['A','B','A'], ['A','B','A'], 3, 3). 
test(middle_snake_7) :-
        middle_snake(['A','B','A'], ['A','B'], 3, 2).
test(middle_snake_8) :-
        middle_snake(['A','B','A'], ['A','B', 'A', 'A'], 3, 4).
test(middle_snake_9) :-
        middle_snake(['A','B','A'], ['C','B', 'A'], 0, 1).
test(middle_snake_10) :-
        middle_snake(['A','B','A'], ['A','C', 'A'], 1, 2).
test(middle_snake_11) :-
        middle_snake(['A','B','A'], ['A','A', 'C'], 3, 2).
test(middle_snake_12) :-
        middle_snake(['A','B','B', 'C'], ['A','A', 'C','B'], 2, 2).
test(middle_snake_13) :-
        middle_snake(['A','B','B','B'], ['A','A', 'C','B'], 2, 2).


% Never Give Up...




:- end_tests(middle_snake).
