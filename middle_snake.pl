:- module(middle_snake, [
        middle_snake/4
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
mksnake_backward(Delta,Offset, K, _, FRArray, X) :-
  % Moving Left from Right as there is no diaognal on the left
  KPlus #= K + 1,
  get_fr(Offset, KPlus, FRArray, XRight),
  X is XRight - 1.

% Searching forward and comparing two lists 
% Returns the length of longest common prefix 
search_forward([], [], 0) :- true.
search_forward([], L, 0) :- true.
search_forward(L, [], 0) :- true.
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
extend_fr_forward(_, [], _, X, K, X, FRArray, FRArray) :- true.
extend_fr_forward(_, _, [], X, K, X, FRArray, FRArray) :- true.
extend_fr_forward(Offset, From, To, X, K, X, FRArrayIn, FRArrayOut) :-
  diagonal_to_xy(K,X,Y),
  length(To, L),
  Y #> L-2, 
  Index #= Offset + K,
  append(H, [_|T], FRArrayIn), length(H, Index),
  append(H, [X|T], FRArrayOut).
extend_fr_forward(Offset, From, To, X, K, X, FRArrayIn, FRArrayOut) :-
  length(From, L),
  X #> L-2,
  Index #= Offset + K,
  append(H, [_|T], FRArrayIn), length(H, Index),
  append(H, [X|T], FRArrayOut).
extend_fr_forward(Offset, From, To, X, K, XOut, FRArrayIn, FRArrayOut) :-
  Index #= Offset + K,
  diagonal_to_xy(K,X,Y),
  XX #= X+1,
  YY #= Y+1,
  append(L, S0, From), length(L, XX),
  append(R, S1, To), length(R, YY),
  search_forward(S0, S1, Move),
  XOut #= X+Move,
  append(H, [_|T], FRArrayIn), length(H, Index),
  append(H, [XOut|T], FRArrayOut).

% Good
% Backward Version
extend_fr_backward(_, [], _, X, K, X, FRArray, FRArray) :- true.
extend_fr_backward(_, _, [], X, K, X, FRArray, FRArray) :- true.
extend_fr_backward(_, [], [], X, K, X, FRArrayIn, FRArrayOut) :- 
  X #< 1,
  Index #= Offset + K,
  append(H, [_|T], FRArrayIn), length(H, Index),
  append(H, [X|T], FRArrayOut).
extend_fr_backward(_, [], [], X, K, X, FRArrayIn, FRArrayOut) :- 
  diagonal_to_xy(K,X,Y),
  Y #< 1,
  Index #= Offset + K,
  append(H, [_|T], FRArrayIn), length(H, Index),
  append(H, [X|T], FRArrayOut).
extend_fr_backward(Offset, From, To, X, K, XOut, FRArrayIn, FRArrayOut) :- 
  Index #= Offset + K,
  diagonal_to_xy(K,X,Y),
  XX #= X+1,
  YY #= Y+1,
  append(S0, L, From), length(S0, XX),
  append(S1, R, To), length(S1, YY),
  search_backward(S0, S1, Move),
  (
    X-Move #< 0
  -> XOut #= 0
  ;  XOut #= X-Move
  ),
  append(H, [_|T], FRArrayIn), length(H, Index),
  append(H, [XOut|T], FRArrayOut).

%% Inner K-Loop: 
%
%  The inner k loop terminates when: 
%  If searching forward, after making a snake (calling diagonal_to_fr) and search (extend_fr_forward)
%    We check_finish_forward (return x,y if clash with backward searching)
%  If searching backward, after making a snake (calling diagonal_to_fr) and search (extend_fr_backward)
%    We check_finish_backward (return x,y if clash with forward searching)

% Check Forward Search termination
check_finish_forward(LX, LY, Offset, Delta, D, K, BackwardIn, X) :- 
  Y #= X-K, 
  Y #= LY-1,
  X #= LX-1.
check_finish_forward(LX, LY, Offset, Delta, D, K, BackwardIn, X) :- 
  % Delta is odd && k in [delta-d+1, delta+d-1]
  Delta mod 2 #= 1, 
  Delta + 1 - D < K + 1, 
  K < Delta + D, 
  Index #= K + Offset, 
  nth0(Index, BackwardIn, X).

% Main K-Loop Iteration 
kloop_iter_forward(Offset, From, To, Delta, D, K, Max, ForwardIn, ForwardIn, BackwardIn, -1, -1) :- 
  K #> D. 
kloop_iter_forward(Offset, From, To, Delta, D, K, Max, ForwardIn, ForwardOut, BackwardIn, XOut, YOut) :- 
  K #< D+1, 
  Next #= K+2,
  once(mksnake_forward(Offset, K, D, ForwardIn, X)), % X is FR on diagonal K in previous iteration
  once(extend_fr_forward(Offset, From, To, X, K, XExt, ForwardIn, ForwardExt)), 
  length(From, LX), length(To, LY),
  (
    check_finish_forward(LX, LY, Offset, Delta, D, K, BackwardIn, XExt)
  -> XOut #= XExt, diagonal_to_xy(K, XOut, YOut)
  ; once(kloop_iter_forward(Offset, From, To, Delta, D, Next, Max, ForwardExt, ForwardOut, BackwardIn, XOut, YOut))
  ).

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
  K #> D. 
kloop_iter_backward(Offset, From, To, Delta, D, K, Max, BackwardIn, BackwardOut, ForwardIn, XOut, YOut) :- 
  K #< D+1,
  Next #= K+2,
  KPlusDelta #= K + Delta,
  once(mksnake_backward(Delta, Offset, K, D, BackwardIn, X)), % X is FR on diagonal K in previous iteration
  once(extend_fr_backward(Offset, From, To, X, KPlusDelta, XExt, BackwardIn, BackwardExt)),
  (
     check_finish_backward(Offset, Delta, D, KPlusDelta, ForwardIn, X)
  -> XOut #= X, YOut #= X - K - Delta,
     diagonal_to_xy(KPlusDelta, XOut, YOut)
  ; 
     once(kloop_iter_backward(Offset, From, To, Delta, D, Next, Max, BackwardExt, BackwardOut, ForwardIn, XOut, YOut))
  ).

% kloop_iter_backward(Offset, From, To, Delta, D, K, Max, BackwardIn, BackwardOut, ForwardIn, XOut, YOut) :- 
%   K #< D+1,
%   format('~w ~46t ~w~72|~n', [K, BackwardIn]),
%   Next #= K + 2, 
%   once(mksnake_backward(Delta, Offset, K, D, BackwardIn, X)), % X is FR on diagonal K in previous iteration
%   KPlusDelta #= K + Delta,
%   once(extend_fr_backward(Offset, From, To, X, KPlusDelta, XExt, BackwardIn, BackwardExt)),
%   kloop_iter_backward(Offset, From, To, Delta, D, Next, Max, BackwardExt, BackwardOut, ForwardIn, XOut, YOut).
  

% :- kloop_iter_forward(3, [a, b, c], [a, b, d], 0, 0, 0, [0,0,0,0,0,0], O, [0,0,0,0,0,0], XOut,YOut) 

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

dloop(Offset, From, To, Delta, D, Max, ForwardIn, BackwardIn, -1, -1,ForwardOut, BackwardOut) :- 
  D #> div(Max+1, 2)+1.
dloop(Offset, From, To, Delta, D, Max, ForwardIn, BackwardIn, XOut, YOut, ForwardOut, BackwardOut) :- 
  D #< div(Max+1, 2)+2, 
  DNext #= D+1,
  DNeg  #= 0-D,

  format('DLOOP: ~w ~w ~w ~n', [D, ForwardIn, BackwardIn]),

  once(kloop_iter_forward(Offset, From, To, Delta, D, DNeg, Max, ForwardIn, ForwardExt, BackwardIn, XOut0, YOut0)), 
  (
    XOut0 =:= -1
  -> once(kloop_iter_backward(Offset, From, To, Delta, D, DNeg, Max, BackwardIn, BackwardExt, ForwardExt, XOut1, YOut1)),
     ( 
       XOut1 =:= -1
     -> dloop(Offset, From, To, Delta, DNext, Max, ForwardExt, BackwardExt, XOut ,YOut, ForwardOut, BackwardOut)
     ; XOut is XOut1, YOut is YOut1
     )
  ; XOut is XOut0, YOut is YOut0
  ).
% dloop_proc_1(From, To, Delta, D, Max, ForwardExt, BackwardIn, XOut,ForwardOut, BackwardOut).

middle_snake([], [], ReturnX, ReturnY) :- 
  ReturnX #= 0, ReturnY #= 0.
middle_snake([], S, ReturnX, ReturnY) :- 
  length(S, L),
  ReturnX #= 0, ReturnY #= (L+1)//2.
middle_snake(S, [], ReturnX, ReturnY) :- 
  length(S, L),
  ReturnX #= (L+1)//2, ReturnY #= 0.
middle_snake(S0, S1, ReturnX, ReturnY) :- 
  append(['x'], S0, From),
  append(['y'], S1, To),

  length(From, M),
  length(To, N),
  Delta #= M - N, 
  Max #= M + N,
  Offset #= (Max+1) // 2,

  L #= Offset * 2 + 1, 
  P #= Offset + 1,
  MMinus #= M-1,

  % Buffers for furthest reaching point 
  init_list(Max, 0, Forward),
  init_list(Max, MMinus, Backward),

  dloop(Offset, From, To, Delta, 0, Max, Forward, Backward, ReturnX, ReturnY, _, _). 


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
  mksnake_forward(3,0,0,[0,0,0,0,0,0], 0). 
test('Getting the correct snake move (Forward, K=-1, Left Boundary):') :- 
  mksnake_forward(3,-1,1,[0,0,0,1,0,0], 1). 
test('Getting the correct snake move (Forward, K=1, Right Boundary):') :- 
  mksnake_forward(3,1,1,[0,0,0,1,0,0], 2). 
test('Getting the correct snake move (Forward, K=-2, Left Boundary User Right):') :- 
  mksnake_forward(3,-2,2,[0,0,3,1,2,0], 3). 
test('Getting the correct snake move (Forward, K=-2, Left Boundary Use Right):') :- 
  mksnake_forward(3,-2,2,[0,0,1,1,2,0], 1). 
test('Getting the correct snake move (Forward, K=0, Use Left):') :- 
  mksnake_forward(3,0,2,[0,0,2,1,2,0], 3). 
test('Getting the correct snake move (Forward, K=0, Use Right):') :- 
  mksnake_forward(3,0,2,[0,0,1,1,2,0], 2). 
test('Getting the correct snake move (Forward, K=2, Right Boundary User Left):') :- 
  mksnake_forward(3,2,2,[0,0,1,1,2,0], 3). 
test('Getting the correct snake move (Forward, K=2, Right Boundary User Left):') :- 
  mksnake_forward(3,2,2,[0,0,1,1,1,0], 2). 

test('Getting the correct snake move (Backward, Even Delta):') :- 
  mksnake_backward(0, 3, 0, 0, [6,6,6,6,6,6], 6).
test('Getting the correct snake move (Backward, Even Delta, Moving Left):') :- 
  mksnake_backward(0, 3, -1, 1, [6,6,6,5,6,6], 4).
test('Getting the correct snake move (Backward, Even Delta, Moving Up):') :- 
  mksnake_backward(0, 3, 1, 1, [6,6,6,5,6,6], 5).
test('Getting the correct snake move (Backward, Even Delta, Taking Smaller One and Move Up):') :- 
  mksnake_backward(0, 3, 0, 2, [6,6,4,5,6,6], 4).
test('Getting the correct snake move (Backward, Even Delta, Taking Smaller One and Move Left):') :- 
  mksnake_backward(0, 3, 0, 2, [6,6,5,5,4,6], 3).
test('Getting the correct snake move (Backward, Odd Delta):') :- 
  mksnake_backward(1, 3, 1, 0, [6,6,5,6,6,6], 6).
test('Getting the correct snake move (Backward, Odd Delta, Taking Smaller One and Move Up):') :- 
  mksnake_backward(1, 3, 1, 1, [6,6,5,4,6,5], 4).
test('Getting the correct snake move (Backward, Odd Delta, Taking Smaller One and Move Left):') :- 
  mksnake_backward(1, 3, 1, 1, [6,6,5,5,4,3], 2).
test('Getting the correct snake move (Backward, Odd Delta, Right Boundary):') :- 
  mksnake_backward(1, 3, 2, 1, [6,6,5,5,4,6], 4).
test('Getting the correct snake move (Backward, Odd Delta, Left Boundary):') :- 
  mksnake_backward(1, 3, 0, 2, [6,6,5,5,4,6], 3).

test('Check Finish Forward -- should finish') :- 
  check_finish_forward(5,5,3, 1, 4, 0, [0,1,2,3,4], 3).
test('Check Finish Forward -- should not finish -- delta is even') :- 
  not(check_finish_forward(6,5,3, 0, 4, 0, [0,1,2,3,4], X)).
test('Check Finish Forward -- should not finish -- no match') :- 
  not(check_finish_forward(5,5,3, 1, 4, 0, [0,1,2,2,4], 3)).

test('Check Finish Backward -- should finish') :- 
  check_finish_backward(3, 0, 4, 0, [0,1,2,3,4,5], 3).
test('Check Finish Backward -- should not finish -- delta is odd') :- 
  not(check_finish_backward(3, 1, D, K, [0,1,2,3,4,5], X)).
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
  once(dloop(2, [f,a], [f,a], 0, 0, 4, [0,0,0,0], [1,1,1,1], 1, 1, FOut, BOut)).
test('DLoop Simulation: aa .. aa') :- 
  once(dloop(3, [f,a,a], [f,a,a], 0, 0, 6, [0,0,0,0,0,0], [2,2,2,2,2,2], 2, 2, FOut, BOut)).
test('DLoop Simulation: ab .. aa') :- 
  once(dloop(3, [f,a,b], [f,a,a], 0, 0, 6, [0,0,0,0,0,0], [2,2,2,2,2,2], 1, 2, fout, bout)).


test(middle_snake_0) :-
        middle_snake([], [], 0, 0).
test(middle_snake_k) :-
        middle_snake(['A'], ['A'], 1, 1).
test(middle_snake_2) :-
        middle_snake(['A'], ['B'], 0, 1).
test(middle_snake_3) :-
        middle_snake(['A'], [], 1, 0). 
test(middle_snake_4) :-
        middle_snake(['A'], ['A','B'], 1, 2).
test(middle_snake_5) :-
        middle_snake(['A'], ['B','B'], 0, 2).
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


:- end_tests(middle_snake).
