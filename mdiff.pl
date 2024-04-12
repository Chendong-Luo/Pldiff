% CPSC 312 2024
% m_diff functions

:- consult('middle_snake.pl').

:- use_module(library(clpfd)).
:- use_module(library(lists)).

% fake snake use for test, test case: div_conq_matrix({[a,b,c,d,e],0,5}, {[a,b,d,f],0,4}, Path). 
% middle_snake({_,_,_}, {_,0,4}, 3, 2, 1, 2, 1).
solve_diff_one(N, M, [], _, [[BX, OutY]], BX, BY) :- 
  OutY #= BY + M.
solve_diff_one(N, M, _, [], [[OutX, BY]], BX, BY) :- 
  OutX #= BX + N.
solve_diff_one(N, M, [H|_], [H|_], Path, BX, BY) :- 
  NN #= BX + N, MM #= BY + M,
  (
    N #> M
  -> Path = [[BX,BY], [MM, MM], [NN, MM]]
  ; Path = [[BX,BY], [NN, NN], [NN, MM]]
  ).
solve_diff_one(N, M, [A|_], [B|_], Path, BX, BY) :- 
  dif(A, B), 
  BXPlus1 #= BX + 1, BYPlus1 #= BY + 1, 
  NN #= BX + N, MM #= BY + M,
  (
    N #> M 
  -> Path = [[BX,BY], [BXPlus1, BY], [NN, MM]]
  ; Path = [[BX,BY], [BX, BYPlus1], [NN, MM]]
  ).


len({_, Lo, Hi}, Length) :- 
  Length #= Hi-Lo+1. 

sublist(_, _, _, [], []).
sublist(_, EndIndex, CurrentIndex, _, Sublist) :-
    CurrentIndex >= EndIndex,
    Sublist = [].
sublist(StartIndex, EndIndex, _, _, Sublist) :-
    StartIndex == EndIndex,
    Sublist = [].

sublist(StartIndex, EndIndex, CurrentIndex, [_|Xs], Sublist) :-
    CurrentIndex < StartIndex,
    NewIndex is CurrentIndex + 1,
    sublist(StartIndex, EndIndex, NewIndex, Xs, Sublist).
sublist(StartIndex, EndIndex, CurrentIndex, [X|Xs], [X|Sublist]) :-
    CurrentIndex >= StartIndex,
    CurrentIndex < EndIndex,
    NewIndex is CurrentIndex + 1,
    sublist(StartIndex, EndIndex, NewIndex, Xs, Sublist).

divide_string({Char0,Lo0,Hi0}, {Char1,Lo1,Hi1}, X, Y, U, V, {Char0,Lo0,Mid0}, {Char1,Lo1,Mid1}, {Char0,Mid2,Hi0},{Char1,Mid3,Hi1} ) :-
    Mid0 is Lo0 + X,
    Mid1 is Lo1 + Y,
    Mid2 is Lo0 + U,
    Mid3 is Lo1 + V.

all_points_on_diagonal({_,Lo0,Lo0}, {_,Lo1,Lo1}, Path) :- 
    % add all pts on diagonal into path
    % base case
    Path = [[Lo0,Lo1]].

all_points_on_diagonal({Char0,Lo0,Hi0}, {Char1,Lo1,Hi1}, Path) :- 
    % add all pts on diagonal into path
    Lo0 < Hi0, Lo1 < Hi1,
    Path0 = [[Lo0,Lo1]],
    Lo0_new is Lo0 + 1,
    Lo1_new is Lo1 + 1,
    all_points_on_diagonal({Char0,Lo0_new,Hi0}, {Char1,Lo1_new,Hi1}, Path_rest),
    append(Path0, Path_rest, Path).

% div_conq_matrix({_,Lo0,Lo0}, {_,Lo1,Lo1}, Path) :-
%     % base case: both empty
%     Path = [].
%
% % div_conq_matrix({Char0,Lo0,Hi0}, {Char1,Lo1,Lo1}, Path) :-
% div_conq_matrix({_,Lo0,Hi0}, {_,Lo1,Lo1}, Path) :-
%     % base case: char1 empty return delete char0 -> [Lo0,Lo1],[Lo0+1,Lo1]
%     Path = [[Lo0, Lo1],[Hi0, Lo1]].
%
% % div_conq_matrix({Char0,Lo0,Lo0}, {Char1,Lo1,Hi1}, Path) :-
% div_conq_matrix({_,Lo0,Lo0}, {_,Lo1,Hi1}, Path) :-
%     % base case: char0 empty return  insert char1 -> [Lo0,Lo1],[Lo0,Lo1+1]
%     Path = [[Lo0, Lo1],[Lo0, Hi1]].
%
% div_conq_matrix({Str0,Lo0,Hi0}, {Str1,Lo1,Hi1}, Path) :-
%     % base case: if both short and equal, return path [Lo0,Lo1],[Lo0+1,Lo1+1]
%     Hi0 is Lo0 + 1,
%     Hi1 is Lo1 + 1,
%     nth1(Hi0, Str0, Char0),
%     nth1(Hi1, Str1, Char1),
%     Char0 == Char1,
%     Path = [[Lo0, Lo1],[Hi0, Hi1]].
%
% div_conq_matrix({Str0,Lo0,Hi0}, {Str1,Lo1,Hi1}, Path) :-
%     % base case: if both short and not equal, return delete char0 -> [Lo0,Lo1],[Lo0+1,Lo1], insert char1 -> [Lo0+1,Lo1+1]
%     Hi0 is Lo0 + 1,
%     Hi1 is Lo1 + 1,
%     nth1(Hi0, Str0, Char0),
%     nth1(Hi1, Str1, Char1),
%     Char0 \= Char1,
%     Path = [[Lo0, Lo1],[Hi0, Lo1],[Hi0, Hi1]].    
%
% div_conq_matrix({Char0,Lo0,Hi0}, {Char1,Lo1,Hi1}, Path) :-
%     % base case: if midsnake return diagonal end point and char0 char1 exactly match
%     sublist(Lo0, Hi0, 0, Char0, SubChar0),
%     sublist(Lo1, Hi1, 0, Char1, SubChar1),
%     middle_snake(SubChar0, SubChar1, X, Y, U, V, Diff),
%     X is Hi0 - Lo0,
%     Y is Hi1 - Lo1,
%     all_points_on_diagonal({Char0,Lo0,Hi0}, {Char1,Lo1,Hi1}, Path).

div_conq_matrix({Char0,Lo0,Hi0}, {Char1,Lo1,Hi1}, Path) :-
  Lo0 #> Hi0-1, 
  Lo1Next #= Lo1+1,
  (
    Lo1 < Hi1
  -> div_conq_matrix({Char0,Lo0,Hi0}, {Char1,Lo1Next,Hi1}, PathNext),
     Path = [[Lo0,Lo1]|PathNext]
  ;  Path = [[Lo0,Lo1]]
  ).

div_conq_matrix({Char0,Lo0,Hi0}, {Char1,Lo1,Hi1}, Path) :-
  Lo1 #> Hi1-1, 
  Lo0Next #= Lo0+1,
  (
    Lo0 < Hi0
  -> div_conq_matrix({Char0,Lo0Next,Hi0}, {Char1,Lo1,Hi1}, PathNext),
     Path = [[Lo0,Lo1]|PathNext]
  ;  Path = [[Lo0,Lo1]]
  ).

div_conq_matrix({Char0,Lo0,Hi0}, {Char1,Lo1,Hi1}, Path) :-
    N #= Hi0 - Lo0, M #= Hi1 - Lo1,
    sublist(Lo0, Hi0, 0, Char0, SubChar0),
    sublist(Lo1, Hi1, 0, Char1, SubChar1),
    % sub(Lo0, Hi0, Char0, SubChar0),
    % sub(Lo1, Hi1, Char1, SubChar1),
    format("~n Search: X= ~w Y=~w [~w ,~w]---[~w ,~w] ~n ~n", [SubChar0, SubChar1, Lo0, Hi0, Lo1, Hi1]),
    middle_snake(SubChar0, SubChar1, X, Y, U, V, Diff),
    format(">>> Got: (~w,~w) --> (~w,~w) Diff=~w ~n ~n", [X,Y,U,V,Diff]),
      Lo2 is Lo0 + X,
      Lo3 is Lo1 + Y,
      Lo4 is Lo0 + U,
      Lo5 is Lo1 + V,
      divide_string({Char0,Lo0,Hi0}, {Char1,Lo1,Hi1}, X, Y, U, V, SubStr0, SubStr1, SubStr2, SubStr3),
    ( Diff > 1
    -> 
      % Lo2 is Lo0 + X,
      % Lo3 is Lo1 + Y,
      % Lo4 is Lo0 + U,
      % Lo5 is Lo1 + V,
      Path0 = [[Lo2,Lo3], [Lo4, Lo5]],
      format("Recursive Case: (~w ,~w) (~w, ~w), Diff=~w ~n ~w ~n ~w ~n ~w ~n ~w ~n", [X,Y,U,V,Diff,SubStr0, SubStr1, SubStr2, SubStr3]),
      once(div_conq_matrix(SubStr0, SubStr1, Path_L)),
      once(div_conq_matrix(SubStr2, SubStr3, Path_R)),
      append(Path_L, Path0, Path_L_0),
      append(Path_L_0, Path_R, Path) 
    ; 
      format("Base Case: (~w ,~w) (~w, ~w), Diff=~w ~n ~w ~n ~w ~n ~w ~n ~w ~n", [X,Y,U,V,Diff,SubStr0, SubStr1, SubStr2, SubStr3]),
      append(RowLeft, _, SubChar0), length(RowLeft, X), 
      append(ColLeft, _, SubChar1), length(ColLeft, Y),
      UPlus1 #= U+1, VPlus1 #= V+1, 
      append(RR, RowRight, SubChar0), length(RR, U), 
      append(LL, ColRight, SubChar1), length(LL, V),

      format("Rows ~w ~w ~w ~w ~n", [RowLeft, RowRight, ColLeft, ColRight]),
      solve_diff_one(X, Y, RowLeft, ColLeft, PathLeft, Lo0, Lo1), 
      % length(RowRight, XRight), length(ColRight, YRight),
      UPlusLo0 #= U + Lo0, VPlusLo1 #= V + Lo1,
      % XPlusLo0 #= X + Lo0, YPlusLo1 #= Y + Lo1,
      XRight #= Hi0 - UPlusLo0, YRight #= Hi1 - VPlusLo1,
      solve_diff_one(XRight, YRight, RowRight, ColRight, PathRight, UPlusLo0, VPlusLo1), 
      format("Path Result: ~w ~w ~w ~w ~w ~w ~n", [PathLeft, PathRight, X, Y, XRight, YRight]),
      % append(PathLeft, PathRight, Path)
      append(PathLeft, [[XPlusLo0,YPlusLo1],[UPlusLo0,VPlusLo1]], PathMidLeft),
      append(PathMidLeft, PathRight, Path) 
    ).

read_file_to_string(FileName, Content) :-
    open(FileName, read, Stream),
    read_string(Stream, _, Content),
    close(Stream).

sub(Lo, Hi, In, Out) :- 
  append(Left, Out, LeftMid), 
  append(LeftMid, Right, In),
  LoMinus #= Lo-1,
  length(Left, Lo), 
  OutLen #= Hi-Lo+1,
  length(Out, OutLen).

% remove duplicate points in the Path and also preserve the original sequence.
remove_duplicates([], []).
remove_duplicates([H|T], [H|Result]) :-
    % Remove all H from T
    delete_all(H, T, NewT),
    remove_duplicates(NewT, Result).

% helper for recursively delete all occurances of a point in Path list.
delete_all(_, [], []).
delete_all(X, [X|T], Result) :-
    delete_all(X, T, Result).
delete_all(X, [H|T], [H|Result]) :-
    dif(X, H),
    delete_all(X, T, Result).

% the entry point of this program.
mdiff(Diff, Step) :-
    read_file_to_string("A.txt", Row_string),
    read_file_to_string("B.txt", Column_string),
    string_chars(Row_string, Row_chars), 
    string_chars(Column_string, Column_chars),
    length(Row_chars, Row_len),
    length(Column_chars, Column_len),
    once(div_conq_matrix({Row_chars,0,Row_len}, {Column_chars,0,Column_len}, Path)),
    format("Path: ~w ~n", [Path]),
    remove_duplicates(Path, NewPath),
    render(Row_chars, Column_chars, [0, 0], NewPath, Diff, Step).

mdiff_raw(Diff, S1, S2, Step) :-
    string_chars(S1, Row_chars), 
    string_chars(S2, Column_chars),
    length(Row_chars, Row_len),
    length(Column_chars, Column_len),
    RLen #= Row_len-1, CLen #= Column_len-1,
    once(div_conq_matrix({Row_chars,0,Row_len}, {Column_chars,0,Column_len}, Path)),
    format("Path: ~w ~n", [Path]),
    remove_duplicates(Path, NewPath),
    render(Row_chars, Column_chars, [0, 0], NewPath, Diff, Step).

% render the differences according to the Path list.
render([R_char | R_tail], [C_char | C_tail], [C, R], [[C1, R1] | Path_tail], Diff, Step) :-
    R1 == 0, 
    C1 == 0,
    render([R_char | R_tail], [C_char | C_tail], [C, R], Path_tail, Diff, Step);

    R1 - R < C1 - C,
    C_new is C + 1,
    render(R_tail, [C_char | C_tail], [C_new, R], Path_tail, Diff_child, Step_child),
    Step is Step_child + 1,
    atom_concat('+', R_char, Result),
    append([Result], Diff_child, Diff);

    R1 - R =:= C1 - C,
    R1 - R > 1, 
    R_new is R + 1,
    C_new is C + 1,
    render(R_tail, C_tail, [C_new, R_new], [[C1, R1] | Path_tail], Diff_child, Step_child),
    Step is Step_child,
    atom_concat('=', R_char, Result),
    append([Result], Diff_child, Diff);

    R1 - R =:= C1 - C,
    R1 - R =:= 1,
    R_new is R + 1,
    C_new is C + 1,
    render(R_tail, C_tail, [C_new, R_new], Path_tail, Diff_child, Step_child),
    Step is Step_child,
    atom_concat('=', R_char, Result),
    append([Result], Diff_child, Diff);

    R1 - R > C1 - C,
    R_new is R + 1,
    render([R_char | R_tail], C_tail, [C, R_new], Path_tail, Diff_child, Step_child),
    Step is Step_child + 1,
    atom_concat('-', C_char, Result),
    append([Result], Diff_child, Diff).

% Row chars are running out
render([], [C_char | C_tail], _, _, Diff, Step) :-
    render([], C_tail, _, _, Diff_child, Step_child),
    Step is Step_child + 1,
    atom_concat('-', C_char, Result),
    append([Result], Diff_child, Diff).

% Column chars are running out 
render([R_char | R_tail],[], _, _, Diff, Step) :-
    render(R_tail,[], _, _, Diff_child, Step_child),
    Step is Step_child + 1,
    atom_concat('+', R_char, Result),
    append([Result], Diff_child, Diff).

% base case: both char lists are running out
render([], [], _, [], [], 0).


% tests

test1(D) :-
    render([a,b,c,d], [a,b,c,d], [0,0], [[0,0], [4,4]], D).

test2(D2) :-
    render([a,b,e,f], [a,b,c,f], [0,0], [[0,0],[2,2],[2,3],[3,3],[4,4]], D2).
    
start :-
    repeat,
    write('Enter your choice (1: Compare two strings, 2: Compare files, 3: Quit): '),
    flush_output(current_output),
    read_line_to_string(user_input, InputString),
    atom_number(InputString, Input),
    action(Input),
    Input == 3, !.

action(1) :-
    write('Please enter the original string: '), flush_output(current_output),
    read_line_to_string(user_input, Str1),
    write('You entered: '), write(Str1), nl,
    write('Please enter the string to change to: '), flush_output(current_output),
    read_line_to_string(user_input, Str2),
    write('You entered: '), write(Str2), nl,
    once(mdiff_raw(Diff, Str2, Str1, Step)),
    write('Answer: '), write(Diff), nl,
    write('Minimal steps: '), write(Step), nl.

action(2) :-
    once(mdiff(Diff, Step)),
    write('Answer: '), write(Diff), nl,
    write('Minimal steps: '), write(Step), nl.

action(3) :-
    write('Quitting program.'), nl.
