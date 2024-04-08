% CPSC 312 2024
% m_diff functions

divide_string({Char0,Lo0,Hi0}, {Char1,Lo1,Hi1}, X, Y, {Char0,Lo0,Mid0}, {Char1,Lo1,Mid1}, {Char0,Mid0,Hi0},{Char1,Mid1,Hi1} ) :-
    % Assumeing given {ABCDE, 0,5}, {ABDF, 0, 4}, 3,2 
    % return    {ABCDE, 0, 3}, // ABC
    %           {ABDF, 0, 2}, //AB
    %           {ABCDE, 3,5}, //DE
    %           {ABDF, 3, 4} //DF
    Mid0 is Lo0 + X,
    Mid1 is Lo1 + Y.

all_points_on_diagonal({_,Lo0,Lo0}, {_,Lo1,Lo1}, Path) :- 
    % add all pts on diagonal into path
    % base case
    Path = [[Lo0,Lo1]].

all_points_on_diagonal({Char0,Lo0,Hi0}, {Char1,Lo1,Hi1}, Path) :- 
    % add all pts on diagonal into path
    Path0 = [[Lo0,Lo1]],
    Lo0_new is Lo0 + 1,
    Lo1_new is Lo1 + 1,
    all_points_on_diagonal({Char0,Lo0_new,Hi0}, {Char1,Lo1_new,Hi1}, Path_rest),
    append(Path0, Path_rest, Path).


div_conq_matrix({_,Lo0,Lo0}, {_,Lo1,Lo1}, Path) :-
    % base case: both empty
    Path = [].

% div_conq_matrix({Char0,Lo0,Hi0}, {Char1,Lo1,Lo1}, Path) :-
div_conq_matrix({_,Lo0,Hi0}, {_,Lo1,Lo1}, Path) :-
    % base case: char1 empty return delete char0 -> [Lo0,Lo1],[Lo0+1,Lo1]
    Path = [[Lo0, Lo1],[Hi0, Lo1]].

% div_conq_matrix({Char0,Lo0,Lo0}, {Char1,Lo1,Hi1}, Path) :-
div_conq_matrix({_,Lo0,Lo0}, {_,Lo1,Hi1}, Path) :-
    % base case: char0 empty return  insert char1 -> [Lo0,Lo1],[Lo0,Lo1+1]
    Path = [[Lo0, Lo1],[Lo0, Hi1]].

div_conq_matrix({Str0,Lo0,Hi0}, {Str1,Lo1,Hi1}, Path) :-
    % base case: if both short and not equal, return delete char0 -> [Lo0,Lo1],[Lo0+1,Lo1], insert char1 -> [Lo0+1,Lo1+1]
    Hi0 is Lo0 + 1,
    Hi1 is Lo1 + 1,
    nth1(1, Str0, Char0),
    nth1(1, Str1, Char1),
    Char0 \= Char1,
    Path = [[Lo0, Lo1],[Hi0, Lo1],[Hi0, Hi1]].    

div_conq_matrix({Char0,Lo0,Hi0}, {Char1,Lo1,Hi1}, Path) :-
    % base case: if midsnake return diagonal end point and char0 char1 exactly match
    % Template function to get snake mid point
    mid_snake({Char0,Lo0,Hi0}, {Char1,Lo1,Hi1}, X, Y),
    X is Hi0 - Lo0 + 1,
    Y is Hi1 - Lo1 + 1,
    all_points_on_diagonal({Char0,Lo0,Hi0}, {Char1,Lo1,Hi1}, Path).

div_conq_matrix({Char0,Lo0,Hi0}, {Char1,Lo1,Hi1}, Path) :-
    % Template function to get snake mid point
    mid_snake({Char0,Lo0,Hi0}, {Char1,Lo1,Hi1}, X, Y),
    divide_string({Char0,Lo0,Hi0}, {Char1,Lo1,Hi1}, X, Y, SubStr0, SubStr1, SubStr2, SubStr3),
    Lo2 is Lo0 + X - 1,
    Lo3 is Lo1 + Y - 1,
    Path0 = [[Lo2,Lo3]],
    div_conq_matrix(SubStr0, SubStr1, Path_L),
    div_conq_matrix(SubStr2, SubStr3, Path_R),
    append(Path_L, Path0, Path_L_0),
    append(Path_L_0, Path_R, Path).

read_file_to_string(FileName, Content) :-
    open(FileName, read, Stream),
    read_string(Stream, _, Content),
    close(Stream).

remove_duplicates([], []).
remove_duplicates([H|T], [H|Result]) :-
    % Remove all H from T
    delete_all(H, T, NewT),
    remove_duplicates(NewT, Result).

delete_all(_, [], []).
delete_all(X, [X|T], Result) :-
    delete_all(X, T, Result).
delete_all(X, [H|T], [H|Result]) :-
    dif(X, H),
    delete_all(X, T, Result).

mdiff(Diff) :-
    read_file_to_string("A.txt", Row_string),
    read_file_to_string("B.txt", Column_string),
    string_chars(Row_string, Row_chars), 
    string_chars(Column_string, Column_chars),
    length(Row_chars, row_len),
    length(Column_chars, column_len),
    div_conq_matrix({Row_string,0,row_len}, {Column_string,0,column_len}, Path),
    remove_duplicates(Path, NewPath),
    render(Row_chars, Column_chars, [0, 0], NewPath, Diff).

render([R_char | R_tail], [C_char | C_tail], [R, C], [[R1, C1] | Path_tail], Diff) :-
    R1 == 0, 
    C1 == 0,
    render([R_char | R_tail], [C_char | C_tail], [R, C], Path_tail, Diff);

    R1 - R > C1 - C,
    render(R_tail, [C_char | C_tail], [R1, C1], Path_tail, Diff_child),
    atom_concat('+', R_char, Result),
    append([Result], Diff_child, Diff);

    R1 - R =:= C1 - C,
    render(R_tail, C_tail, [R1, C1], Path_tail, Diff_child),
    atom_concat('=', R_char, Result),
    append([Result], Diff_child, Diff);

    R1 - R < C1 - C,
    render([R_char | R_tail], C_tail, [R1, C1], Path_tail, Diff_child),
    atom_concat('-', C_char, Result),
    append([Result], Diff_child, Diff).


render([], [C_char | C_tail], _, [[R1, C1] | Path_tail], Diff) :-
    render([], C_tail, [R1, C1], Path_tail, Diff_child),
    atom_concat('-', C_char, Result),
    append([Result], Diff_child, Diff).


render([R_char | R_tail],[], _, [[R1, C1] | Path_tail], Diff) :-
    render(R_tail,[], [R1, C1], Path_tail, Diff_child),
    atom_concat('+', R_char, Result),
    append([Result], Diff_child, Diff).

render([], [], _, [], []).


% tests

:- begin_tests(render_tests).
test(test1) :- 
    render(['a','b','c'], ['a','b','c'], [0,0], [[0,0], [1,1], [2,2], [3,3]], ['=a', '=b', '=c']),
    render(['a','b','c'], ['a','b','c', 'd'], [0,0], [[0, 0], [1,1], [2,2], [3,3]], ['=a', '=b', '=c', '-d']).


test(test2) :- 
    render(['C','B','A','B','A','C'], ['A', 'B', 'C', 'A','B','B','A'], [0,0], [[0,1], [0,2], [1,3], [2,3], [3,4], [4,5], [4,6], [5,7], [6,7]], ['-A', '-B', '=C', '+B', '=A', '=B', '-B', '=A', '+C']).

:- end_tests(render_tests).

    
% Requirement for Input and Output:
% Point format should be [Row,Column]