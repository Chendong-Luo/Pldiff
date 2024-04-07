% CPSC 312 2024
% Some simple Prolog examples. In public domain.

% To load in Prolog,:
% swipl
% ?- [mdiff].

% Helper function
% Helper input 
% [[1,2,3,4,5,6],[7,8,9,10,11,12],[13,14,15,16,17,18],[19,20,21,22,23,24],[25,26,27,28,29,30],[31,32,33,34,35,36]]

middle_length(List, MidLength) :- 
    append(Left, [Middle|_], List),
    length(Left, MidLength),
    length(Right, MidLength),
    append(Left, [Middle|Right], List);

    append(Left, Right, List),
    length(Left, MidLength),
    length(Right, MidLength).

middle_point_coor(Matrix, [Y,X]) :- 
    % Y = Num of row / 2, pick the up one if row is odd
    % X = Num of col / 2, pick the left one if col is odd
    middle_length(Matrix, Y),
    nth1(Y, Matrix, Row),
    middle_length(Row, X).

middle_point(Matrix, MiddPoint) :-
    middle_point_coor(Matrix, [Y,X]),
    nth1(Y, Matrix, Row),
    nth1(X, Row, MiddPoint).


% m_diff functions

coor_value(Matrix, [Y,X], Value) :-
    nth1(Y, Matrix, Row),
    nth1(X, Row, Value).

split_row(Row, Index, Left, Right) :-
    append(Left, Right, Row),
    length(Left, Index).

lr_split_matrix([], _, [], []).

lr_split_matrix([Row|Rest], Index, [Left|LeftRest], [Right|RightRest]) :-
    split_row(Row, Index, Left, Right),
    lr_split_matrix(Rest, Index, LeftRest, RightRest).

up_down_split_matrix(Matrix, Index, Matrix_up, Matrix_down) :-
    append(Matrix_up, Matrix_down, Matrix),
    length(Matrix_up, Index).

divide_matrix(Matrix, [Y,X], Matrix_left_up, Matrix_right_down) :- 
    up_down_split_matrix(Matrix, Y, Up_matrix, Down_matrix),
    lr_split_matrix(Up_matrix, X, Matrix_left_up, _),
    lr_split_matrix(Down_matrix, X, _, Matrix_right_down).


div_conq_matrix(Matrix, Path) :-
    % base case: if matrix contain only one point then return path with one point
    length(Matrix, 1),
    nth1(1, Matrix, Row),
    length(Row, 1),
    coor_value(Matrix, [1,1], Point),
    Path = [Point].

div_conq_matrix(Matrix, Path) :-
    % Template function to get snake mid point
    middle_point_coor(Matrix, [Y,X]),

    % coor_value(Matrix, [Y,X], Mid)
    % this line is no longer needed because the mid point was in the upper left matrix and will be included in LU_path
    
    divide_matrix(Matrix, [Y,X], LU, RD),
    div_conq_matrix(LU, LU_path),
    div_conq_matrix(RD, RD_path),
    append(LU_path, RD_path, Path).

read_file_to_string(FileName, Content) :-
    open(FileName, read, Stream),
    read_string(Stream, _, Content),
    close(Stream).


render_diff(Path, Diff) :-
    read_file_to_string("A.txt", Row_string),
    string_chars(Row_string, Row_chars),
    read_file_to_string("B.txt", Column_string), 
    string_chars(Column_string, Column_chars),
    render(Row_chars, Column_chars, [0, 0], Path, Diff).

render([R_char | R_tail], [C_char | C_tail], [R, C], [[_, [R1, C1]] | Path_tail], Diff) :-
    R1 - R > C1 - C,
    R_new is R + 1,
    render(R_tail, [C_char | C_tail], [R_new, C], [[_, [R1, C1]] | Path_tail], Diff_child),
     atom_concat('+', R_char, Result),
    append([Result], Diff_child, Diff);

    R1 - R =:= C1 - C,
    R_new is R + 1,
    C_new is C + 1,
    render(R_tail, C_tail, [R_new, C_new], Path_tail, Diff_child),
    append([R_char], Diff_child, Diff);

    C_new is C + 1,
    render([R_char | R_tail], C_tail, [R, C_new], [[_, [R1, C1]] | Path_tail], Diff_child),
    atom_concat('-', C_char, Result),
    append([Result], Diff_child, Diff).


render([], [C_char | C_tail], _, [], Diff) :-
   
    render([], C_tail, _, _, Diff_child),
    atom_concat('-', C_char, Result),
    append([Result], Diff_child, Diff).


render([R_char | R_tail], [], _, [], Diff) :-
    render(R_tail, [], _, _, Diff_child),
    atom_concat('+', R_char, Result),
    append([Result], Diff_child, Diff).

render([], [], _, [], []).


% render([a,b,c], [a,b,c], [0,0], [[_, [1,1]], [_, [2,2]], [_,[3,3]]], R).
% render([a,b,c], [a,b,c,d], [0,0], [[_, [1,1]], [_, [2,2]], [_,[3,3]]], R).


    
% Requirement for Input and Output:
% Input format Matrix is a list of Rows, 
% Rows is a list of Points, 
% Point format should be [Char, [Row,Column]] 
%   Char is the content of the cell; 
%   Row, Column is the original row/col number of that cell
%   All three value should stay unchanged after initial input
% Expected black-box function TO_DO: middle_point_coor(Matrix, [Y,X])
%   takes in the matrix and return the coordiator value of Y,X of the mid snake point
%   Expected always return coordiator that is within the matrix
%   Expected base case is 1*1 matrix? if not, either need to fix base case in div_conq_matrix or middle_point_coor (TO_DO: Further discussion)
% TO_DO: test with odd number of col/row, bugs may need to be fixed
% Current Test case to use:
% ?- div_conq_matrix([[1,2,3,4,5,6],[7,8,9,10,11,12],[13,14,15,16,17,18],[19,20,21,22,23,24],[25,26,27,28,29,30],[31,32,33,34,35,36]], Path).
