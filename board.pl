/** <module> Board

This module provides predicate to manipulate a 2D game board

@author Thomas Petiteau
@version 1.0
*/
:- module(board, [board_new/2,
                    board/1,
                    board_width/1,
                    board_height/1,
                    board_cell_coord/3,
                    board_get_cell_value/2,
                    board_get_cells_value/2,
                    board_set_cell_value/2,
                    board_set_cells_value/2]).

:- use_module(tools).

/**
 * board_new(+Width: int, +Height: int)
 *
 * Create a new board
 *
 * @arg Width the width of the board, between 1 and 26
 * @param Height the height of the board, superior or equal to 1
 */
board_new(Width, Height) :-
    Width > 0,
    Width < 27,
    Height > 0,

    retractall(board_width(PreWidth)),
    retractall(board_height(PreHeight)),

    retractall(board(PreBoard)),

    retractall(board_cell_coord(Cell, X, Y)),

    asserta(board_width(Width)),
    asserta(board_height(Height)),

    list_build(empty, Width, Row),
    list_build(Row, Height, Board),
    asserta(board(Board)),

    not(board_assert_cell_coord(0, 0)).

/**
 * board_width(-Width: int)
 *
 * Return the width of the board
 *
 * @param Width the width of the board
 */
:- dynamic board_width/1.

/**
 * board_height(-Height: int)
 *
 * Return the height of the board
 *
 * @param Height the height of the board
 */
:- dynamic board_height/1.

/**
 * board(-Board: list)
 *
 * Return the board as a list of lists where every list is a row
 *
 * @param Board the board
 */
:- dynamic board/1.

/**
 * board_cell_coord(?Cell: term, ?X: int, ?Y: int)
 *
 * Convert between a cell and its coordinate (X;Y)
 *
 * @param Cell a cell
 * @param X index of the column
 * @param Y index of the row
 */
:- dynamic board_cell_coord/3.

/**
 * board_assert_cell_coord(+FileCode: int, +Rank: int)
 *
 * assert all the valid board_cell_coord predicate
 *
 * @param FileCode the ascii code of the first file, should be 97
 * @param Rank the rank of the first row, should be 8
 */
board_assert_cell_coord(X, Y) :-
    board_width(Width),
    board_height(Height),
    X is Width,
    Y < Height,
    YPlusOne is Y + 1,
    board_assert_cell_coord(0, YPlusOne).

board_assert_cell_coord(X, Y) :-
    board_width(Width),
    board_height(Height),
    X < Width,
    Y < Height,
    XPlusOne is X + 1,
    FileCode is X + 97,
    Rank is Y * -1 + Height,
    char_code(File, FileCode),
    atom_concat(File, Rank, Cell),
    asserta(board_cell_coord(Cell, X, Y)),
    board_assert_cell_coord(XPlusOne, Y).

/**
 * board_get_cell_value(+X: int, +Y: int, -Value: term)
 *
 * Get the value of the cell (X; Y)
 *
 * @param X index of the column
 * @param Y index of the row
 * @param Value value of the cell
 */
board_get_cell_value(X, Y, Value) :-
    board(Board),
    nth0(Y, Board, Row),
    nth0(X, Row, Value).

/**
 * board_get_cell_value(+Cell: term, -Value: term)
 *
 * Get the value of a cell
 *
 * @param Cell a cell
 * @param Value the value of the cell
 */
board_get_cell_value(Cell, Value) :-
    board_cell_coord(Cell, X, Y),
    board_get_cell_value(X, Y, Value).

/**
 * board_get_cells_value(+Cells: list, -CellsValue: list)
 *
 * Get the value of given cells
 *
 * @param Cells a list of cell
 * @param CellsValue the value of every cell in Cells in same order
 */
board_get_cells_value([], []).
board_get_cells_value([Cell|Cells], [CellValue|CellsValue]) :-
    board_get_cell_value(Cell, CellValue),
    board_get_cells_value(Cells, CellsValue).

/**
 * board_set_cell_value(+X: int, +Y: int, +NewValue: term)
 *
 * Replace the value of the cell (X; Y) by NewValue
 *
 * @param X index of the column
 * @param Y index of the row
 * @param NewValue the new value of the cell (X; Y)
 */
board_set_cell_value(X, Y, NewValue) :-
    board(Board),
    nth0(Y, Board, Row),
    replace(Row, X, NewValue, NewRow),
    replace(Board, Y, NewRow, NewBoard),
    retract(board(Board)),
    asserta(board(NewBoard)).

/**
 * board_set_cell_value(+Cell: term, +NewValue: term)
 *
 * Replace the value of a cell by NewValue
 *
 * @param Cell a cell
 * @param NewValue the new value of the cell
 */
board_set_cell_value(Cell, NewValue) :-
    board_cell_coord(Cell, X, Y),
    board_set_cell_value(X, Y, NewValue).

/**
 * board_set_cells_value(+Cells: list, +NewValue: term)
 *
 * Replace the value of the given cells by NewValue
 *
 * @param Cells a list of cell
 * @param NewValue the new vvalue of the cells
 */
board_set_cells_value([], NewValue).
board_set_cells_value([Cell|Cells], NewValue) :-
    board_set_cell_value(Cell, NewValue),
    board_set_cells_value(Cells, NewValue).
