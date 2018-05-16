/** <module> Breakthrough

Module providing everything needed to play breakthrough

@author Thomas Petiteau
@version 1.0
*/
:- module(breakthrough, [init_normal_game/0,
                            win/1,
                            turn/1,
                            do/2,
                            legal/2]).

:- use_module(board).
:- use_module(library(lists)).

/**
 * init_normal_game
 *
 * Inititialize a normal breakthrough game
 */
init_normal_game :-
    board_new(8, 8),
    board_set_cells_value([a8, b8, c8, d8, e8, f8, g8, h8, a7, b7, c7, d7, e7, f7, g7, h7], black),
    board_set_cells_value([a1, b1, c1, d1, e1, f1, g1, h1, a2, b2, c2, d2, e2, f2, g2, h2], white),
    retractall(win(Player)),
    retractall(turn(Player2)),
    asserta(turn(white)), !.

/**
 * win(-Player: term)
 *
 * Return the player that win the current game
 *
 * @param Player the player who won
 */
:- dynamic win/1.

/**
 * turn(-Player: term)
 *
 * Return the player who has to play
 */
:- dynamic turn/1.

/**
 * legal(+Player: term, -Action: predicate)
 *
 * Give the legal move the given player can do
 *
 * @param Player a player
 * @param Action an action
 */
legal(white, move(Pawn, Goal)) :-
    not(win(X)),
    turn(white),
    board_cell_coord(Pawn, PX, PY),
    board_cell_coord(Goal, GX, GY),
    board_get_cell_value(Pawn, white),
    board_get_cell_value(Goal, empty),
    GY is PY - 1,
    (GX is PX; GX is PX-1; GX is PX+1).

legal(white, capture(Pawn, Goal)) :-
    not(win(X)),
    turn(white),
    board_cell_coord(Pawn, PX, PY),
    board_cell_coord(Goal, GX, GY),
    board_get_cell_value(Pawn, white),
    board_get_cell_value(Goal, black),
    GY is PY - 1,
    (GX is PX + 1; GX is PX - 1).

legal(black, move(Pawn, Goal)) :-
    not(win(X)),
    turn(black),
    board_cell_coord(Pawn, PX, PY),
    board_cell_coord(Goal, GX, GY),
    board_get_cell_value(Pawn, black),
    board_get_cell_value(Goal, empty),
    GY is PY + 1,
    (GX is PX; GX is PX-1; GX is PX+1).

legal(black, capture(Pawn, Goal)) :-
    not(win(X)),
    turn(black),
    board_cell_coord(Pawn, PX, PY),
    board_cell_coord(Goal, GX, GY),
    board_get_cell_value(Pawn, black),
    board_get_cell_value(Goal, white),
    GY is PY + 1,
    (GX is PX + 1; GX is PX - 1).

/**
 * do(+Player: term, -Action: predicate)
 *
 * Change the current state according to the given action
 *
 * @param Player a player
 * @param Action an action
 */
do(white, move(Pawn, Goal)) :-
    legal(white, move(Pawn, Goal)),
    board_set_cell_value(Pawn, empty),
    board_set_cell_value(Goal, white),
    retract(turn(white)),
    asserta(turn(black)),
    check_win.

do(white, capture(Pawn, Goal)) :-
    legal(white, capture(Pawn, Goal)),
    board_set_cell_value(Pawn, empty),
    board_set_cell_value(Goal, white),
    retract(turn(white)),
    asserta(turn(black)),
    check_win.

do(black, move(Pawn, Goal)) :-
    legal(black, move(Pawn, Goal)),
    board_set_cell_value(Pawn, empty),
    board_set_cell_value(Goal, black),
    retract(turn(black)),
    asserta(turn(white)),
    check_win.

do(black, capture(Pawn, Goal)) :-
    legal(black, capture(Pawn, Goal)),
    board_set_cell_value(Pawn, empty),
    board_set_cell_value(Goal, black),
    retract(turn(black)),
    asserta(turn(white)),
    check_win.

/**
 * check_win()
 *
 * Check and assert if a player has won
 */
check_win :-
    board_get_cells_value([a8, b8, c8, d8, e8, f8, g8, h8], BlackHomeRow),
    member(white, BlackHomeRow),
    asserta(win(white)).

check_win :-
    board_get_cells_value([a1, b1, c1, d1, e1, f1, g1, h1], WhiteHomeRow),
    member(black, WhiteHomeRow),
    asserta(win(black)).
