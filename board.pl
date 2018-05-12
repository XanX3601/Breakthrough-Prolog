/** <module> Board

This module provides predicate to manipulate a 2D game board

@author Thomas Petiteau
@version 1.0
*/
:- module(board, [board_new/2,
					board_width/1,
					board_height/1]).

/**
 * board_new(+Width:int, +Height: int)
 *
 * Create a new board
 * @param Width the width of the board, between 1 and 26
 * @param Height the height of the board, superior or equal to 1
 */
board_new(Width, Height) :-
	not(board_width(X)),
	Width > 0,
	Width < 27,
	Height > 0,
	asserta(board_width(Width)),
	asserta(board_height(Height)).

board_new(Width, Height) :-
	Width > 0,
	Width < 27,
	Height > 0,

	board_width(PreWidth),
	board_height(PreHeight),
	retract(board_width(PreWidth)),
	retract(board_height(PreHeight)),

	asserta(board_width(Width)),
	asserta(board_height(Height)).

/**
 * board_width(-Width: int)
 *
 * Return the width of the board
 * @param Width the width of the board
 */
:- dynamic board_width/1.

/**
 * board_height(-Height: int)
 *
 * Return the height of the board
 * @param Height the height of the board
 */
:- dynamic board_height/1.