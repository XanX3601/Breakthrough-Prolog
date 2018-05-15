/** <module> Breakthrough console view

Console view of a breakthrough game

@author Thomas Petiteau
@version 1.0
*/
:- module(breakthrough_console_view, [start_a_game/0,
                                        board/0,
                                        turn/0,
                                        legal/0,
                                        move/2,
                                        capture/2,
                                        instructions/0]).

:- use_module(breakthrough).
:- use_module(board).

instructions :-
    write('Hello !\n   start_a_game -> start a new game\n   board -> display the board\n   turn -> display the current player\n   legal -> display the moves legal for the current player\n   move(Pawn, Goal) -> move a pawn to the given goal\n   capture(Pawn, Goal) -> the given pawn capture an enemy pawn on goal\n').

/**
 * start_a_game()
 *
 * Start a game
 */
start_a_game :-
    init_normal_game.

/**
 * board()
 *
 * Display the board
 */
board :-
    board_get_cells_value([a8, b8, c8, d8, e8, f8, g8, h8, a7, b7, c7, d7, e7, f7, g7, h7, a6, b6, c6, d6, e6, f6, g6, h6, a5, b5, c5, d5, e5, f5, g5, h5, a4, b4, c4, d4, e4, f4, g4, h4, a3, b3, c3, d3, e3, f3, g3, h3, a2, b2, c2, d2, e2, f2, g2, h2, a1, b1, c1, d1, e1, f1, g1, h1], CellsValue),
    values_str(CellsValue, ValuesStr),
    atomics_to_string(['    a   b   c   d   e   f   g   h\n',
                       '  \u2554\u2550\u2550\u2550\u2564\u2550\u2550\u2550\u2564\u2550\u2550\u2550\u2564\u2550\u2550\u2550\u2564\u2550\u2550\u2550\u2564\u2550\u2550\u2550\u2564\u2550\u2550\u2550\u2564\u2550\u2550\u2550\u2557\n',
                       '8 \u2551 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2551\n',
                       '  \u255F\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u2562\n',
                       '7 \u2551 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2551\n',
                       '  \u255F\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u2562\n',
                       '6 \u2551 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2551\n',
                       '  \u255F\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u2562\n',
                       '5 \u2551 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2551\n',
                       '  \u255F\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u2562\n',
                       '4 \u2551 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2551\n',
                       '  \u255F\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u2562\n',
                       '3 \u2551 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2551\n',
                       '  \u255F\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u2562\n',
                       '2 \u2551 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2551\n',
                       '  \u255F\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u253C\u2500\u2500\u2500\u2562\n',
                       '1 \u2551 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2502 %w \u2551\n',
                       '  \u255A\u2550\u2550\u2550\u2567\u2550\u2550\u2550\u2567\u2550\u2550\u2550\u2567\u2550\u2550\u2550\u2567\u2550\u2550\u2550\u2567\u2550\u2550\u2550\u2567\u2550\u2550\u2550\u2567\u2550\u2550\u2550\u255D\n'], Output),
    writef(Output, ValuesStr).

/**
 * turn()
 *
 * Display the name of the current player
 */
turn :-
    breakthrough:turn(Player),
    write(Player), nl.

/**
 * legal()
 *
 * Display legal moves for current player
 */
legal :-
    breakthrough:turn(Player),
    breakthrough:legal(Player, X),
    write(X), nl.

/**
 * move(+Pawn, +Goal)
 *
 * move the given pawn to given goal if legal
 *
 * @param Pawn a cell
 * @param Goal a cell
 */
move(Pawn, Goal) :-
    breakthrough:turn(Player),
    not(legal(Player, move(Pawn, Goal))),
    write('Illegal move\n').

move(Pawn, Goal) :-
    breakthrough:turn(Player),
    legal(Player, move(Pawn, Goal)),
    not(do(Player, move(Pawn, Goal))).

/**
 * capture(+Pawn, +Goal)
 *
 * Capure the pawn on goal using th given pawn if legal
 *
 * @param Pawn a cell
 * @param Goal a cell
 */
capture(Pawn, Goal) :-
    breakthrough:turn(Player),
    not(legal(Player, capture(Pawn, Goal))),
    write('Illegal capture\n').

capture(Pawn, Goal) :-
    breakthrough:turn(Player),
    legal(Player, capture(Pawn, Goal)),
    not(do(Player, capture(Pawn, Goal))).

/**
 * values_str(+Values: list, -ValuesStr: list)
 *
 * Convert from a list of values to same list with string representation of every value
 *
 * @Values a list of values
 * @ValuesStr a list of string representation of values
 */
values_str([], []).
values_str([Value|Values], [Str|Strs]) :-
    value_str(Value, Str),
    values_str(Values, Strs).

/**
 * value_str(Value, String)
 *
 * Convert between a value and its string representation
 *
 * @Value a value
 * @String string representation of the value
 */
value_str(empty, " ").
value_str(black, "b").
value_str(white, "w").

