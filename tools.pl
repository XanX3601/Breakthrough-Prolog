/** <module> Tools

This module provide different tools

@author Thomas Petiteau
@version 1.0
*/
:- module(tools, [list_build/3,
                    replace/4]).

/**
 * list_build(+Element: term, +Lenght: int, -List: list)
 *
 * Build a list contaning Lenght time Element
 *
 * @param Element the element contained by List Lenght times
 * @param Lenght the length of the lits to build
 * @param List the list to build
 */
list_build(Element, Length, List) :-
    length(List, Length),
    maplist(=(Element), List).

/**
 * replace(+List: list, +Index: int, +NewElement: term, -NewList: list)
 *
 * Replace the element at the given Index in List by NewElement
 * The index start at 0
 *
 * @param List the list to modify
 * @param Index the index of the element to replace
 * @param NewElement the element coming in replacement
 * @param NewList the list after modification
 */
replace([_|Tail], 0, NewElement, [NewElement|Tail]).
replace([Head|Tail], Index, NewElement, [Head|NewTail]) :-
    Index > 0,
    IndexMinusOne is Index - 1,
    replace(Tail, IndexMinusOne, NewElement, NewTail).
