/** <module> Tools

This module provide different tools

@author Thomas Petiteau
@version 1.0
*/
:- module(tools, [list_build/3]).

/**
 * list_build(+Element, +Lenght: int, -List: list)
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
