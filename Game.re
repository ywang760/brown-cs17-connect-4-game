module type Game = {

    /* specifies a player */
    type whichPlayer = P1 | P2;

    /* status of game: if it's over (and who won) or ongoing (and who's turn) */
    type status =
       | Win(whichPlayer)
       | Draw
       | Ongoing(whichPlayer);

    /* the state of the game: the position, status, anything else associated
    with the game at a given turn */
    type state;

    /* describes a move that a player can make */
    type move;

    /* printing functions */
    /* returns the string representation of the inputted player*/
    let stringOfPlayer: whichPlayer => string;
    /* returns a string representation of the inputted state*/
    let stringOfState: state => string;
    /* returns a string representation of the inputted move*/
    let stringOfMove: move => string;

    /* Game Logic */

    /* the state of the game when it begins */
    /* input: a string denoting the state of the game when it begins */
    /* output: the state of the game when it begins */
    let initialState: string => state;

    /* produces the list of legal moves at a state */
    let legalMoves: state => list(move);

    /* returns the status of the game at the given state */
    let gameStatus: state => status;

    /* given a state and a legal move, yields the next state */
    let nextState: (state, move) => state;

    /* for transforming human player input into
    internal representation of move */
    let moveOfString: (string, state) => move;

    /* estimates the value of a given state (static evaluation) */
    let estimateValue: state => float;
};
