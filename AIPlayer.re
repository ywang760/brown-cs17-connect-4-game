open! CS17SetupGame;
open Game;

module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame;
  /* TODO */
  open PlayerGame;

  /* a record, which contains a state, its estimate value, the move that yields
     the state, and the very first move*/
  type record = {
    initmove: move,
    thismove: move,
    state,
    eV: float,
  };

  /* nextMove: (state => move)
     input: s, a state
     output: the move to make after searching through all the possible outcomes
     and coming up with the optimal move with minimax algorithm */
  let nextMove: state => move =
    s => {
let nextStateHelper: state => list(float) = 
  s => {
  let rec nextStateHelper2: (state, list(move)) => list(float) = 
  (s, mov) =>
  switch(mov) {
  | [] => []
  | [mova] => [estimateValue(nextState(s, mova))]
  | [movhd, ...movtl] => [estimateValue(nextState(s, movhd)), ...nextStateHelper2(s, movtl)]
  }
  nextStateHelper2(s, legalMoves(s))
  }

let netStateH: state => list(state) = 
s => {
let rec netStateHe: (state, list(move)) => list(state) = 
(s, mov) =>
switch(mov) {
| [] => []
| [mova] => [nextState(s, mova)]
| [movhd, ...movtl] => [nextState(s, movhd), ...netStateHe(s, movtl)]
}
netStateHe(s, legalMoves(s))
}


let rec getMax: list(float) => float = 
alof => switch(alof) {
| [] => failwith("")
| [a] => a
| [hd, ...tl] =>
if (hd < getMax(tl)) {getMax(tl)} else {hd}
}

let rec getMin: list(float) => float = 
alof => switch(alof) {
| [] => failwith("")
| [a] => a
| [hd, ...tl] =>
if (hd > getMin(tl)) {getMin(tl)} else {hd}
}

let rec getMax2: list(state) => state = 
alof => switch(alof) {
| [] => failwith("")
| [a] => a
| [hd, ...tl] =>
if (estimateValue(hd) < estimateValue(getMax2(tl))) {getMax2(tl)} else {hd}
}

let rec getMin2: list(state) => state = 
alof => switch(alof) {
| [] => failwith("")
| [a] => a
| [hd, ...tl] =>
if (estimateValue(hd) > estimateValue(getMin2(tl))) {getMin2(tl)} else {hd}
}

let rec minimax: (state, int) => float = (s, d) => switch(gameStatus(s), d) {
  | (Ongoing(P1), 0) => getMax(nextStateHelper(s))
  | (Ongoing(P2), 0) => getMin(nextStateHelper(s)) 
  | (Win(P1), _) => 100000000000000000000000000.
  | (Win(P2), _) => -100000000000000000000000000.
  | (Draw, _) => 0.
  | (Ongoing(P1), n) => minimax(getMax2(netStateH(s)), n - 1)
  | (Ongoing(P2), n) => minimax(getMin2(netStateH(s)), n - 1)
}

let rec floattomove: float => move = f => failwith("we dont have time")

floattomove(minimax(s, 4))
    };

  /* put your team name here! */
  let playerName = "Spike the smart squirrel";
};

module TestGame = Connect4.Connect4;
open Player;

module TestAIPlayer = AIPlayer(TestGame);
module MyAIPlayer: Player = TestAIPlayer;
/* open TestAIPlayer; */
/* we don't have any test cases for AIPlayer because all of the helpers
   are nested, and nextMove itself takes in a state as input*/
/* insert test cases for any procedures that don't take in
 * or return a state here */