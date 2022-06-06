open! CS17SetupGame;
open Game;

module Connect4 = {
  /* player 1 is P1, player 2 is P2 */
  type whichPlayer =
    | P1
    | P2;

  /* either a player has won, it's a draw, or it's ongoing */
  type status =
    | Win(whichPlayer)
    | Draw
    | Ongoing(whichPlayer);

  /* the state of the game: the position, status, anything else associated
     with the game at a given turn */
  type state =
    | State(status, list(list(int)));

  /* describes a move that a player can make */
  type move =
    | Move(int);

  /* transformation helpers */

  /* transpose: list(list(int)) => list(list(int))
   * input: mat, a board, represented as a list of list of int
   * output: the transpose of the input board
   *
   * Recursion diagram 1:
   * OI: []
   * RI: N/A
   * RO: N/A
   * failwith("A matrix cannot be 0-dimensional.")
   * OO: failwith("A matrix cannot be 0-dimensional.")
   *
   * Recursion diagram 2:
   * OI: [[], [], []]
   * RI: N/A
   * RO: N/A
   * failwith("A matrix cannot be 0-dimensional.")
   * OO: failwith("A matrix cannot be 0-dimensional.")
   *
   * Recursion diagram 3:
   * OI: [[1], [2], [3]]
   * RI: [[2], [3]]
   * RO: [[2, 3]]
   * List of one-element lists:
   * OO: [[1, 2, 3]]
   *
   * Recursion diagram 4:
   * OI: [[1, 2, 3], [4, 5, 6]]
   * RI: [[2, 3], [5, 6]]
   * RO: [[2, 5], [3, 6]
   * cons(map (head), mat) to RO
   * OO: [[1, 4], [2, 5], [3, 6]]
   *
   * Recursion diagram 5:
   * OI: [[1, 2], [3, 4], [5, 6]]
   * RI: [[2], [4], [6]]
   * RO: [[2, 4, 6]]
   * cons(map (head), mat) to RO
   * OO: [[1, 3, 5], [2, 4, 6]]
   */

  let rec transpose: list(list(int)) => list(list(int)) =
    mat =>
      switch (mat) {
      | []
      | [[], ..._] => failwith("A matrix cannot be 0-dimensional.")
      | [[_hd], ..._] => [List.flatten(mat)]
      | [[_hd, ..._tl], ..._] => [
          List.map(List.hd, mat),
          ...transpose(List.map(List.tl, mat)),
        ]
      };

  /* duplicate: (('a), int) => (list ('a))
   * input: (alod, int), an pair consisting of an 'a and an int
   * output: a list of 'a that consists of the 'a repeating int times
   * Recursion diagram 1:
   * OI: ("f", 0)
   * RI: n/a
   * RO: n/a
   * output [] when int is 0
   * OO: []
   *
   * Recursion diagram 2:
   * OI: (false, 2)
   * RI: (false, 1)
   * RO: [false]
   * cons false to RO
   * OO: [false, false] */

  let rec duplicate: ('a, int) => list('a) =
    (alod, int) =>
      switch (int) {
      | 0 => []
      | _ => [alod, ...duplicate(alod, int - 1)]
      };

  /* shear: list(list(int)) => list(list(int))
   * input: mat, board represented as an list of list of int
   * output: a list of list of int that represents the board after the shear
   * transformation (shifting each sublist one unit, filling up the empty spots
   * with the number 3)
   *
   * Recursion diagram 1:
   * OI: []
   * RI: N/A
   * RO: N/A
   * failwith("A matrix cannot be 0-dimensional.")
   * OO: failwith("A matrix cannot be 0-dimensional.")
   *
   * Recursion diagram 2:
   * OI: [[], [], []]
   * RI: N/A
   * RO: N/A
   * failwith("A matrix cannot be 0-dimensional.")
   * OO: failwith("A matrix cannot be 0-dimensional.")
   *
   * Recursion diagram 3:
   * OI: [[1, 2, 3]]
   * RI: n/a
   * RO: n/a
   * output the input
   * OO: [[1, 2, 3]]
   *
   * Recursion diagram 4:
   * OI: [[1, 1, 1], [1, 1, 1]]
   * RI: [[1, 1, 1]]
   * RO: [[1, 1, 1]]
   * map the procedure of adding 3 in front of a list to RO, and cons the
   * head of the list extended by duplicates of 3 with the number equal to
   * the length of the tail in OI
   * OO: [[1, 1, 1, 3], [3, 1, 1, 1]]
   */

  let rec shear: list(list(int)) => list(list(int)) =
    mat =>
      switch (mat) {
      | []
      | [[], ..._] => failwith("A matrix cannot be 0-dimensional.")
      | [[hd, ...tl]] => [[hd, ...tl]]
      | [hd, ...tl] => [
          List.append(hd, duplicate(3, List.length(tl))),
          ...List.map(alod => [3, ...alod], shear(tl)),
        ]
      };

  /* initialState: string => state
   * input: s, a string denoting the state of the game when it begins
   * output: the state of the game when it begins */

  let initialState: string => state =
    s => {
      let boardDims = parseBoardDims(s);
      let boardHeight = getBoardHeight(boardDims);
      let boardWidth = getBoardWidth(boardDims);
      State(Ongoing(P1), duplicate(duplicate(0, boardHeight), boardWidth));
    };

  /* stringOfPlayer: whichPlayer => string
   * print the player into a string
   * input: py, either P1 or P2
   * output: Player1 if the input is P1, Palyer2 if the input is P2 */

  let stringOfPlayer: whichPlayer => string =
    py =>
      switch (py) {
      | P1 => "Player1"
      | P2 => "Player2"
      };

  /* stringOfState: state => string
   * input: inState, a state which consists of the board
   * output: a string representation of the current board */

  let stringOfState: state => string =
    inState => {
      let State(_, board) = inState;

      /* printcolumn: list(int) => string
       * input: column, a list of int representing a column
       * output: the ints in the column, separated by spaces */

      let rec printcolumn: list(int) => string =
        column =>
          switch (column) {
          | [] => ""
          | [a] => string_of_int(a)
          | [hd, ...tl] => string_of_int(hd) ++ " " ++ printcolumn(tl)
          };

      /* printboard: list(list(int)) => string
       * input: board, a list of list of representing a board
       * output: a string representation of the board, where columns are
       * separated by line breaks */

      let rec printboard: list(list(int)) => string =
        board =>
          switch (board) {
          | [] => ""
          | [hd, ...tl] => printcolumn(hd) ++ "\n" ++ printboard(tl)
          };
      printboard(transpose(List.map(List.rev, board)));
    };

  /* stringOfMove: move => string
   * input: mov, a move
   * output: the string representation of the move by taking the integer within
   */

  let stringOfMove: move => string =
    fun
    | Move(int) => string_of_int(int);

  /* legalMoves: state => list(move)
   * input: inState, a state with a board
   * output: a list of moves that is legal within that board (i.e., the columns
   * of the board that are not completely filled up)
   *
   * legalHelper: (list(list(int)), int) => list(move)
   * input: (cols, n), a list of list of int representing the board, and an
   * integer used to test whether the columns in the board contains n or not,
   * in this case, the int is 0
   * output: a list of moves generated from the list of columns that contains
   * a 0 in it, which means it's not full and therefore a legal move can be
   * placed in the column */

  let legalMoves: state => list(move) =
    inState => {
      let rec legalHelper: (list(list(int)), int) => list(move) =
        (cols, n) =>
          switch (cols) {
          | [hd, ...tl] =>
            if (List.mem(0, hd)) {
              [Move(n + 1), ...legalHelper(tl, n + 1)];
            } else {
              legalHelper(tl, n + 1);
            }
          | _ => []
          };
      switch (inState) {
      | State(_, []) => []
      | State(_, a) => legalHelper(a, 0)
      };
    };

  /* gameStatus: state => status
   * input: inState, a state, which consists a status
   * output: the status within the state */

  let gameStatus: state => status =
    inState => {
      let State(p, _) = inState;
      p;
    };

  /* nextState: (state, move) => state
   * input: (inState, mov), a pair consisting a state and a move
   * output: an updated state with a new board that has the move added to it */

  let nextState: (state, move) => state =
    (inState, mov) => {
      /* addingAPieceHelper: (list(int), int) => list(int)
       * input: (alod, n), a pair consisting of a list of int and an int, which
       * represents a column and a whichplayer's move
       * output: an updated list of int, representing the updated column with n
       * added to it */
      let rec addingAPieceHelper: (list(int), int) => list(int) =
        (alod, n) =>
          switch (alod) {
          | [] => failwith("invalid move")
          | [hd, ...tl] =>
            if (hd == 0) {
              [n, ...tl];
            } else {
              [hd, ...addingAPieceHelper(tl, n)];
            }
          };

      /* addingAPiece: (list(list(int)), move, int) => list(list(int))
       * input: (board, Move(mov), num), a list of list of integer representing
       * board, a move, and an integer representing a whichplayer's move
       * output: a list of list of integer representing an updated board, with
       * the move added to the board */

      let rec addingAPiece: (list(list(int)), move, int) => list(list(int)) =
        (board, Move(mov), num) =>
          switch (board, mov) {
          | ([], _) => failwith("invalid move")
          | ([hd, ...tl], 1) => [addingAPieceHelper(hd, num), ...tl]
          | ([hd, ...tl], a) => [
              hd,
              ...addingAPiece(tl, Move(a - 1), num),
            ]
          };

      /* columnWinDetectorHelper: list(int) => bool
       * input: alod, a list of int representing a column
       * output: a bool, true if there're four 1s or 2s in a row,
       *         false otherwise */

      let rec columnWinDetectorHelper: list(int) => bool =
        alod =>
          switch (alod) {
          | [] => false
          | [1, 1, 1, 1, ..._tl] => true
          | [2, 2, 2, 2, ..._tl] => true
          | [_hd, ...tl] => columnWinDetectorHelper(tl)
          };

      /* columnWinDetector: list(list(int)) => bool
       * input: board, a list of list of int representing a board
       * output: a bool, true if there is a vertical win in at least one of the
       * columns, false otherwise */

      let columnWinDetector: list(list(int)) => bool =
        board => List.exists(columnWinDetectorHelper, board);

      /* winDetector: list(list(int)) => bool
       * input: board, a list of list of int representing a board
       * output: a bool, true if there is a win of any king in the board, false
       * otherwise */

      let winDetector: list(list(int)) => bool =
        board =>
          columnWinDetector(board)
          || columnWinDetector(transpose(board))
          || columnWinDetector(transpose(shear(board)))
          || columnWinDetector(transpose(shear(List.map(List.rev, board))));

      let State(_, bd) = inState;
      switch (legalMoves(inState)) {
      | [] => State(Draw, bd)
      | _ =>
        switch (inState, mov) {
        | (State(Win(_), _), _) => inState
        | (State(Draw, _), _) => inState
        | (State(Ongoing(P1), board), col) =>
          let b1 = addingAPiece(board, col, 1);
          if (winDetector(b1)) {
            State(Win(P1), b1);
          } else if (legalMoves(State(Ongoing(P2), b1)) == []) {
            State(Draw, b1);
          } else {
            State(Ongoing(P2), b1);
          };
        | (State(Ongoing(P2), board), col) =>
          let b2 = addingAPiece(board, col, 2);
          if (winDetector(b2)) {
            State(Win(P2), b2);
          } else if (legalMoves(State(Ongoing(P1), b2)) == []) {
            State(Draw, b2);
          } else {
            State(Ongoing(P1), b2);
          };
        }
      };
    };

  /* moveOfString: (string, state) => move
   * input: (str, inState), a pair consists of a string and a state
   * output: if the move is not a legal move, fail it,
   * otherwise create the Move representation of the string */

  let moveOfString: (string, state) => move =
    (str, inState) =>
      switch (int_of_string(str), legalMoves(inState)) {
      | (x, y) =>
        if (List.mem(Move(x), y)) {
          Move(x);
        } else {
          failwith("not a legal move");
        }
      };

  /* score: (list(list(int)), float, int) => float
   * Input: lst, a list of list of integers, representing the board (might be
   *     transposed or sheared)that we are estimating
   * num, a float, representing the initial value
   * n, an integer, representing the current player
   * Output: a float representing the estimate value of the current board with
   *   the current player
   *
   * recursion diagram 1:
   * Original input: ([[1, 1, 2, 0, 0], [1, 0, 0, 0, 0], [2, 2, 0, 0, 0]]
   *                , 0., 1)
   *   Recursive input: ([[1, 0, 0, 0, 0], [2, 2, 0, 0, 0]], 0., 1)
   *   Recursive output: 10.
   *     Ideas: calculate the value of the first list in lst with helper search
   *         and add it to the RO
   *   Original output: 10.
   *
   * recursion diagram 2:
   * Original input: ([[1, 1, 2, 0, 0], [1, 0, 0, 0, 0], [2, 2, 0, 0, 0]]
   *                , 0., 2)
   *   Recursive input: ([[1, 0, 0, 0, 0], [2, 2, 0, 0, 0]], 0., 2)
   *   Recursive output: -110.
   *     Ideas: calculate the value of the first list in lst with helper search
   *         and add it to the RO
   *   Original output: -110.
   *
   * search: (list(int), float, int) => float
   * Input: lst, a list of integers, representing a row or column (might be
   *     transposed or sheared) of the board that we are estimating
   * num, a float, representing the initial value
   * n, an integer, representing the current player
   * Output: a float representing the estimate value of lst with the current
   *     player
   *
   * recursion diagram 1:
   * Original input: ([1, 1, 0, 0, 0], 0., 1)
   *   Recursive input: ([1, 0, 0, 0], 100., 1)
   *   Recursive output: 110.
   *     Ideas: N/A
   *   Original output: 110.
   *
   * recursion diagram 2:
   * Original input: ([2, 2, 2, 0, 1, 0, 0], 0., 2)
   *   Recursive input: ([2, 2, 0, 1, 0, 0], -500000., 2)
   *   Recursive output: -500000.
   *     Ideas: N/A
   *   Original output: -500000.
   *
   * member: (list(int), list(int)) => bool
   * Input: lst, a list of integer
   * sublst, a list of integer that should be shorter than lst
   * Output: true if sublst is a sublist of lst (i.e. all elements in sublst
   *     are in lst in the particular order, but are not necessarily adjacent),
   *     false if otherwise
   *
   * recursion diagram 1:
   * Original input: ([1, 1, 2, 0, 0], [1, 2])
   *   Recursive input: ([1, 2, 0, 0], [2])
   *   Recursive output: true
   *     Ideas: N/A
   *   Original output: true
   *
   * recursion diagram 2:
   * Original input: ([1, 1, 2, 0, 0], [0, 0])
   *   Recursive input: ([1, 2, 0, 0], [0, 0])
   *   Recursive output: true
   *     Ideas: N/A
   *   Original output: true */

  let rec score: (list(list(int)), float, int) => float =
    (lst, num, n) =>
      switch (lst) {
      | [] => num
      | [hd, ...tl] =>
        let rec search: (list(int), float, int) => float = (
          (lst, num, n) =>
            switch (lst) {
            | [_hd1, _hd2, _hd3] => num
            | [hd1, hd2, hd3, hd4, ...tl] =>
              let rec member: (list(int), list(int)) => bool = (
                (lst, sublst) =>
                  switch (lst, sublst) {
                  | ([], _) => false
                  | ([hd1, ...tl], [hd2]) =>
                    if (hd1 == hd2) {
                      true;
                    } else {
                      member(tl, [hd2]);
                    }
                  | ([hd1, ...tl1], [hd2, ...tl2]) =>
                    if (hd1 == hd2) {
                      member(tl1, tl2);
                    } else {
                      member(tl1, sublst);
                    }
                  | _ => failwith("sublst cannot be empty")
                  }
              );
              if (n == 1) {
                if (member([hd1, hd2, hd3, hd4], [2])) {
                  search([hd2, hd3, hd4, ...tl], num, n);
                } else if ([hd1, hd2, hd3, hd4] == [0, 1, 1, 1]
                           || [hd1, hd2, hd3, hd4] == [1, 1, 1, 0]) {
                  search([hd2, hd3, hd4, ...tl], num +. 500000., n);
                } else if (member([hd1, hd2, hd3, hd4], [1, 1, 1])) {
                  search([hd2, hd3, hd4, ...tl], num +. 100000., n);
                } else if (member([hd1, hd2, hd3, hd4], [1, 1])) {
                  search([hd2, hd3, hd4, ...tl], num +. 100., n);
                } else if (member([hd1, hd2, hd3, hd4], [1])) {
                  search([hd2, hd3, hd4, ...tl], num +. 10., n);
                } else {
                  search([hd2, hd3, hd4, ...tl], num, n);
                };
              } else if (n == 2) {
                if (member([hd1, hd2, hd3, hd4], [1])) {
                  search([hd2, hd3, hd4, ...tl], num, n);
                } else if ([hd1, hd2, hd3, hd4] == [0, 2, 2, 2]
                           || [hd1, hd2, hd3, hd4] == [2, 2, 2, 0]) {
                  search([hd2, hd3, hd4, ...tl], num -. 500000., n);
                } else if (member([hd1, hd2, hd3, hd4], [2, 2, 2])) {
                  search([hd2, hd3, hd4, ...tl], num -. 100000., n);
                } else if (member([hd1, hd2, hd3, hd4], [2, 2])) {
                  search([hd2, hd3, hd4, ...tl], num -. 100., n);
                } else if (member([hd1, hd2, hd3, hd4], [2])) {
                  search([hd2, hd3, hd4, ...tl], num -. 10., n);
                } else {
                  search([hd2, hd3, hd4, ...tl], num, n);
                };
              } else {
                failwith("there are only 2 players");
              };
            | _ => failwith("the list is too short")
            }
        );
        score(tl, num, n) +. search(hd, 0., n);
      };

  /* estimateValue: state => float
   * Input: inState, a state that we want to estimate
   * Output: a float representing the estimate value of the current state */

  let estimateValue: state => float =
  inState =>
    switch (inState) {
    | State(Win(P1), _) => 100000000000000000000000000.
    | State(Win(P2), _) => (-100000000000000000000000000.)
    | State(Draw, _) => 0.
    | State(Ongoing(_), board) =>
      score(board, 0., 1)
      +. 1.5
      *. score(transpose(board), 0., 1)
      +. 2.
      *. (
        score(transpose(shear(board)), 0., 1)
        +. score(transpose(shear(List.map(List.rev, board))), 0., 1)
      )
      +. score(board, 0., 2)
      +. 1.5
      *. score(transpose(board), 0., 2)
      +. 2.
      *. (
        score(transpose(shear(board)), 0., 2)
        +. score(transpose(shear(List.map(List.rev, board))), 0., 2)
      )
    };
};

module MyGame: Game = Connect4;
open Connect4;

/* test cases */
/* transpose */
checkExpect(
  transpose([[0, 0], [0, 0], [0, 0], [0, 0]]),
  [[0, 0, 0, 0], [0, 0, 0, 0]],
  "2*4",
);
checkExpect(
  transpose([
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
  ]),
  [
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
  ],
  "6*5",
);
checkExpect(transpose([[0], [0]]), [[0, 0]], "1*2");
checkError(() => transpose([]), "A matrix cannot be 0-dimensional.");
checkError(() => transpose([[], []]), "A matrix cannot be 0-dimensional.");
/* duplicate */
checkExpect(duplicate("a", 3), ["a", "a", "a"], "duplicate a 3 times");
checkExpect(duplicate("a", 0), [], "duplicate a 0 times");
checkExpect(duplicate(2, 5), [2, 2, 2, 2, 2], "duplicate 2 5 times");
/* shear */
checkExpect(shear([[1, 1]]), [[1, 1]], "shear 1");
checkExpect(
  shear([[1, 1, 1], [1, 1, 1], [1, 1, 1]]),
  [[1, 1, 1, 3, 3], [3, 1, 1, 1, 3], [3, 3, 1, 1, 1]],
  "shear 2",
);
checkError(() => shear([]), "A matrix cannot be 0-dimensional.");
checkError(() => shear([[], []]), "A matrix cannot be 0-dimensional.");
/* initialState */
checkExpect(
  initialState("2 4"),
  State(Ongoing(P1), [[0, 0], [0, 0], [0, 0], [0, 0]]),
  "2*4 board",
);
checkExpect(
  initialState("6 5"),
  State(
    Ongoing(P1),
    [
      [0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0],
    ],
  ),
  "6*5 board",
);
/* stringOfPlayer */
checkExpect(stringOfPlayer(P1), "Player1", "player 1");
checkExpect(stringOfPlayer(P2), "Player2", "player 1");
/* stringOfState */
checkExpect(
  stringOfState(State(Ongoing(P1), [[1, 1], [1, 0], [1, 0], [0, 0]])),
  "1 0 0 0\n1 1 1 0\n",
  "2*4",
);
checkExpect(
  stringOfState(State(Draw, [[1, 2, 1], [2, 1, 2], [1, 2, 1]])),
  "1 2 1\n2 1 2\n1 2 1\n",
  "3*3",
);
/* stringOfMove */
checkExpect(stringOfMove(Move(2)), "2", "stringOfMove2");
checkExpect(stringOfMove(Move(7)), "7", "stringOfMove7");
/* legalMoves */
checkExpect(
  legalMoves(State(Ongoing(P1), [[0, 0], [0, 0], [0, 0], [0, 0]])),
  [Move(1), Move(2), Move(3), Move(4)],
  "legal move for  2*4 board1",
);
checkExpect(
  legalMoves(State(Ongoing(P1), [[1, 2], [0, 0], [0, 0], [0, 0]])),
  [Move(2), Move(3), Move(4)],
  "legal move for  2*4 board2",
);
checkExpect(
  legalMoves(State(Ongoing(P1), [[1], [1], [1]])),
  [],
  "legal move for  1*3 board",
);
/* gameStatus */
checkExpect(
  gameStatus(State(Ongoing(P1), [[0, 0], [0, 0], [0, 0], [0, 0]])),
  Ongoing(P1),
  "ongoing player 1",
);
checkExpect(
  gameStatus(State(Ongoing(P2), [[1, (-1)], [0, 0], [1, 0], [0, 0]])),
  Ongoing(P2),
  "ongoing player 2",
);
/* nextState */
checkExpect(
  nextState(
    State(Win(P1), [[1, 2], [1, 2], [1, 2], [1, 0]]),
    Move(2),
  ),
  State(Win(P1), [[1, 2], [1, 2], [1, 2], [1, 0]]),
  "already winp1",
);
checkExpect(
  nextState(State(Draw, [[1, 2], [1, 2], [1, 2], [1, 2]]), Move(3)),
  State(Draw, [[1, 2], [1, 2], [1, 2], [1, 2]]),
  "already draw",
);
checkExpect(
  nextState(
    State(Ongoing(P1), [[0, 0], [0, 0], [0, 0], [0, 0]]),
    Move(1),
  ),
  State(Ongoing(P2), [[1, 0], [0, 0], [0, 0], [0, 0]]),
  "ongoing1",
);
checkExpect(
  nextState(
    State(Ongoing(P2), [[1, 0], [0, 0], [0, 0], [0, 0]]),
    Move(3),
  ),
  State(Ongoing(P1), [[1, 0], [0, 0], [2, 0], [0, 0]]),
  "ongoing2",
);
checkExpect(
  nextState(
    State(Ongoing(P1), [[1, 2], [1, 2], [1, 2], [0, 0]]),
    Move(4),
  ),
  State(Win(P1), [[1, 2], [1, 2], [1, 2], [1, 0]]),
  "win horizontal",
);
checkExpect(
  nextState(
    State(
      Ongoing(P2),
      [
        [1, 1, 1, 2, 0, 0],
        [1, 1, 2, 0, 0, 0],
        [2, 0, 0, 0, 0, 0],
        [2, 1, 0, 0, 0, 0],
        [2, 0, 0, 0, 0, 0],
      ],
    ),
    Move(3),
  ),
  State(
    Win(P2),
    [
      [1, 1, 1, 2, 0, 0],
      [1, 1, 2, 0, 0, 0],
      [2, 2, 0, 0, 0, 0],
      [2, 1, 0, 0, 0, 0],
      [2, 0, 0, 0, 0, 0],
    ],
  ),
  "win diagonal",
);
checkExpect(
  nextState(
    State(
      Ongoing(P1),
      [
        [1, 1, 1, 0, 0, 0],
        [1, 2, 0, 0, 0, 0],
        [2, 0, 0, 0, 0, 0],
        [2, 0, 0, 0, 0, 0],
        [2, 0, 0, 0, 0, 0],
      ],
    ),
    Move(1),
  ),
  State(
    Win(P1),
    [
      [1, 1, 1, 1, 0, 0],
      [1, 2, 0, 0, 0, 0],
      [2, 0, 0, 0, 0, 0],
      [2, 0, 0, 0, 0, 0],
      [2, 0, 0, 0, 0, 0],
    ],
  ),
  "win vertical",
);
checkExpect(
  nextState(
    State(Ongoing(P2), [[1, 1], [2, 2], [2, 1], [1, 0]]),
    Move(4),
  ),
  State(Draw, [[1, 1], [2, 2], [2, 1], [1, 2]]),
  "draw",
);
checkError(
  () =>
    nextState(
      State(Ongoing(P2), [[1, 1], [2, 2], [2, 1], [1, 0]]),
      Move(1),
    ),
  "invalid move",
);
/* move of string*/
checkExpect(
  moveOfString(
    "2",
    State(
      Ongoing(P2),
      [
        [1, 1, 1, 2, 0, 0],
        [1, 1, 2, 0, 0, 0],
        [2, 0, 0, 0, 0, 0],
        [2, 1, 0, 0, 0, 0],
        [2, 0, 0, 0, 0, 0],
      ],
    ),
  ),
  Move(2),
  "move of string",
);
checkError(
  () =>
    moveOfString(
      "2",
      State(
        Ongoing(P2),
        [
          [1, 1, 1, 2, 0, 0],
          [1, 1, 2, 1, 2, 1],
          [2, 0, 0, 0, 0, 0],
          [2, 1, 0, 0, 0, 0],
          [2, 0, 0, 0, 0, 0],
        ],
      ),
    ),
  "not a legal move",
);
/* score */
checkExpect(
  score(
    [
      [1, 1, 1, 2, 0, 0],
      [1, 1, 2, 0, 0, 0],
      [2, 0, 0, 0, 0, 0],
      [2, 1, 0, 0, 0, 0],
      [2, 0, 0, 0, 0, 0],
    ],
    0.,
    1,
  ),
  10.,
  "P1",
);
checkExpect(
  score(
    [
      [1, 1, 1, 0, 0, 0],
      [1, 2, 0, 0, 0, 0],
      [2, 0, 0, 0, 0, 0],
      [2, 0, 0, 0, 0, 0],
      [2, 0, 0, 0, 0, 0],
    ],
    0.,
    1,
  ),
  500110.,
  "P1",
);
checkExpect(
  score(
    [
      [1, 1, 0, 0, 0, 0],
      [1, 0, 0, 0, 0, 0],
      [2, 0, 0, 0, 0, 0],
      [2, 0, 0, 0, 0, 0],
      [2, 0, 0, 0, 0, 0],
    ],
    0.,
    1,
  ),
  120.,
  "P1",
);
checkExpect(
  score(
    [
      [1, 1, 1, 2, 0, 0],
      [1, 1, 2, 0, 0, 0],
      [2, 0, 0, 0, 0, 0],
      [2, 1, 0, 0, 0, 0],
      [2, 0, 0, 0, 0, 0],
    ],
    0.,
    2,
  ),
  -30.,
  "P2",
);
checkExpect(
  score(
    [
      [1, 1, 2, 2, 0, 0],
      [1, 1, 2, 0, 0, 0],
      [2, 0, 0, 0, 0, 0],
      [2, 1, 1, 0, 0, 0],
      [2, 0, 0, 0, 0, 0],
    ],
    0.,
    2,
  ),
  -130.,
  "P2",
);
checkError(
  () => score([[1, 1], [2, 2], [2, 1], [1, 0]], 0., 1),
  "the list is too short",
);
checkError(
  () =>
    score(
      [
        [1, 1, 2, 2, 0, 0],
        [1, 1, 2, 0, 0, 0],
        [2, 0, 0, 0, 0, 0],
        [2, 1, 1, 0, 0, 0],
        [2, 0, 0, 0, 0, 0],
      ],
      0.,
      3,
    ),
  "there are only 2 players",
);
/* estimate value */
checkExpect(
  estimateValue(
    State(
      Ongoing(P2),
      [
        [1, 1, 1, 2, 0, 0],
        [1, 1, 2, 0, 0, 0],
        [2, 0, 0, 0, 0, 0],
        [2, 1, 0, 0, 0, 0],
        [2, 0, 0, 0, 0, 0],
      ],
    ),
  ),
  -49660.,
  "P2",
);
checkExpect(
  estimateValue(
    State(
      Ongoing(P1),
      [
        [1, 1, 1, 0, 0, 0],
        [1, 2, 0, 0, 0, 0],
        [2, 0, 0, 0, 0, 0],
        [2, 0, 0, 0, 0, 0],
        [2, 0, 0, 0, 0, 0],
      ],
    ),
  ),
  500010.,
  "P1",
);
checkExpect(
  estimateValue(
    State(
      Ongoing(P1),
      [
        [1, 1, 0, 0, 0, 0],
        [1, 0, 0, 0, 0, 0],
        [2, 0, 0, 0, 0, 0],
        [2, 0, 0, 0, 0, 0],
        [2, 0, 0, 0, 0, 0],
      ],
    ),
  ),
  225.,
  "P1",
);
checkExpect(
  estimateValue(
    State(
      Win(P1),
      [
        [1, 1, 1, 1, 0, 0],
        [2, 0, 0, 0, 0, 0],
        [2, 0, 0, 0, 0, 0],
        [2, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0],
      ],
    ),
  ),
  100000000000000000000000000.,
  "P1",
);
checkExpect(
  estimateValue(
    State(
      Win(P2),
      [
        [2, 2, 2, 2, 0, 0],
        [1, 1, 0, 0, 0, 0],
        [1, 0, 0, 0, 0, 0],
        [1, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0],
      ],
    ),
  ),
  -100000000000000000000000000.,
  "P2",
);
checkError(
  () =>
    estimateValue(
      State(Ongoing(P2), [[1, 1], [2, 2], [2, 1], [1, 0]]),
    ),
  "the list is too short",
);