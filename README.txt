1. In order to play against a friend (two humans play against each other), 
one first needs to access the file Referee.re, and ensure the last few lines
of the code looks like this:

module R1 = Referee(Connect4.Connect4, 
  (HumanPlayer.HumanPlayer(Connect4.Connect4)), 
  (HumanPlayer.HumanPlayer(Connect4.Connect4)));

R1.playGame();

One might need to change "AIPlayer" to "HumanPlayer".
Then, type in "npm run build" (without the quotation marks) in the terminal,
and run the code by typing "node src/Referee.bs.js" (without the quotation 
marks). Then, a board with all zeroes will appear on the screen, and the
players can start the game. 
To perform a move, one needs to input an integer from 1 to the maximum number
of columns in the board to represent the column that the player wants to make
the move on. An additional 1 will appear on the board when player1
makes a valid move (2 will appear for player2). If the move is not legal,
the terminal will make a warning and ask the player to make another move
attempt. The two players take turns to make moves on the board, with 
regular Connect4 rules apply, until either player wins or the game ends 
with a draw.
Either player can type in exit into the terminal when it's time to make a move
to end the game.


In order to play against an AI (player vs. AI), one needs to access the file
Referee.re, and ensure the last few lines of the code looks like either this:
(this code denotes the AI is the second player)

module R1 = Referee(Connect4.Connect4, 
  (HumanPlayer.HumanPlayer(Connect4.Connect4)), 
  (AIPlayer.AIPlayer(Connect4.Connect4)));

R1.playGame();

or this
(this code denotes the AI is the first player)
module R1 = Referee(Connect4.Connect4, 
  (AIPlayer.AIPlayer(Connect4.Connect4)), 
  (HumanPlayer.HumanPlayer(Connect4.Connect4)));

R1.playGame();

One might need to change the code accordingly.
Then, type in "npm run build" (without the quotation marks) in the terminal,
and run the code by typing "node src/Referee.bs.js" (without the quotation 
marks). Then, a board with all 0's will appear and the game will begin.
If the AI is the first player, a move represented by the number 1
will appear immediately on the screen, and the human player can type in an 
integer from 1 to the maximum number of columns in the board to represent 
the column that the player wants to make the move on. 
If the AI is the second player, the human player plays first by typing in 
integer from 1 to the maximum number of columns in the board to represent 
the column that the player wants to make the move on. The AI will automatically
make the move immediately.
If the move is not legal,
the terminal will make a warning and ask the player to make another move
attempt. The two players take turns to make moves on the board, with 
regular Connect4 rules apply, until either player wins or the game ends 
with a draw.
The human player can type in exit into the terminal when it's time to 
make a move to end the game.


In order to watch 2 AIs play against each other, one needs to access the file
Referee.re, and ensure the last few lines of the code looks like this:

module R1 = Referee(Connect4.Connect4, 
  (AIPlayer.AIPlayer(Connect4.Connect4)), 
  (AIPlayer.AIPlayer(Connect4.Connect4)));

R1.playGame();

One might need to change the code accordingly.
Then, type in "npm run build" (without the quotation marks) in the terminal,
and run the code by typing "node src/Referee.bs.js" (without the quotation 
marks). Then, a board with all 0's will appear and the game will begin.
The AIs will automatically make moves as soon as the game begins, and the
result of the game will appear soon.


In order to change the size of the board (whichever game mode), one can access
Referee.re, and change the two integers in Line 42, which looks like this:

    try (gameLoop(CurrentGame.initialState("5 7"))) {

the first integer in the quotation marks represent the number of rows in the 
board, and the second integer represent the number of columns. It is suggested
that both integers are greater than or equal to 4, so that there is possibility
to yield a win (even though the game can still be played with a smaller board).
It is also suggested the integers are not too big (like greater than 50), so
that the terminal can properly display the board.
Then, type in "npm run build" (without the quotation marks) in the terminal,
and run the code by typing "node src/Referee.bs.js" (without the quotation 
marks). Then, a board with all 0's will appear and the game will begin. The 
same procedures to play the game still apply.



2. The Connect4.re contains the concrete implementation of the Game. 
It provides the rules and setting of connect 4, including basic types, 
transformation helper functions (such as transpose), procedures that 
keep the game moving on, helpers that are useful in AIPlayer and Referee, 
and estimate values for present board states.
The HumanPlayer and AIPlayer contain the concrete implementation of Player.
The HumanPlayer allows users to type in their moves when its their turn and
ask for a new input when the move is illegal. The AIPlayer serves as an AI 
that uses the minimax algorithm based on estimateValue procedure in Connect4 
to calculate the optimal move and play the game. The Referee will conduct 
the actual game, which could be played by 2 HumanPlayers, 2 AIPlayers, or a 
HumanPlayer against an AIPlayer.

3. Our AIPlayer doesn't always make intelligent moves. This is partly due to
the limitations of our estimateValue, as the pattern-matching cases do not
account for all scenarios (but do account for most scenarios).

4. This project was completed by Yutong Wang and Ruoxuan Mao. Also, special 
thanks to Stephen Chen who helped us regarding implementation during design
check, and special thanks to Thet who helped us via edstem.

5. We didn't implement any extra features.