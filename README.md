**A functional programming implementation of 2048!**
To run the program, entering `cabal run` will have a new window pop up that displays the initial game state. After this, the game reacts to valid keyboard clicks which consists of the 4 arrow keys on the keyboard. To quit the game, either the `esc` key or the command q key - both of these are part of the gloss Haskell package. The premise of the game is that a move moves the tiles to the farthest it can go into the board in the direction which the user chooses, and when two tiles touch they are merged to its sum. When a merge occurs, that sum is added to the score that is seen on the bottom of the screen. The goal is to reach at least a 2048 tile. 