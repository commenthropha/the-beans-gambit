![Event Ticket Website](logo.png)

## Overview
A parser for BGN files that will take in a file and generate a story from it if it is valid.

### Features
* If the file is of an incorrect format then the program will return a statement indicating the user
of this and it will not parse the program at all.
* If the file is of a correct format but during the course of execution the program identifies any
moves that are played which defy the basic rules of the Bean’s Gambit, it will provide detailed
information about this including the expected result as well as the nature of the move that was
played.
* Otherwise, if the file was successfully parsed, it will play the game in order, generating a story
paragraph for each move based on the following factors:
    1. The type of move (a standard move, a capture, or the end of a game)
    2. The player making the move
    3. Whether the player making the move is winning/losing at the time of making the aforementioned move

### Libraries/Frameworks Used
1. [Megaparsec](https://hackage.haskell.org/package/megaparsec)

### Installation & Usage
Provided that your system already has Haskell pre-installed (if not refer to [GHCup](https://www.haskell.org/ghcup/)), run

``` bash
stack -- run *pathtofile*
```

# My Development Experience
## The Problem
The aim for this project was to create a parser for files using Bean's Gambit Notation (`.bgn`) that would check if a file is valid. Bean’s Gambit Notation (BGN) is a propietary format used to play a game of the "The Bean's Gambit" based on a similar notation for Chess called Portable Game Notation (PGN), where each line is in the following format:
```
XX. MOVE_BLUE MOVE_RED
```
The only exception to this rule is the last line, which may only have a Blue move if the game ended after Blue's move
``
XX. MOVE_BLUE
``
Both MOVE_BLUE and MOVE_RED are specified in the same format and may be any of
the following shapes:
``` 
a1a2  -- Move a piece on a1 to empty square a2
c3xc2 -- Using a bean on c3 to capture a piece on c2
D     -- The last move resulted in a drawn game state
#     -- The player loses due to a trapped cow
```

Throughout the entirety of the design process, I was particularly scrupulous about abiding by principle of separation of concerns - consequently, I abstracted the parsing process into separate parsers for each distinct type of `PlayerMove` as well as each line. I tested these parsers at each level of the design process by feeding test data into `parseTest` to make sure that they were functioning as intended. 

The `parseLine` parser was of particular importance as this served as the foundation for my file parser. Each line in a `.bgn` file consisted of a pair of moves, therefore it made sense to me to parse this as a tuple of PlayerMoves and store it as a `BGNLine` type as it would be a more direct translation of the lines into a data type than parsing each move individually. This served a number of advantages such as easily being able to tell which player was making a move as well as the corresponding line number in the `.bgn` file by looking at the index in the tuple and the index of the `BGNLine` respectively. 

When parsing each file, I stored it within my program as data of type `BGN = [BGNLine]`, which could then be used to construct a list of boards `[Board]` by recursively appending each pair of boards that the PlayerMoves from each BGNLine corresponded to. Although this may seem unnecessarily convoluted, my solution meant that it was extremely easy to find out which `Board` corresponds to which `PlayerMove`, the `BGNLine` that this `PlayerMove` lies in, as well as a plethora of other important details that allowed me to process and output specific and relevant information whenever it was required. 

Updating the design of my parsers throughout the development process using systematic refactoring of the code for each function also meant that I was able to make effective use of combinators from the Megaparsec library to make the code as concise and well-written as I could. Further, more descriptive, details pertaining to each parser and the difficulties I encountered when designing them can be found in the respective functions in the `Main.hs` file.

## Reflections
### Things Learned
Primarily, this project was intended to serve the purpose of allowing me to become more familiar with monadic interfaces. Undoubtedly, understanding monads was the most difficult part of learning Haskell for me and I feel as if this project - with its extensive use of monads as a direct result of using the `megaparsec` library for a number of the primary functions - has allowed me to develop a greater understanding of them.

### Improvements
One thing that I’ve noticed with my program is in regards to outputting the number of the move in the terminal and it is that if a board is repeated, it will write the move number as the first move in which the board appeared instead of the correct one. For example, if a board state first happened at Move 2 and it repeats itself at Move 7 then the heading will read ”Move 2” instead of ”Move 7.” This is because when looking for the existence of a `Board` in a list it will look for the first instance. 

There’s no real way of fixing this without going through some trouble to do so - I could make it so that it looks for the existence of the last board in the list and return that index however that will have the same issue only the other way around. I should also add that for the BGN file `invalid-wrong-format.bgn` my program considers it as valid but that was an intentional choice as my program will always take the first two valid strings on each line, irrespective of what follows, to allow for commenting within BGN files if the author is so inclined.
