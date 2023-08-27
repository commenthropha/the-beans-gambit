module Bean.Functions where

import Bean.Types
import Bean.Game
import Data.List
import System.Random
--------------------------------------------------------------------------------
-- | New Functions

{-|
  Returns the last board state before the current board from a list of boards
  (try saying that without getting bored)
-}
lastBoard :: Board -> [Board] -> Board
lastBoard b bs
  | getBoardIndex b bs == 0 = b
  | otherwise = bs !! (getBoardIndex b bs - 1)

{-|
  Returns the sign of an integer (or 0 if the number is 0)
-}
signumInt :: Int -> Int
signumInt x
  | x > 0     = 1
  | x < 0     = -1
  | otherwise = 0

{-|
  Converts from a piece to a string - useful for outputs
-}
pieceToString :: Maybe Piece -> String
pieceToString p = case p of
  Just (Red Bean) -> "Bean"
  Just (Blue Bean) -> "Bean"
  Just (Red Cow) -> "Cow"
  Just (Blue Cow) -> "Cow"
  Just _ -> " "
  Prelude.Nothing -> " "

{-|
  Returns a PlayerMove corresponding to a given board in a game of The
  Bean's Gambit

  This is essntially just mapping values from one data structure to another.
  If the board index is even then we want the Red Player's move in the corresponding
  BGNLine which is the second move out of the tuple and the opposite is true for
  whenever we want the Blue Player's move.
-}
boardToPlayerMove :: Board -> [Board] -> BGN -> PlayerMove
boardToPlayerMove b bs bgn =
  let 
    i = getBoardIndex b bs
    element = if even i then snd (bgn !! boardIndexToBGNIndex i) else fst (bgn !! boardIndexToBGNIndex i)
  
  -- In this particular let expression, we just use the 'in' keyword to return the 
  -- intermediate value of 'element' which we just computed
  in element

{-|
  Returns the player for a given board in a game of The Bean's Gambit

  Because each line of the BGN files are in the form (BluePlayer, RedPlayer),
  after accounting for the starting board at index 0, it would follow that all
  BluePlayer boards have an odd index and all RedPlayer boards have an even one.
-}
boardToPlayer :: Board -> [Board] -> Player
boardToPlayer b bs = if even (getBoardIndex b bs) then RedPlayer else BluePlayer

{-|
  Gets the index of a board in a list of boards

  Essentially identical to the elemIndex function with the exception
  of returning an Integer when the board is found and an error when the
  board is not. This allows us to return an Int instead of a Maybe Int,
  and this helps get rid of some issues pertaining to Type Errors.  
-}
getBoardIndex :: Board -> [Board] -> Int
getBoardIndex b bs = case elemIndex b bs of
  Just i -> i
  Prelude.Nothing -> error "Board not found"

{-|
  Used to find the BGN index from the board index

  Because each element of the BGN type is a BGNLine, which 
  is a pair of player moves, it followed that I needed a function 
  which mapped values in the following way to ensure that each
  board index pointed towards the correct BGNLine:

  1 -> 0

  2 -> 0

  3 -> 1

  4 -> 1
  ...

  When the board index is 0, it is pointing towards the starting board;
  I consider the case for the 0 index separately as this function will
  generate a negative index otherwise. Although I also map this to 0, this 
  is never an issue in practice as I explicitly deal with the case in 
  which the board is equal to the starting board in all functions that
  make use of the boardIndexToBGNIndex function.
-}
boardIndexToBGNIndex :: Int -> Int
boardIndexToBGNIndex 0 = 0
boardIndexToBGNIndex n = (n + 1) `div` 2 - 1

{-|
  Picks a random element from a list
-}
randomElement :: [a] -> IO a
randomElement xs = do
  
  -- Generates a random integer that could point towards
  -- any index in the list
  index <- randomRIO (0, length xs - 1)
  
  -- Returns the element at this randomly generated index
  return (xs !! index)

{-|
  Outputs a graphic at the end of the game telling you who won

  The reason this works is because the checkMove function returns
  a Board - even in the case when the game is won or drawn - so I 
  decided to use this to add a minor style affectation to my
  program whereby it will return a board corresponding to the result
  of the game.

  The last element in the list of boards will correspond to one of 
  these boards as mentioned above, and so it provides us with a easy
  way to determine the correct graphic to output.
-}
winMessage :: Board -> IO String
winMessage b

  -- I had to use guards here as "case of" syntax wouldn't work.
  -- This is because the boards are not defined as a type, and
  -- so using "case of" function confuses the compiler which will
  -- create new variables in their place. Instead, we must check 
  -- for equality explicitly.

  -- If the Red Player has won
  | b == redBoard = do
      fileContents <- readFile "text/redwin.txt"
      -- Print the graphic in red
      return $ "\x1b[31m" ++ fileContents ++ "\x1b[0m\n"
  
  -- If the Blue Player has won
  | b == blueBoard = do
      fileContents <- readFile "text/bluewin.txt"
      -- Print the graphic in blue
      return $ "\x1b[34m" ++ fileContents ++ "\x1b[0m\n"

  -- If the game has been drawn
  | b == drawnBoard = do
      fileContents <- readFile "text/draw.txt"
      -- Print the graphic in yellow
      return $ "\x1b[33m" ++ fileContents ++ "\x1b[0m\n"

  | otherwise = return ""

{-|
  Determines whether a piece belongs to a player

  I wrote this function to deal with a case proposed by the BGN file
  invalid-incorrect-move.bgn where both players attempt to move a piece
  that is not theirs; the makeMove function, as defined in coursework 1,
  only checks if a move is valid however it doesn't check if the player
  is moving a piece that belongs to them and this obviously causes issues.
-}
belongsToPlayer :: Player -> Board -> Coord -> Bool
belongsToPlayer p b c = case p of
  
  -- A piece belongs to the Red Player if it is a Red Bean or a Red Cow
  RedPlayer -> getElement b c == Red Bean || getElement b c == Red Cow
  
  -- A piece belongs to the Blue Player if it is a Blue Bean or a Blue Cow
  BluePlayer -> getElement b c == Blue Bean || getElement b c == Blue Cow
  
{-| 
  A helper function that deletes the last element from a list

  This function is used as sometimes I have an extra board in the list
  and I want to get rid of it. I go into more detail about this in the
  specific instances where I use the deleteLast function.

  This function saw a lot of development. Initially, I was going to just
  use the init function to remove the last element from a list however
  reading up on the init function online suggested that it doesn't
  work if the list is empty. Consequently, this made me disregard the
  function entirely in my head and so I designed this function as follows:

  deleteLast :: [a] -> [a]

  deleteLast [] = []

  deleteLast [x] = []

  deleteLast (x:xs) = x : deleteLast xs

  However, after writing the function, I then realised that I could just
  pattern match on the list to deal with the case that the list is empty
  and return an empty list to prevent the init function from causing me 
  issues, which lead to me writing the following function.

  Both implementations of this function write exactly the same, however
  I ended up going with the one that I did because I thought it would
  be slightly more efficient than the recursive approach that I outlined
  earlier.
-}
deleteLast :: [a] -> [a]
deleteLast [] = []
deleteLast xs = init xs

{-|
  Returns the opponent's board
-}
opponentBoard :: Player -> Board
opponentBoard p = case p of
  RedPlayer -> blueBoard
  BluePlayer -> redBoard

{-|
  Returns the opposite player
-}
oppositePlayer :: Player -> Player
oppositePlayer p = case p of
    RedPlayer -> BluePlayer
    BluePlayer -> RedPlayer

--------------------------------------------------------------------------------
-- | Functions from Coursework 1

{-|
  My makeMove implementation from CW1; exclusively for use by captureMove
-}
makeMoveA :: Board -> Coord -> Coord -> Board
makeMoveA b c1 c2 =  setCoord (setCoord b c2 (getElement b c1)) c1 Empty

{-|
  Checks if a move results in a capture
-}
captureMove :: Board -> Player -> Coord -> Coord -> Bool
captureMove b p c1 c2
  | p == RedPlayer && balance (makeMoveA b c1 c2) > balance b = True
  | p == BluePlayer && balance (makeMoveA b c1 c2) < balance b = True
  | otherwise = False

{-|
  Checks if a move is valid for all types of pieces
-}
validMove :: Board -> Coord -> Coord -> Bool
validMove b c1 c2 = case getElement b c1 of
  Red Cow -> c2 `elem` validCowMoves b c1
  Blue Cow -> c2 `elem` validCowMoves b c1
  Red Bean -> c2 `elem` validBeanMoves b RedPlayer c1
  Blue Bean -> c2 `elem` validBeanMoves b BluePlayer c1
  Empty -> False

{-|
  Gets an element at a coordinate in a Board
-}
getElement :: Board -> Coord -> Piece
getElement b c = b !! snd c !! fst c