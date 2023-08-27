module Bean.Types where

import Data.Char (toLower)
import qualified Data.List as List

--------------------------------------------------------------------------------
-- Custom Type Definitions

-- | The board to display at the end of the game if the game has been drawn.
drawnBoard :: Board
drawnBoard = [replicate 4 (Blue Bean)
  , replicate 4 (Red Bean)
  , replicate 4 (Blue Bean)
  , replicate 4 (Red Bean)]

-- | The board to display at the end of the game if the Blue player won the game.
blueBoard :: Board
blueBoard = [replicate 4 (Blue Bean)
  , replicate 4 (Blue Bean)
  , replicate 4 (Blue Bean)
  , replicate 4 (Blue Bean)]

-- | The board to display at the end of the game if the Red player won the game.
redBoard :: Board
redBoard = [replicate 4 (Red Bean)
  , replicate 4 (Red Bean)
  , replicate 4 (Red Bean)
  , replicate 4 (Red Bean)]

-- A BGN type
type BGN = [BGNLine]

-- A BGNLine type
type BGNLine = (PlayerMove, PlayerMove)

-- A PlayerMove type
data PlayerMove = Move {c1 :: Coord, c2 :: Coord }
         | Capture{captor :: Coord, captive :: Coord}
         | LoseDraw{ch :: Char}
         | Nothing
          deriving(Show)
--------------------------------------------------------------------------------
-- Pre-provided Type Definitions

-- | The board is a two-dimensional list of pieces.
type Board = [[Piece]]

instance {-# OVERLAPS #-} Show Board where
  show b = List.intercalate "\n" $ map (concatMap show) b

-- | A coordinate represents the (row,column) of the square in question. 
--   Zero-indexed from the top-left of the board.
type Coord = (Int,Int)

-- | A Piece is one square of the board; it may be empty or contain a Red
--   piece or a Blue piece.
data Piece = Empty | Red PieceType | Blue PieceType
  deriving (Eq)

-- | The corresponding player is either Red or Blue.
data Player = RedPlayer | BluePlayer
  deriving (Eq, Ord)

-- | There are two pieces: Beans and Cows.
data PieceType
  = Bean
  | Cow
  deriving (Eq, Ord)

--------------------------------------------------------------------------------
-- Provided instances

-- You may make use of these instances as appropriate when implementing your 
-- own instances as required. 

instance Show PieceType where
  show Bean = "B"
  show Cow  = "C"

instance Show Piece where
  show Empty     = "."
  show (Red pt)  = "\27[31m" ++              show pt ++ "\27[0m"
  show (Blue pt) = "\27[34m" ++ map toLower (show pt) ++ "\27[0m"

-- I modified this to get rid of the player text at the end
instance Show Player where
  show RedPlayer = "\27[31mRed\27[0m"
  show BluePlayer = "\27[34mBlue\27[0m"
