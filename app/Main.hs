module Main (main) where

import Bean.Types
import Bean.Game
import Bean.Functions
import Bean.Story
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Functor
import Data.Char
import Data.List ( intercalate )
import Control.Monad
import System.Environment (getArgs)

--------------------------------------------------------------------------------
-- | Main program

type Parser = Parsec Void String

{-|
  Using the IO monad to allow the user to input a file and receive outputs.
-}
main :: IO ()
main = do 

    -- Gets the file arguments
    args <- getArgs
    case args of

        -- If the argument provided is empty
        [] -> putStrLn "\27[31mNo game file specified\27[0m"

        -- Otherwise, take the first argument
        (gameFile:_) -> do 
            game <- readFile gameFile

            -- Splits the contents of the game file into a list of strings
            let gameLines = lines game

            -- Only parse the file if it has a valid ending (the game ends in a draw or a loss)
            --
            -- This was an example of a case that I had not considered initially but was brought
            -- up by one of the invalid BGN files - specifically noendchar.bgn
            if not ('D' `elem` last gameLines || '#' `elem` last gameLines)
            then do
                putStrLn "\n\27[31mUnfortunately, this BGN file is invalid - it does not have an end character.\27[0m"
                return ()

            -- Parse the file and store it as a variable of type BGN
            --
            -- We want to add this to a new list so we pass in an empty list
            else let bgn = convertBGN gameLines []
              -- Checks if the file was able to parse successfully
              --
              -- We add startingPos to a list as we want this to be in our history of game states
              in case bgnToBoards bgn startingPos [startingPos] of
                
                -- Error case
                Left err -> do 
                  putStrLn "\n\27[31mUnfortunately, this BGN file is invalid\27[0m\n" 
                  putStrLn err
                
                -- Expected case
                Right boards -> do
                  putStrLn ""
                  logo <- readFile "text/logo.txt"

                  -- Print the logo of the game
                  putStrLn ("\x1b[33m" ++ logo ++ "\x1b[0m")

                  -- Output the file name
                  putStrLn ("\x1b[38;5;208m" ++ gameFile ++ "\x1b[0m\n")

                  -- Using the bind operator here as it's more concise then writing another 'do' block
                  openingLine >>= putStrLn

                  -- If the last element is Nothing, deletes the last value of the list
                  --
                  -- I do this because I return a board in the case that the second element 
                  -- in a BGNLine is Nothing; this serves no practical purpose other to 
                  -- make the output pretty and get rid of that duplicate 
                  case snd (last bgn) of
                  
                    -- Using mapM_ here maps the print to all boards and returns it monadically
                    -- This allows us to use a "do" statement here and discard the result of the
                    -- Map (which would usually return a list) as we don't need it, which is better
                    -- for memory.
                    --
                    -- The print function is equivalent to putStrLn . show, so it outputs 
                    -- using the provided show instance
                    Bean.Types.Nothing ->  do
                      mapM_ (\board -> do
                        
                        -- Print minor information about the move
                        moveInformation board boards
                        
                        -- Print the board
                        print board
                        
                        -- Print slightly more detailed information + move-specific story dialogue
                        moveLine (boardToPlayer board boards) (boardToPlayerMove board boards bgn) board (lastBoard board boards) >>= putStrLn
                        
                        -- This is the only way that I could make the contents of the divider a different color, as far as I know
                        -- I could've used another 'do' block here but I wanted to try this statement on one line
                        putStrLn "\x1b[33m" >> readFile "text/divider.txt" >>= putStrLn >> putStrLn "\x1b[0m")
                        (deleteLast boards)

                    -- Otherwise, just print the list as it is
                    _ -> do 
                      mapM_ (\board -> do

                        -- Print minor information about the move
                        moveInformation board boards

                        -- Print the board
                        print board

                        -- Print slightly more detailed information + move-specific story dialogue
                        moveLine (boardToPlayer board boards) (boardToPlayerMove board boards bgn) board (lastBoard board boards) >>= putStrLn
                        putStrLn "\x1b[33m" >> readFile "text/divider.txt" >>= putStrLn >> putStrLn "\x1b[0m")
                        boards
                
                  -- Print a win message depending on what the end result of the game is
                  --
                  -- We can tell by looking at the last board as the end board will correspond to
                  -- the game result
                  winMessage (last boards) >>= putStrLn

{-|
  Converts a BGN file into a BGN data type for internal processing.
-}
convertBGN :: [String] -> BGN -> BGN

-- Base Case
--
-- If the list is empty, return the result in reverse order
-- This ensures that the BGN data is returned in the proper order
convertBGN [] result = reverse result

-- Recursive Call
--
-- Otherwise, if the list is not empty, parse each line recurseively
convertBGN (l:ls) result =
  
  -- Run parseLine on each of the input lines
  -- Input is just a placeHolder here for the input source
  let parseResult = runParser parseLine "input" l
  in case parseResult of
    
    -- Return an error if any of the lines fail to parse correctly
    Left _ -> error "\n\n\27[31mThis BGN file is of an invalid format.\27[0m\n"

    -- Otherwise, continue parsing
    Right bgnLine -> convertBGN ls (bgnLine:result)


{-|
  Converts a BGN type into a list of boards representing the game.
-}
bgnToBoards :: BGN -> Board -> [Board] -> Either String [Board]

-- Base Case
--
-- If the list of moves is empty, return the set of boards
bgnToBoards [] b allBoards = Right allBoards

-- Recursive Call
--
-- Otherwise, if the list is not empty, apply the first set of moves
-- to the current board and recursively call this function on the
-- board that is returned as a result of the set of moves
bgnToBoards (l:ls) b allBoards =
  let (newBoardBlue, newBoardRed) = lineToBoard l b allBoards
      
      -- Append newBoardBlue and newBoardRed to the list of allBoards
      allBoards' = allBoards ++ [newBoardBlue, newBoardRed] -- 

  in case bgnToBoards ls newBoardRed allBoards' of

       -- If an error occurs in the recursive call, pass it along
       Left err -> Left err

       -- Otherwise, the result of the recursive call as-is
       Right boards -> Right boards 

{-|
  Converts a pair of PlayerMoves into a pair of boards representing the result of those moves.
-}
lineToBoard :: BGNLine -> Board -> [Board] -> (Board, Board)

-- The board returned by the first move is passed into the RedPlayer's move
lineToBoard l b bs = (checkMove (fst l) BluePlayer b (b:bs), 
  checkMove (snd l) RedPlayer (checkMove (fst l) BluePlayer b bs) (b:bs))

{-|
  A function that returns appropriate information pertaining to where/how an error has occured.

  This function was always going to be kind of disgusting irregardless of what I did due to its
  nature of having to output different strings, however there were a few measures that I took in
  order to ease the burden on the eyes. The first one was to use pattern matching instead of case
  statements, and the second was to use the intercalate function from Data.List to insert the \n
  character between all of the strings instead of having to define them explicitly but it doesn't
  improve much.
-}
moveDiagnostic :: Board -> Player -> PlayerMove -> String

-- If the PlayerMove is a standard move
moveDiagnostic b p (Move c1 c2) =
  intercalate "\n" ["\n\nPlayer's Turn: " ++ show p, "The move was from " ++ show c1 ++ 
    " [" ++ maybe "Nothing" show (getPiece b c1) ++ "] to " ++ show c2 ++ " [" ++
   maybe "Nothing" show (getPiece b c2) ++ "]\n\n\x1b[3mAnd so our tale comes to a premature end...\x1b[0m\n"]

-- If the PlayerMove is expected to result in a capture
moveDiagnostic b p (Capture c1 c2) =
  intercalate "\n" ["\n\nPlayer's Turn: " ++ show p, "The move was from " ++ show c1 ++
   " [" ++ maybe "Nothing" show (getPiece b c1) ++ "] to " ++ show c2 ++ " [" ++
    maybe "Nothing" show (getPiece b c2) ++ "]\n\n\x1b[3mAnd so our tale comes to a premature end...\x1b[0m\n"]

-- If the game is supposed to have ended
moveDiagnostic _ p (LoseDraw ch) =
  case ch of
    -- If it says that the player has lost
    '#' -> intercalate "\n" ["\n\nPlayer's Turn: " ++ show p, 
      "Resulting in a loss for " ++ show p ++ " however this wasn't the case." ++ 
      "\n\n\x1b[3mAnd so our tale comes to a premature end...\x1b[0m\n"]

    -- If it says that the game has been drawn
    'D' -> intercalate "\n" ["\n\nPlayer's Turn: " ++ show p, 
      "Resulting in a draw, however this wasn't the case." ++ 
      "\n\n\x1b[3mAnd so our tale comes to a premature end...\x1b[0m\n"]
    _ -> ":)"

moveDiagnostic _ _ Bean.Types.Nothing = ":)"

{-|
  Returns the resulting Board from a move after checking if the move is valid. 
    
  Initially, both this function and checkValidMove were part of the same function,
  however because the checks for Move and Capture are both essentially identical I 
  refactored the code to include a separate function - checkValidMoveCapture. Ultimately,
  this function is still kinda gross however I do think that this was a welcome change 
  in making the function slightly more elegant.
-}
checkMove :: PlayerMove -> Player -> Board -> [Board] -> Board
checkMove pm p b bs = case pm of

  -- If the PlayerMove is a standard move
  Move c1 c2 -> checkValidMoveCapture b bs p pm (validMove b c1 c2 && 
    belongsToPlayer p b c1) (makeMoveA b c1 c2)
  
  -- If the PlayerMove is expected to result in a capture
  Capture c1 c2 -> checkValidMoveCapture b bs p pm (validMove b c1 c2 
    && belongsToPlayer p b c1 && captureMove b p c1 c2) (makeMoveA b c1 c2)
  
  -- If the game is supposed to have ended
  LoseDraw ch -> case ch of

    -- Check for a draw
    'D' -> 
      case p of 
      BluePlayer -> if gameIsDrawn bs
            then drawnBoard
            else error ("\n\n\27[31mThe next line implies that the game is drawn, however that is not the case.\27[0m" 
            ++ moveDiagnostic b p pm)
      RedPlayer -> if gameIsDrawn (deleteLast bs)
            then drawnBoard
            else error ("\n\n\27[31mThe next line implies that the game is drawn, however that is not the case.\27[0m" 
            ++ moveDiagnostic b p pm)

    -- Check if the current player has lost
    '#' -> if gameIsWon b p then opponentBoard p else error 
      ("\n\n\27[31mThe next line implies that the game has been lost, however that is not the case.\27[0m" 
      ++ moveDiagnostic b p pm)
    
      -- If the character is neither 'D' nor '#', then something spooky is going on
      --
      -- Pretty sure this is an impossibility, but I left the wildcard pattern match just in case.
    _ -> error "To be honest I'm not even sure how you ended up here!"
  
  Bean.Types.Nothing -> b

{-|
  Determines if the move is a valid Move/Capture, and returns the result of
  the move if it is.
|-}
checkValidMoveCapture :: Board -> [Board] -> Player -> PlayerMove -> Bool -> Board -> Board
checkValidMoveCapture b bs p pm isValidMove moveResult = 

  -- This checks if the game should've been won/drawn and wasn't
  --
  -- This was another example of a case that I hadn't considered initially and only came to 
  -- light after I tested my program with invalidIgnoredLoss.bgn from the library of BGN
  -- files. This lead me to consider cases when the game was won by the opponent and, by
  -- extension, when the game was drawn by the opponent and ignored
  if not (gameIsWon b p) || not (gameIsDrawn bs)
  then if isValidMove
    then moveResult
    else error ("\n\n\27[31mThe next attmempted move in the bgn file was not valid.\27[0m" ++ moveDiagnostic b p pm)
  else error ("\n\n\27[31mThis game should've ended in a loss, however it continued.\27[0m" ++ moveDiagnostic b p pm)


{-|
  Parses a line and returns it as a pair of PlayerMoves if it's recognised by the parser
-}
parseLine :: Parser BGNLine
parseLine = do

  -- We only want to start parsing the lines after the line numbers
  --
  -- The void functor makes easy work of this as it disregards the result of the
  -- evaluation you give it - in this case, everything up to and including the 
  -- first space character
  void (manyTill anySingle (char ' '))

  -- Parse the first string
  --
  -- Throughout all of my parsers I make use of the <?> operator to return an
  -- error if a statement is unable to be parsed.
  s1 <- parseExpr <?> "This file failed to parse."
  case s1 of
      -- If the first string is identified as a LoseDraw, return it as a 
      -- tuple with Nothing as the second element
      LoseDraw {ch = '#'} -> return (s1, Bean.Types.Nothing)
      LoseDraw {ch = 'D'} -> return (s1, Bean.Types.Nothing)
      _ -> do
        
        -- Parse the space character
        _ <- spaceChar
        
        -- Parse the second string, consisting of any non-space characters
        s2 <- parseExpr <?> "This file failed to parse."
        
        -- Return the parsed result as a tuple
        return (s1, s2)

{-|
  Finds the first that parses successfully and returns accordingly.

  Initially, I encountered difficulty with trying to make this function as intended;
  using the choice combinator in isolation for this parser was insufficient as if one parser
  succeeded, the program would proceed to automatically parse the second string expression as
  the same data type. As a result, I had to combine this with the higher order function 'map 
  try' to enforce parsing each statement separately.
-}
parseExpr :: Parser PlayerMove 

-- Syntactic sugar for choice(map try [..])
-- Using the $ operator here is slightly clearer than using parenthesis
parseExpr = choice $ map try [parseMove, parseCapture, parseLoseDraw]

{-|
  Parses a player move string and returns a PlayerMove value.

  Expected form: a1b1
-}
parseMove :: Parser PlayerMove
parseMove = do

  -- Parse the first character
  a <- oneOf "abcd" <?> "Expected a letter between a and d" 

  -- Parse the first integer
  n1 <- digitChar Data.Functor.<&> digitToInt

    -- Parse the second character
  b <- oneOf "abcd" <?> "Expected a letter between a and d"

  -- Parse the second integer
  n2 <- digitChar Data.Functor.<&> digitToInt

  -- Check if n1 and n2 are within the range 0-4
  guard (n1 >= 0 && n1 <= 4 && n2>=0 && n2 <= 4) <?> "Expected a number in the range 1-4"
  
  -- Convert the parsed characters and integers to coordinates
  -- Return the parsed result as a Move player move
  return $ Move (charIntToCoord a n1) (charIntToCoord b n2)

{-|
  Parses a capture string and returns a PlayerMove value.

  Expected form: a1xb1
-}
parseCapture :: Parser PlayerMove
parseCapture = do

  -- Parse the first character
  a <- oneOf "abcd" <?> "Expected a letter between a and d"

  -- Parse the first integer
  n1 <- digitChar Data.Functor.<&> digitToInt

  -- Parse the "x" character in the middle
  x <- char 'x'

  -- Parse the second character
  b <- oneOf "abcd" <?> "Expected a letter between a and d"

  -- Parse the second integer
  n2 <- digitChar Data.Functor.<&> digitToInt

  -- Check if n1 and n2 are within the range 0-4
  guard (n1 >= 0 && n1 <= 4 && n2>=0 && n2 <= 4) <?> "Expected a number in the range 1-4"

  -- Convert the parsed characters and integers to coordinates
  -- Return the parsed result as a Capture player move
  return $ Capture (charIntToCoord a n1) (charIntToCoord b n2)

{-|
  Parses a lose/draw character and returns a PlayerMove value.
-}
parseLoseDraw :: Parser PlayerMove
parseLoseDraw = do

  -- Parse a character that represents a lose/draw move ('D' or '#')
  a <- oneOf "D#" <?> "Expected 'D' or '#'" 

  -- Return the parsed result as a LoseDraw player move
  return $ LoseDraw a

{-|
  Convert from the BGN format to the internal coordinate format
-}
charIntToCoord :: Char -> Int -> Coord
charIntToCoord z x = case z of
  'a' -> (0, 4-x)
  'b' -> (1, 4-x)
  'c' -> (2, 4-x)
  'd' -> (3, 4-x)

  -- Again, I'm pretty sure that this case is an impossibility as
  -- the file will not parse to let it get to this point, but I
  -- accounted for it nevertheless
  _ -> error "Invalid Coordinate"