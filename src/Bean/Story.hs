{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Bean.Story where
import Bean.Types
import Bean.Game
import Bean.Functions
import Bean.Text

{-
  Prints an opening line for the game.

  Because we expect this function to return an IO String, we use "do" notation
  here as we want to sequence actions automatically without having to explicitly
  write the bind operator each time. This serves the added advantage of being
  much more readable while also providing a way to easily interact with monadic
  interfaces such as the IO monad; I use "do" notation throughout the rest
  of the functions in this file for these same reasons.

  The only other thing of note in this function is that I make use of ANSI 
  character codes to format the output to the terminal: in this case, it is to
  output the line in italics. I use these throughout a number of functions in my
  program, however I thought that I would mention it here explicitly.
-}
openingLine :: IO String
openingLine = do
  
  -- Bind a random opening to the op variable
  op <- randomElement opening
  
  -- Output the opening in italics
  return $ "\x1b[3m" ++ op ++ "\x1b[0m\n"

{-
  Returns minor information about the move including the move number and which
  players turn it is to move.

  The primary focal point when designing this function was figuring out how to
  get the move number from the board. Initially, I was doing it all within this
  function however that approach proved to be slightly inelegant but mainly failed
  to abide by the principle of separations of concerns. Consequently, I ultimately
  decided to include a function to do that separately within Functions.hs
-}
moveInformation :: Board -> [Board] -> IO ()
moveInformation b bs = do

  -- The reason this benefitted from an extra function was that finding an
  -- element using an index in the usual manner - using elemIndex - returns
  -- a Maybe value (as the value provided may not exist within the list).
  -- So, it's required to pattern match on the elemIndex function to extract 
  -- the integer value.
  let i = getBoardIndex b bs
  
  -- Getting the player using the boardToPlayer function
  let player = boardToPlayer b bs
  
  case i of
    -- For the starting board
    0 -> putStrLn "\x1b[3mBoth armies take their starting position on the battlefield...\x1b[0m\n"
    -- For all other boards, print information about the player move
    _ -> putStrLn $ "\x1b[4m\x1b[33mMove Number: " ++ show i ++ "\x1b[0m\n\nIt is " ++ show player ++ "'s turn to move.\n"

{-
  Returns a story paragraph depending on state of the game.

  I will be the first to admit that this isn't the most concise of functions,
  however this was almost always inevitable considering the fact that there are
  12 different possibilities from each move alone; I thought that it would be a 
  cool idea to output the story differently, not only from the type of move, but 
  alsobased on whether the player making the move is "winning" or "losing" the 
  game atthat point in time. I evaluate this using the board balance heuristic 
  from the first coursework which is determined by the difference in Beans between
  both players. If the Red Player has a greater balancThe battle between the Red and Blue armies had reached a stalemate, with both sides locked ine than the second Player
  than they are winning, and vice versa. Retrospectively, I probably wouldn't 
  have made this design choice ifI realised the effort it would've taken to write
  48 unique possibilities from a single move alone, but it does lead to a more 
  complete storytelling process by accounting for these small things.

  In terms of function design, I used do notation thoroughly for the same reasons
  as outlined in my preamble for the openingLine function and took appropriate 
  measures to try and make the code concise wherever it was possible; for example, 
  using explicit pattern matching for the type of the move. An area where I was
  able to cut down a lot of otherwise unnecessary code was in the Move case, where
  I considered the player and piece cases simultaneously instead of doing them
  separately and unnecessarily repeating blocks of code.
-}
moveLine :: Player -> PlayerMove -> Board -> Board -> IO String
moveLine p (Move c1 c2) b lb = do
    if b == startingPos
        
        -- If it's the starting board
        then return "\nLet the battle begin!"
        
        -- Otherwise
        else do
            let piece = getPiece lb c1
                boardBalance = signumInt (balance lb)

            -- Considering all 12 cases for a single move
            dialogue <- case (p, piece) of
                (RedPlayer, Just (Red Bean)) ->
                    case boardBalance of
                        1 -> randomElement redWBMove
                        0 -> randomElement redEBMove
                        -1 -> randomElement redLBMove
                (RedPlayer, Just (Red Cow)) ->
                    case boardBalance of
                        1 -> randomElement redWCMove
                        0 -> randomElement redECMove
                        -1 -> randomElement redLCMove
                (BluePlayer, Just (Blue Bean)) ->
                    case boardBalance of
                        -1 -> randomElement blueWBMove
                        0 -> randomElement blueEBMove
                        1 -> randomElement blueLBMove
                (BluePlayer, Just (Blue Cow)) ->
                    case boardBalance of
                        -1 -> randomElement blueWCMove
                        0 -> randomElement blueECMove
                        1 -> randomElement blueLCMove

                -- Return an error if it doesn't match any of the cases above
                _ -> error ("Invalid move" ++ show piece ++ show c1 ++ show c2)

            -- Return information about the move including the piece type, where the piece was
            -- moved from/to as well as relevant story dialogue
            return $ "\nA " ++ pieceToString piece ++ " is moved from " ++ show c1 ++ " to " ++
                    show c2 ++ "\n\n" ++ "\x1b[3m" ++ dialogue ++ "\x1b[0m"

moveLine p (Capture _ c2) b lb = do
    if b == startingPos
        
        -- If it's the starting board
        then return "\nLet the battle begin!"
        
        -- Otherwise
        else do
            let piece = getPiece lb c2
            
            -- Return the appropriate story dialogue depending on the player
            dialogue <- case p of
                RedPlayer -> randomElement redCapture
                BluePlayer -> randomElement blueCapture
            
            -- Return the appropriate "Bean Slain" graphic depending on the player
            fileContents <- case p of
                RedPlayer -> readFile "text/blueslain.txt"
                BluePlayer -> readFile "text/redslain.txt"
            return $ "\nA " ++ pieceToString piece ++ " is slain at " ++ show c2 ++ "\n\n" ++ "\x1b[3m"
             ++ dialogue ++ "\x1b[0m" ++ "\n\n" ++ fileContents

-- Initially, I wrote this case using do notation but I realised that I could get
-- it down to one line by making use of an if statement - one of the seldom instances
-- where using an if statement is more concise than using any alternatives! 
--
-- I use the fmap operator <$> to make the output look pretty by concatenating a 
-- newline character to whichever string is provided.
moveLine p (LoseDraw '#') _ _ =
    ("\n" ++) <$> randomElement (if p == RedPlayer then blueWin else redWin)

moveLine _ (LoseDraw 'D') _ _ =
    ("\n" ++) <$> randomElement draw

moveLine _ Bean.Types.Nothing _ _ = return " "










