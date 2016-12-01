-- CPSC 312 - 2016 - Games in Haskell
module Play2 where

-- To run it, try:
-- ghci
-- :load Play2

import System.IO
import System.Random
import Text.Read

import Games2
import SimpleGrid

-- error correcting read
getresponse line =
    case readMaybe line of
        Just val -> return val
        Nothing ->  do
            putStrLn "Error, please retry"
            newline <- getLine
            getresponse newline

------------ Single Player Games ---------

type SPTournammentState = (Int,Int)   -- total, games

single_play :: (Show s, Show m, Read m) => Game m s i -> Result m s i ->  SPTournammentState
                      -> IO SPTournammentState

single_play game (EndOfGame str res init)  (total,games) =    
   do
      putStrLn str
      putStrLn ("Total reward is "++show (total+res))
      putStrLn ("")
      putStrLn ("")
      single_play game (game (Start init))  (0,0)
      
single_play game (ContinueGame reward state avail)  (total,games) =
   do
      putStr( if reward /= 0 then "Reward of this room is " ++ show reward ++ " " else "")
      putStrLn ("Total reward is "++show (total+reward))
      putStrLn ("Here's the state: " ++ show state)
      putStrLn ("Available moves: "++ show avail)
      line <- getLine
      response <- getresponse line
      single_play game (game (Move response state))  (total+reward,games)

-- Play simple grid game
playSG = do
  g <- getStdGen   -- use newStdGen to get different random number generater each time
  single_play sggame (sggame (Start (randoms g :: [Double]))) (0,0)



