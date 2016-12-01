-- CPSC 312 - 2016 - Games in Haskell
module Games2 where

-- To load it, try:
-- ghci
-- :load Games2

data Action mt st init               -- mt=move_type, st=state_type, init=initialization_type
            = Move mt st             -- do move in state
            | Start init             -- returns starting state with initialization

data Result mt st init
            = EndOfGame [Char] Int init  -- end of game, string, reward, initializarion for next game
            | ContinueGame Int st [mt]   -- continue with reward, new state, and list of possible moves
         deriving (Eq, Show)

type Game mt st init = Action mt st init -> Result mt st init

-- mt=move_type, st=state_type, init=initialization_type, ps=player_state
type Player mt st init ps = Game mt st init -> Result mt st init -> ps ->  (mt,ps)

-- for random numbers in staes, we do not want print or comparisons to depend on them
data Rands = Rands [Double]    -- random numbers. Do not print them!!!
instance Show Rands where
   show d = "_"
instance Eq Rands where
   x == y = True
instance Ord Rands where
   x < y = False
   x <= y = True
