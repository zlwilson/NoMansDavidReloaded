-- CPSC 312 - 2016 - Games in Haskell
module SimpleGrid where

-- To run it, try:
-- ghci
-- :load Minimax2

import Games2

data SGMove = GoLeft | GoRight | GoUp | GoDown
    deriving (Ord,Eq, Show, Read)
sgmoves = [GoRight, GoDown]
sgmovesTOP = [GoLeft, GoRight, GoDown]
sgmovesBOTTOM = [GoLeft, GoRight, GoUp]
sgmovesLEFT = [GoRight, GoUp, GoDown]
sgmovesRIGHT = [GoLeft, GoUp, GoDown]
sgmovesALL = [GoLeft, GoRight, GoUp, GoDown]
sgmovesTR = [GoDown, GoLeft]
sgmovesBR = [GoUp, GoLeft]
sgmovesBL = [GoUp, GoRight]
gb = [50, 86, 73,
      68, 65, 68,
      420, 41, 69,
      80, 08, 5]

type SGState =  (Int, Int, [Int])  -- (mine, house, random numbec)

--- simple grid game ---
--- get David = 360 ---
sggame :: Game SGMove SGState [Double]

sggame (Move GoLeft (i,c,gb))
  | c==0 = EndOfGame "==== GAME OVER ====" 0  []
  | i==0 = ContinueGame (-30) (i,c-1,gb) sgmovesLEFT  
  | i==3 = ContinueGame (-30) (i,c-1,gb) sgmovesLEFT
  | i==6 = ContinueGame (-30) (i,c-1,gb) sgmovesLEFT
  | otherwise = ContinueGame (gb!!(i-1)) (i-1,c-1,gb) sgmovesALL

sggame (Move GoRight (i,c,gb))
  | c==0 = EndOfGame "==== GAME OVER ====" 0  []
  | i==2 = ContinueGame (-30) (i,c-1,gb) sgmovesRIGHT
  | i==5 = ContinueGame (-30) (i,c-1,gb) sgmovesRIGHT
  | i==8 = ContinueGame (-30) (i,c-1,gb) sgmovesRIGHT
  | otherwise = ContinueGame (gb!!(i+1)) (i+1,c-1,gb) sgmovesALL

sggame (Move GoUp (i,c,gb))
  | c==0 = EndOfGame "==== GAME OVER ====" 0  []
  | i<3 = ContinueGame (-30) (i,c-1,gb) sgmovesTOP
  | otherwise = ContinueGame (gb!!(i-3)) (i-3,c-1,gb) sgmovesALL

sggame (Move GoDown (i,c,gb))
  | c==0 = EndOfGame "==== GAME OVER ====" 0  []
  | i>6 = ContinueGame (-30) (i,c-1,gb) sgmovesTOP
  | otherwise = ContinueGame (gb!!(i+3)) (i+3,c-1,gb) sgmovesALL

sggame (Start c) = ContinueGame 0 (0,5,gb) sgmovesALL