{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module Main where

import System.Environment
import System.IO
import System.Random
import System.Exit (exitSuccess)
import Data.Maybe (fromMaybe)
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Pure.Game
import Data.List
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Debug.Trace


--------------------------------------------------------------------------------
-- Model and Actions
type Board = [[Int]]

data GameState = GameState
 { board :: Board
 , score :: Int
 , gen :: StdGen
  }

data Action = KeyPress Char

data Direction = East
              | West
              | North
              | South

initBoard :: Board
initBoard = replicate 4 $ replicate 4 0

initialState :: IO GameState
initialState = do
    stdGen <- newStdGen
    let initgamestate = GameState initBoard 0 stdGen
    let randomPos = generateRandomPosition initgamestate
    return $ GameState (putTileOnBoard initBoard randomPos 2) 0 stdGen

putTileOnBoard :: Board -> (Int, Int) -> Int -> Board
putTileOnBoard grid (x, y) newValue =
        [ [if i == x && j == y then newValue else grid !! i !! j | j <- [0..3]] | i <- [0..3] ]

generateRandomPosition :: GameState -> (Int, Int)
generateRandomPosition (GameState grid _ gen) =
    let emptyPositions = [(x, y) | x <- [0..3], y <- [0..3], grid !! x !! y == 0]
    in if null emptyPositions
        then (-1, -1)
        else let (randomIndex, newGen) = randomR (0, length emptyPositions - 1) gen
             in (emptyPositions !! randomIndex)

-- Main 
main :: IO ()
main = do
    initial <- initialState
    let handleEventWrapper :: Event -> GameState -> GameState
        handleEventWrapper event = execState (handleEvent event)
    play
        (InWindow "2048 Game" (400, 550) (10, 10))
        white
        10
        initial
        draw
        handleEventWrapper
        update

--- Draw Functions
drawGrid :: GameState -> Picture
drawGrid (GameState grid score gen) = pictures [drawTile x y val | x <- [0..3], y <- [0..3], let val = grid !! x !! y]

drawScoreBoard :: GameState -> Picture
drawScoreBoard (GameState _ score _) =
    translate (-200) (-250) $
      color black $
      scale 0.3 0.3 $
      text ("Score: " ++ show score)

draw :: GameState -> Picture
draw gameState =
  if isGameOver gameState
    then pictures
      [ drawTitle
      , drawGrid gameState
      , drawGameOverBanner
      , drawScoreBoard gameState
      ]
    else pictures
    [ drawTitle
    , drawGrid gameState
    , drawScoreBoard gameState
    ]

drawTitle :: Picture
drawTitle = pictures $ 
  ([translate 60 200 $ color black $ scale 0.4 0.4 $ text "2048!"])

drawGameOverBanner :: Picture
drawGameOverBanner = pictures $
  translate 0 0
            (color red $ rectangleSolid 450 120) :
  ([translate (-150) 0 $ color white $ scale 0.4 0.4 $ text "Game Over!"])

drawTile :: Int -> Int -> Int -> Picture
drawTile x y val =
    pictures $
         translate (fromIntegral x * 100 - 150) (fromIntegral y * 100 - 150)
            (color (tileColor val) $ rectangleSolid 90 90) :
          ([translate (fromIntegral x * 100 - 170 + offsetX) (fromIntegral y * 100 - 170 + offsetY) $
            color white $
            scale 0.2 0.2 $
            text (show val) | val /=0])
  where
    textWidth = textWidth' (show val)
    offsetX = 22 - textWidth / 2
    offsetY = 7

    textWidth' text = fromIntegral (length text) * 20

tileColor :: Int -> Color
tileColor 0 = makeColorI 200 200 200 255
tileColor 2 = makeColorI 230 200 100 255
tileColor 4 = makeColorI 140 210 130 255
tileColor 8 = makeColorI 40 120 165 255
tileColor 16 = makeColorI 230 150 170 255
tileColor 32 = makeColorI 90 90 120 255
tileColor 64 = makeColorI 100 70 30 255
tileColor x = makeColorI r g b 255
  where
    floatx = fromIntegral x :: Float
    t = logBase 2 floatx
    r = (200 - (truncate t * 7)) :: Int
    g = (250 - (truncate t * 20)) :: Int
    b = (truncate t * 25) :: Int

isSolvable :: GameState -> Bool
isSolvable (GameState grid score gen) =
    any canMove [grid, transpose grid]
  where
    canMove :: [[Int]] -> Bool
    canMove g = any (\row -> row /= checkMergeAndShift row) g || any (\col -> col /= checkMergeAndShift col) g

    checkMergeAndShift :: [Int] -> [Int]
    checkMergeAndShift row = shiftRow $ checkMerge $ shiftRow row

    checkMerge :: [Int] -> [Int]
    checkMerge [] = []
    checkMerge [x] = [x]
    checkMerge (x:y:rest)
        | x == y    = x * 2 : checkMerge rest
        | otherwise = x : checkMerge (y:rest)

isGameOver :: GameState -> Bool
isGameOver (GameState grid score gen) = (not (any (elem 0) grid)) && (not (isSolvable (GameState grid score gen)))

-- Handling user input
handleEvent :: Event -> State GameState ()
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) = do
  move North
  updateBoard
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) = do
  move South
  updateBoard
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) = do
  move West
  updateBoard
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) = do
  move East
  updateBoard
handleEvent _ = return ()

update :: Float -> GameState -> GameState
update _ = id

mergeRow :: [Int] -> ([Int], Int)
mergeRow [] = ([], 0)
mergeRow [x] = ([x], 0)
mergeRow (x:y:rest)
    | x == y = let
                  mergedValue = x * 2
                  (restResult, additionalScore) = mergeRow rest
               in (mergedValue : restResult, additionalScore + mergedValue)
    | otherwise = let
                    (restResult, additionalScore) = mergeRow (y:rest)
                  in (x : restResult, additionalScore)

shiftRow :: [Int] -> [Int]
shiftRow row =  take 4 (filter (/= 0) row ++ replicate 4 0)

mergeAndShiftRow :: [Int] -> State GameState [Int]
mergeAndShiftRow row = do
  currentState <- get
  let (merged, addscore) = mergeRow $ shiftRow row
  let newScore = score currentState + addscore
  let newState = currentState {score = newScore}
  put newState
  return $ shiftRow merged

move :: Direction -> State GameState ()
move dir = do
  currentState <- get
  let newGrid = case dir of
                  West  -> transpose <$> mapM mergeAndShiftRow (transpose $ board currentState)
                  East  -> transpose . map reverse <$> mapM (mergeAndShiftRow . reverse) (transpose $ board currentState)
                  North -> map reverse <$> mapM (mergeAndShiftRow . reverse) (board currentState)
                  South -> mapM mergeAndShiftRow (board currentState)
  let (finalBoard, finalState) = runState newGrid currentState
  put finalState {board = finalBoard}
  return ()

updateBoard :: State GameState () -- add a new tile
updateBoard = do
  gameState <- get
  if not (isGameOver gameState)
    then do
      let (x, y) = generateRandomPosition gameState
          (prob, newGen) = randomR (0, 99 :: Int) (gen gameState)
          newTile = if prob < (90 :: Int) then 2 else 4 -- 90% chance that a tile is a 2, 10% chance that a tile is 4
      let newGrid = putTileOnBoard (board gameState) (x, y) newTile
      put $ gameState {board = newGrid, gen = newGen}
    else return ()