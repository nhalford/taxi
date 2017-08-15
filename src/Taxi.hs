-- Author: Noah Halford
-- Taxi.hs
-- some functions, notably keyToDirection, filterMapJust, and networkDescription, were adapted
-- from the tutorial at https://wiki.haskell.org/FRP_explanation_using_reactive-banana
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Taxi where

import Data.Maybe (fromJust)
import Data.Array
import Data.List (sortBy)
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Random (randomRIO)
import Control.Monad (forever)
import System.IO (BufferMode(..), hSetEcho, hSetBuffering, stdin)
import System.Console.ANSI (clearScreen)

-------------------------
---- Data structures ----
-------------------------

-- different types of spaces on the board
data Space = SpaceR | SpaceG | SpaceY | SpaceB | ClearWall | BlockedWall | SpaceBlank
    deriving (Eq, Enum)

-- for drawing the board
instance Show Space where
    show ClearWall = ":"
    show BlockedWall = "|"
    show SpaceBlank = " "
    show SpaceR = "R"
    show SpaceG = "G"
    show SpaceY = "Y"
    show SpaceB = "B"

-- board data type
type Board = Array ((Int, Int)) Space

-- this is a little ugly because it's indexed in reverse
instance {-# OVERLAPS #-} Show Board where
    show b = unlines $ topBottom
           : (groupsOf (length topBottom) ( concatMap show
                                          $ map snd
                                          $ sortBy compareFst
                                          $ map flipFirst
                                          $ assocs b))
           ++ [topBottom]

-- helper function for Board Show instance
flipFirst :: ((a, b), c) -> ((b, a), c)
flipFirst ((x, y), z) = ((y, x), z)

-- helper function for Board Show instance
compareFst :: Ord a => (a, b) -> (a, b) -> Ordering
compareFst (x, _) (y, _) = compare x y

-- helper to turn list into list of lists, each with n elements
groupsOf :: Int -> [a] -> [[a]]
groupsOf n l
    | length l <= n = [l]
    | otherwise = front : groupsOf n back
    where (front, back) = splitAt n l

data Direction = North | South | East | West

data Game = Game { board :: Board
                 , xPos, yPos :: Int
                 -- pickup and dropoff positions: R, G, Y, B; pickup is Nothing if already picked up
                 , pickup , dropoff :: Maybe Space
                 , droppedOff :: Bool
                 , score :: Int
                 }

instance Show Game where
    show g = instructions ++ showGame g

-- for display
topBottom :: String
topBottom = "+---------+" 

showScore :: Game -> String
showScore g
    | droppedOff g = "Final score: " ++ (show $ score g)
    | otherwise = "Score: " ++ (show $ score g)

-- draw game board
showGame :: Game -> String
showGame g
    | pickup g == Nothing && dropoff g == Nothing = defaultShow
    | dropoff g == Nothing = colorPickup pg defaultShow
    | pickup g == Nothing = colorDropoff dg defaultShow
    | otherwise = colorDropoff dg $ colorPickup pg defaultShow
    where defaultShow = (replacePosition (length topBottom) (xPos g) (1 + yPos g) 'X' (show $ board g))
                      ++ "\n" ++ (showScore g)
          pg = head $ show $ fromJust $ pickup g
          dg = head $ show $ fromJust $ dropoff g

-- display instructions
instructions :: String
instructions = unlines $ [ "Pick up from the\x1b[34m blue\x1b[0m letter with Q"
                         , "Drop off on the\x1b[35m magenta\x1b[0m letter with E"
                         , "R for new game"
                         , "WASD to move"
                         , ""
                         , ""
                         , ""
                         ]

-- black magic to color pickup character blue
-- source: http://www.markhneedham.com/blog/2012/04/29/haskell-colour-highlighting-when-writing-to-the-shell/
colorPickup :: Char -> String -> String
colorPickup c s
    | s == "" = ""
    | head s == c = "\x1b[34m" ++ [c] ++ "\x1b[0m" ++ tail s
    | otherwise = (head s) : colorPickup c (tail s)

-- black magic to color dropoff character magenta
-- source: http://www.markhneedham.com/blog/2012/04/29/haskell-colour-highlighting-when-writing-to-the-shell/
colorDropoff :: Char -> String -> String
colorDropoff c s
    | s == "" = ""
    | head s == c = "\x1b[35m" ++ [c] ++ "\x1b[0m" ++ tail s
    | otherwise = (head s) : colorDropoff c (tail s)

-- given a string s that will be shown as a grid of width w, replace the character at (x, y)
-- (indexed from (0,0) in top left) with character c
-- note that here w DOES NOT include the newline character, which is why we add 1 when
-- computing the index ix
replacePosition :: Int -> Int -> Int -> Char -> String -> String
replacePosition w x y = replaceIndex (y * (1 + w) + x)

-- replace element at position ix in the list l with x
replaceIndex :: Int -> a -> [a] -> [a]
replaceIndex ix x l
    | ix >= length l = l
    | otherwise = start ++ [x] ++ (tail end)
    where (start, end) = splitAt ix l

-- default game setup is an array constructed from a list
defaultBoard :: Board
defaultBoard = array ((0,0),(10,4)) $ assocList
    where assocList = zipWith (,) ixList positions
          ixList = flip (,) <$> [0..4] <*> [0..10]
          positions = [ BlockedWall, SpaceR, ClearWall, SpaceBlank, BlockedWall, SpaceBlank
                      , ClearWall, SpaceBlank, ClearWall, SpaceG, BlockedWall
                      , BlockedWall, SpaceBlank, ClearWall, SpaceBlank, ClearWall, SpaceBlank
                      , ClearWall, SpaceBlank, ClearWall, SpaceBlank, BlockedWall 
                      , BlockedWall, SpaceBlank, ClearWall, SpaceBlank, ClearWall, SpaceBlank
                      , ClearWall, SpaceBlank, ClearWall, SpaceBlank, BlockedWall 
                      , BlockedWall, SpaceBlank, BlockedWall, SpaceBlank, ClearWall, SpaceBlank
                      , BlockedWall, SpaceBlank, ClearWall, SpaceBlank, BlockedWall 
                      , BlockedWall, SpaceY, BlockedWall, SpaceBlank, ClearWall, SpaceBlank
                      , BlockedWall, SpaceB, ClearWall, SpaceBlank, BlockedWall 
                      ]

-- standard new game
defaultGame :: Game
defaultGame = Game { board = defaultBoard
                   , xPos = 3
                   , yPos = 0
                   , pickup = Just SpaceR
                   , dropoff = Just SpaceG
                   , droppedOff = False
                   , score = 0
                   }

------------------------
---- Game mechanics ----
------------------------

-- find pickup/dropoff locations for game
-- note that pickupLocation may be Nothing if we've already picked up
pickupLocation, dropoffLocation :: Game -> Maybe (Int, Int)
pickupLocation g
    | p == Nothing = Nothing
    | not $ pchar `elem` "RGBY" = Nothing
    | length locs == 0 = Nothing
    | otherwise = Just $ head locs
    where p = pickup g
          jp = fromJust p
          pchar = head $ show $ fromJust  p
          locs = map fst $ filter (\(_, s) -> s == jp) (assocs $ board g)

dropoffLocation g
    | d == Nothing = Nothing
    | not $ dchar `elem` "RGBY" = Nothing
    | length locs == 0 = Nothing
    | otherwise = Just $ head locs
    where d = dropoff g
          jd = fromJust d
          dchar = head $ show $ fromJust d
          locs = map fst $ filter (\(_, s) -> s == jd) (assocs $ board g)

-- are we in the pickup location?
canPickup :: Game -> Bool
canPickup g
    | pickupLocation g == Just (xPos g, yPos g) = True
    | otherwise = False

-- are we in the dropoff location?
canDropoff :: Game -> Bool
canDropoff g
    | pickupLocation g /= Nothing = False
    | dropoffLocation g == Just (xPos g, yPos g) = True
    | otherwise = False

-- attempt to pick up the passenger
pickupAction :: Game -> Game
pickupAction g
    | score g < -1000 = g { score = score g - 10, droppedOff = True }
    | droppedOff g = g
    | canPickup g = g { pickup = Nothing }
    | otherwise = g { score = score g - 10 }

-- attempt to drop off the passenger
dropoffAction :: Game -> Game
dropoffAction g
    | score g < -1000 = g { score = score g - 10, droppedOff = True }
    | droppedOff g = g
    | canDropoff g = g { droppedOff = True, dropoff = Nothing , score = score g + 20}
    | otherwise = g { score = score g - 10 }

-- determine if we can move to the point p = (x, y)
-- note that order of evaluation is important because of laziness
canMoveTo :: (Int, Int) -> Board -> Bool
canMoveTo p b = p `elem` (indices b) && (b ! p) /= BlockedWall

-- this is slightly complicated becasue we are never actually on the walls
-- so x movement actually moves by two
move :: Direction -> Game -> Game
move North = moveY (-1)
move South = moveY 1
move East = moveX 2
move West = moveX (-2)

-- move in X or Y direction
moveX, moveY :: Int -> Game -> Game
moveX x g
    | score g < -1000 = g { droppedOff = True }
    | x == 0 || droppedOff g = g
    -- if we can move to every point in our way, then move there
    | and $ map (flip canMoveTo (board g)) pts = g { xPos = x', score = score' }
    | otherwise = g { score = score' }
    where pts = map (flip (,) (yPos g)) $ map (+ (xPos g)) [0,(signum x)..x]
          x' = x + xPos g
          score' = score g - 1

moveY y g
    | score g < -1000 = g { droppedOff = True } -- end game; speeds up NN training
    | y == 0 || droppedOff g = g
    -- if we can move to every point in our way, then move there
    | and $ map (flip canMoveTo (board g)) pts = g { yPos = y', score = score' }
    | otherwise = g { score = score' }
    where pts = map ((,) (xPos g)) $ map (+ (yPos g)) [0,(signum y)..y]
          y' = y + yPos g
          score' = score g - 1

-- all pairs of pickup/dropoff spaces
spacePairs :: [(Space, Space)]
spacePairs = filter (not . same) $ (,) <$> spaces <*> spaces
    where same (x, y) = x == y
          spaces = [SpaceR,SpaceG,SpaceY,SpaceB]

-- convert key presses to directions
keyToDirection :: Char -> Maybe Direction
keyToDirection 'w' = Just North
keyToDirection 's' = Just South
keyToDirection 'd' = Just East
keyToDirection 'a' = Just West
keyToDirection _ = Nothing

filterMapJust :: (a -> Maybe b) -> Event a -> Event b
filterMapJust f = filterJust . fmap f

-- clear screen and then print; makes it look like an animation
clearPrint :: Show a => a -> IO ()
clearPrint s = do
    clearScreen
    print s

-- a new random game
newGame :: IO Game
newGame = do
    let l = length spacePairs
    -- pickup, dropoff
    (p, d) <- fmap ((!!) spacePairs) $ randomRIO (0, l-1)
    x <- fmap ((+1) . (*2)) $ randomRIO (0, 4) :: IO Int
    y <- randomRIO (0, 4) :: IO Int
    let game = defaultGame { xPos = x
                           , yPos = y
                           , pickup = Just p
                           , dropoff = Just d }
    return game

-- main function for gameplay
playGame :: IO ()
playGame = do
    game <- newGame
    clearPrint game

    let networkDescription :: AddHandler Char -> MomentIO ()
        networkDescription addKeyEvent = mdo

            bnext <- fromPoll newGame
            nextGame <- valueB bnext

            -- interpret key presses
            ekey <- fromAddHandler addKeyEvent
            let emove = filterMapJust keyToDirection ekey
            let epickup = filterE (== 'q') ekey
            let edropoff = filterE (== 'e') ekey
            let ereset = filterE (== 'r') ekey
            bmove <- accumB game $ unions
                [ move <$> emove
                , pickupAction <$ epickup
                , dropoffAction <$ edropoff
                , (\g1 _ -> g1) <$> bnext <@ ereset
                ]
            echanged <- changes bmove
            reactimate' $ fmap clearPrint <$> echanged

    -- set up the network for interaction
    (addKeyEvent, fireKey) <- newAddHandler
    network <- compile $ networkDescription addKeyEvent
    actuate network 
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    forever (getChar >>= fireKey)
