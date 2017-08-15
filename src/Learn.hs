-- Author: Noah Halford
-- Learn.hs
module Learn where

import Taxi
import AI.HNN.FF.Network
import Numeric.LinearAlgebra
import qualified Data.Vector as V
import System.Random
import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Data.List.Extras.Argmax (argmaxes)
import Data.Digits (digits, unDigits)
import Data.Random.Extras (choice)
import Data.Random.RVar (runRVar)
import Data.Random.Source.DevRandom

-- possible actions
data GameAction = ActionNorth | ActionSouth | ActionEast | ActionWest
                | ActionPickup | ActionDropoff
     deriving (Bounded, Enum, Eq, Show)

instance Random GameAction where
    randomR (minV, maxV) g = (toEnum r, g')
        where (r, g') = randomR (minV', maxV') g
              minV' = fromEnum minV :: Int
              maxV' = fromEnum maxV :: Int
    random g = (toEnum r, g')
        where (r, g') = randomR (fromEnum minB, fromEnum maxB) g
              minB = minBound :: GameAction
              maxB = maxBound :: GameAction

-- convert a GameAction into a movement
doAction :: GameAction -> (Game -> Game)
doAction ActionNorth = move North
doAction ActionSouth = move South
doAction ActionEast = move East
doAction ActionWest = move West
doAction ActionDropoff = dropoffAction
doAction ActionPickup = pickupAction

-- convert n to a binary list of k digits, padded with zeros on the left
binary :: Int -> Int -> [Int]
binary k n = reverse $ take k $ (reverse $ digits 2 n) ++ repeat 0

-- helper function for randomAction (randomly pick from possible argmax values)
argmax' :: Ord b => (a -> b) -> [a] -> IO a
argmax' f l = runRVar (choice $ argmaxes f l) DevRandom

-- pick an action at random with probability epsilon; otherwise pick argmax
randomAction :: Double
             -> ([Double] -> GameAction -> Numeric.LinearAlgebra.Vector Double)
             -> [Double]
             -> Int
             -> IO GameAction
randomAction eps q g t = do
    gen <- newStdGen
    let (minA, maxA) = (minBound, maxBound) :: (GameAction, GameAction)
    let q' action = head $ toList $ q g action
    let (p, _) = randomR (0,1) gen :: (Double, StdGen)
    if p <= eps then getStdRandom random
                 else argmax' q' [minA..maxA]


-- encode game into list that will serve as input to NN
encodeGame :: Game -> [Double]
encodeGame g = encPosition g ++ encPickedUp g

-- has the passenger been picked up?
encPickedUp :: Game -> [Double]
encPickedUp g
    | pickup g == Nothing = [1]
    | otherwise = [0]

-- length is six; x and y encoded in binary
encPosition :: Game -> [Double]
encPosition g = map fromIntegral $ binary 3 (xPos g `div` 2) ++ binary 3 (yPos g)

-- given an encoded state as a list of doubles, extract the game that it encodes
-- input is [x,x,x,y,y,y,p]
decodeState :: [Double] -> Game
decodeState s = defaultGame { xPos = x', yPos = y', pickup = pick }
    where x = map round $ take 3 s
          y = map round $ take 3 $ drop 3 s
          p = last s
          x' = 1 + 2 * unDigits 2 x
          y' = unDigits 2 y
          pick = if p == 0 then (pickup defaultGame) else Nothing

-- from state s and action a, find the next state
nextState :: GameAction -> [Double] -> [Double]
nextState a = encodeGame . doAction a . decodeState

-- target q value from state, action, q function
-- alpha, gamma values suggested from Wikipedia
targetQ :: [Double]
        -> GameAction
        -> ([Double]
        -> GameAction
        -> Numeric.LinearAlgebra.Vector Double)
        -> Double
targetQ s a q
    | r == 1 = 1 -- terminal state, game over
    | otherwise = r + gamma * maxQ
    where gamma = 0.95
          g = decodeState s
          r = reward g (doAction a g)
          s' = nextState a s
          maxQ = maximum $ concatMap (toList . q s') $ [minBound..maxBound]

-- compute reward for going from game g1 to g2
-- reward defined in description at https://gym.openai.com/envs/Taxi-v1
-- Note that most of the work is done in scoring mechanisms elsewhere
reward :: Game -> Game -> Double
reward g1 g2 = 0.05 * (fromIntegral $ (score g2) - (score g1))

-- Given neural network, create Q(s, a)
qFromNN :: Network Double -> ([Double] -> GameAction -> Numeric.LinearAlgebra.Vector Double)
qFromNN net = \s a -> output net tanh $ fromList $ (actionValue a) ++ s

-- convert action to binary
actionValue :: GameAction -> [Double]
actionValue a = map fromIntegral $ binary 3 $ fromEnum a

-- get a random game for training
randomGame :: IO Game
randomGame = do
    x <- randomRIO (0,4) :: IO Int
    y <- randomRIO (0,4) :: IO Int
    p <- randomRIO (0,1) :: IO Int
    let xBin = binary 3 x
    let yBin = binary 3 y
    let l = xBin ++ yBin ++ [p] -- encoded binary list
    return $ decodeState $ map fromIntegral l

-- train neural network to compute Q
trainQNet :: Network Double -> Int -> IO (Network Double)
trainQNet net iters = do
    game <- newGame
    trainOnGame net game 1 iters []

-- function to play the game and train
trainOnGame :: Network Double -> Game -> Int -> Int -> [Sample Double] -> IO (Network Double)
trainOnGame net game t iters s = do
    if iters == 0 then return net else do
        if droppedOff game then do -- game over, start again
            print $ score game
            g <- randomGame
            trainOnGame net g 1 (iters - 1) s
        else do
            let gameState = encodeGame game
            let diff = 100 / (fromIntegral iters)
            let eps = max 0.1 $ (1 - diff) * (1 / (fromIntegral t))
            a <- randomAction eps (qFromNN net) gameState t -- get the action
            let aVal = actionValue a
            let inputList = aVal ++ gameState
            let s' = filter (\(l,_) -> l /= fromList inputList) s
            let target = targetQ gameState a (qFromNN net) -- target Q value
            let samples = [(fromList inputList, fromList [target])]
            let net' = trainNTimes 500 0.3 tanh tanh' net samples -- training
            let game' = doAction a game
            trainOnGame net' game' (t + 1) iters samples

-- set up neural network
trainNN :: Int -> IO ()
trainNN iters = do
    -- x, y, pickup, dropoff, action
    -- [x,x,x,y,y,y,p,a,a,a]
    let nInputs = 10
    let nHidden = 40
    let fstWeights = diagRect (-0.1) (fromList []) nHidden (nInputs + 1)
    let sndWeights = diagRect (0.05) (fromList []) 1 nHidden
    let net = fromWeightMatrices $ V.fromList [fstWeights, sndWeights]
    net' <- trainQNet net iters
    return ()
