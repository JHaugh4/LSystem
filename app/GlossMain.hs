module GlossMain where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import qualified Graphics.Gloss.Data.Point.Arithmetic as PA

import Data.List

import qualified LSystem as LS

import qualified Debug.Trace as DT

-- Functions to animate line drawing
animateLine :: Int -> Point -> Point -> [Path]
animateLine len start@(startX, startY) (endX, endY) =
    take (len + 1) $ map (\p -> [start, p]) $ iterate' (PA.+ step) start
    where
        len' = fromIntegral len
        (diffX, diffY) = (startX - endX, startY - endY)
        step = ((-diffX) / len', (-diffY) / len')

-- advanceAnimation :: FBTS -> FBTS

type PosAngle = (Point, Float)

changePosition :: Point -> PosAngle -> PosAngle
changePosition np (p, a) = (np, a)

changeAngle :: Float -> PosAngle -> PosAngle
changeAngle na (p, a) = (p, na)

turn :: Float -> PosAngle -> PosAngle
turn d (p, a) = (p, a + d)

-- Function to draw a line at a given angle of a given distance
-- from a given point
lineFrom :: PosAngle -> Float -> (Point, Point)
lineFrom (point, angle) dist = (point, point PA.+ diff)
    where
        diff = (dist * cos angle, dist * sin angle)

-- Fractal Binary Tree
data FBTS = FBTS
    { _drawnLines :: [Picture]
    , _curAnimating :: [Path]
    , _input :: String
    , _curPosAngle :: PosAngle
    , _stack :: [PosAngle]
    , _segSize :: Float
    , _animLen :: Int
    } deriving (Eq, Show)

initialFBTS :: String -> Int -> FBTS
initialFBTS s ws =
    let pa = ((0, -(fromIntegral ws / 2)),  pi / 2)
    in FBTS [] [] s pa [] 2 3

nextInputChar :: FBTS -> FBTS
nextInputChar fbts = case _input fbts of
    []     -> fbts
    (c:cs) ->
        let fbts' = fbts { _input = cs }
        in case c of
            '0' ->
                let (start, end) = lineFrom (_curPosAngle fbts) (_segSize fbts)
                in fbts' { _curAnimating = animateLine (_animLen fbts) start end
                         , _curPosAngle =  changePosition end (_curPosAngle fbts)
                         }
            '1' ->
                let (start, end) = lineFrom (_curPosAngle fbts) (_segSize fbts * 2)
                in fbts' { _curAnimating = animateLine (_animLen fbts) start end
                         , _curPosAngle =  changePosition end (_curPosAngle fbts)
                         }
            '[' ->
                fbts' { _stack = _curPosAngle fbts : _stack fbts
                      , _curPosAngle = turn (pi / 4) (_curPosAngle fbts)
                      }
            ']' ->
                let newCurPosAngle = turn (-(pi / 4)) (head (_stack fbts))
                in fbts' { _curPosAngle = newCurPosAngle
                         , _stack = tail (_stack fbts)
                         }

isSingleton :: [a] -> Bool
isSingleton [x] = True
isSingleton _   = False

drawLine :: Path -> Picture
drawLine = color white . line

drawFBTS :: FBTS -> Picture
drawFBTS fbts
    | null (_input fbts) || null (_curAnimating fbts) = pictures (_drawnLines fbts)
    | otherwise = pictures (drawLine (head (_curAnimating fbts)) : _drawnLines fbts)

advance :: FBTS -> FBTS
advance fbts
    -- | DT.trace ("fbts = " ++ show fbts ++ "\n") False = undefined
    | null (_curAnimating fbts) = nextInputChar fbts
    | isSingleton (_curAnimating fbts) =
        let savedLine = head (_curAnimating fbts)
            fbts' = nextInputChar fbts
        in fbts' { _drawnLines = drawLine savedLine : _drawnLines fbts }
    | otherwise = fbts { _curAnimating = tail (_curAnimating fbts) }

evolve :: Char -> String
evolve '0' = "1[0]0"
evolve '1' = "11"
evolve c   = [c]

run :: IO ()
run = do
    -- Ask for the window size
    putStrLn "Enter the desired window size: "
    windowSizeStr <- getLine
    let ws = read windowSizeStr
    -- Create the window
    let window = InWindow "L System" (ws, ws) (0, 0)
    -- Now ask for the number of recursions
    putStrLn "Enter the desired number of recursions: "
    numRecStr <- getLine
    let numRec = read numRecStr
    let axiom = "0"
    let initGen = LS.lSystem evolve axiom !! numRec
    -- Now we can simulate
    simulate window black 60 (initialFBTS initGen ws) drawFBTS (\vp f -> advance)
