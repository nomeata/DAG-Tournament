module Data where

import Data.Complex
import Data.Time

type Player = String
type Edge = (Player, Player)
type Result = (Integer, Integer)
type Game = (Edge, Result)
type GameSum = (Edge, [Integer])

data GameState = GameState
	{ gsPlayer :: [Player]
	, gsGames :: [Game]
	}
	deriving (Show, Read)

-- | Coordinate system in use: (0,0) lower left, (1,0) lower right corner
type Coord = Complex Double

data UIState = UIState
	{ uisPositions :: [(Player, Coord)]
	, uisBBoxes :: [(Player, (Double, Double, Double, Double))] 
	, uisMousePos :: (Double, Double)
	, uisHover :: Maybe Player
	, uisDragStart :: Maybe Player
	, uisLastFrameTime :: UTCTime
	} 
	deriving (Show)
