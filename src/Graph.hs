module Graph where

import Data

import qualified Data.Map as M
import Control.Arrow (second)
import Data.Maybe
import Data.Ord
import Data.List
import Debug.Trace


transHull :: [GameSum] -> [Edge]
transHull = foldr insert [] . map fst . sortBy (comparing snd)
  where insert (p1,p2) graph
         | reachable graph p2 p1 = graph -- Do not build cycles
         | reachable graph p1 p2 = graph -- No useful edge
	 | otherwise             = (p1,p2) : removeObs (p1,p2) graph

-- | Removes any edge pointing towards p2 that can also reach p1
removeObs (p1,p2) graph = filter go graph
  where go (p1', p2') = not $  p2' == p2 && reachable graph p1' p1
                            || p1' == p1 && reachable graph p2 p2'


-- | Tests whether a player is reachable. Stupid algorithm, breaks when there
--   are cycles
reachable :: [Edge] -> Player -> Player -> Bool 
reachable g p1 p2  = go p1
  where go p = any (\(_,p') -> p2 == p' || go p') $ filter ((==) p . fst) g

sumGames :: [Game] -> [GameSum]
sumGames = mapMaybe flipRight . M.toList . M.map sumUp .
           M.fromListWith (++) .  map (second (:[]) .  sortName)
  where sumUp :: [Result] -> [Integer]
        sumUp ress = [ sum (map gameCount ress)
	             , sum (map (uncurry (-)) ress)
		     ]
	flipRight ((p1,p2),l) =  case dropWhile (==0) l of
		[]           -> Nothing
		n:_ | n > 0  -> Just ((p1,p2),l)
		    | n < 0  -> Just ((p2,p1),map negate l)

	sortName x@((p1,p2),(c1,c2)) | p1 <= p2  = x
	                             | otherwise = ((p2,p1),(c2,c1))

gameCount (a,b) | a > b     = 1
                | a < b     = -1
                | otherwise = 0
