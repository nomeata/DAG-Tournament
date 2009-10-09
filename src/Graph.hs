module Graph where

import Data

import qualified Data.Map as M
import Control.Arrow (second, (***))
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
           M.fromListWith (\(a,b) (a',b') -> (a ++ a', b `max` b')) .
	   zipWith (\n (a,b) -> (a,([b],n))) [1,2..] .
	   map sortName
  where sumUp :: ([Result],Integer) -> (Integer, Integer, Integer)
        sumUp (ress,o) = ( sum (map gameCount ress)
	                 , sum (map (uncurry (-)) ress)
			 , o
		         )
	flipRight ((p1,p2),c@(c1,c2,c3)) =
		if c1 == 0 && c2 == 0
		then Nothing
		else if c >= (0,0,0)
		     then Just ((p1,p2),c)
		     else Just ((p2,p1),(-c1,-c2,c3))

	sortName x@((p1,p2),(c1,c2)) | p1 <= p2  = x
	                             | otherwise = ((p2,p1),(c2,c1))

gameCount (a,b) | a > b     = 1
                | a < b     = -1
                | otherwise = 0

countGames :: [Game] -> [(Player, Integer)]
countGames = M.toList . M.fromListWith (+) . concat . map (\((p1,p2),_) -> [(p1,1),(p2,1)])
