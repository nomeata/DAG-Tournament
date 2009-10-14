{-# LANGUAGE PatternGuards #-}

module Physics where 

import Data

import Data.Complex
import Debug.Trace

-- Constants
gravityForce = 1/2
borderForce = 100
forceStrength = 1/10
repellStrength = 1
repellDist = 0.1
attractStrength = 10
angleShiftForce = 2

edgePadding = 10/100
botPadding = 2/100

-- Physics functions

gravity = 0 :+ negate gravityForce

repell (player1, c1) (player2, c2) =
	if player1 == player2
	then 0
	else let d = c2 - c1
	         m = magnitude d
	     in  - signum d * repellStrength * ((repellDist / m)^2 :+ 0)

attract (player, c) pos (player1, player2)
 	| player1 == player2 = 0 --safeguard
	| player  /= player1 && player /= player2 = 0
	| Nothing <- lookup player1 pos = 0 
	| Nothing <- lookup player2 pos = 0 
	| d == 0 = 0
	| otherwise = distForce + angleForce

  where winner = player == player1
  	other = if winner then player2 else player1
  	Just c2 = lookup other pos
        d = c2 - c
        distForce = d * attractStrength
	ddiff = normAngle $ phase d - if winner then -pi/2 else pi/2
	angleForce = cis (signum ddiff * pi/2) * signum d * (ddiff^2 :+ 0) * angleShiftForce
	

bound (x :+ y) = boundBottom . boundLeft . boundRight 
 where boundBottom = if y <    botPadding  then clampPhase (pi/2) else id
       boundLeft   = if x <    edgePadding then clampPhase 0      else id
       boundRight  = if x > 1- edgePadding then clampPhase pi     else id

applyForces elapsedTime down gameState spanningTree uis = uis { uisPositions = map go (uisPositions uis) }
  where go p@(player, coord) = let totalForce = sum $
  					down :
					map (repell p) (uisPositions uis) ++
					map (attract p (uisPositions uis)) spanningTree 
                           in  (player,
			        coord + bound coord totalForce * forceStrength * elapsedTime)

atLeast x = max x
atMost  x = min x

normAngle x | x <= -pi   = normAngle $ x + 2*pi
            | x > pi    = normAngle $ x - 2*pi
	    | otherwise = x

clampPhase p d = cis p * go (cis (-p) * d)
  where go (x :+ y) = atLeast 0 x :+ y
