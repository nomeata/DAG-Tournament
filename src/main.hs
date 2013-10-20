{-# LANGUAGE RecordWildCards #-}

module Main where

import Data
import Graph
import HDAPS
import Physics

import Graphics.UI.Gtk hiding (eventKeyName, eventModifier, eventClick, eventButton)
import Graphics.Rendering.Cairo 
import Graphics.UI.Gtk.Gdk.Events
import Data.Time
import Data.IORef
import Data.Complex
import Control.Monad
import Control.Applicative
import Text.Printf
import Data.List
import System.Environment
import Data.Maybe
import System.Directory
import System.FilePath

fontSize = 1/50

removePlayers (GameState players _) uis = uis { uisPositions = filter (\(p,_) -> p `elem` players) (uisPositions uis) }

addNewPlayers (GameState players _) uis = foldr go uis players
  where go player uis | player `elem` map fst (uisPositions uis) = uis
                      | otherwise = let x = fromIntegral ((4 + length (uisPositions uis)) `mod` 10) 
		                    in uis { uisPositions = (player,((x*0.1 + 0.05) :+ 0.3)) :
				                            uisPositions uis }

setHover uiState = uiState { uisHover = msum $ map check (uisBBoxes uiState) }
  where (mx, my) = uisMousePos uiState
        check (player, (x,y,w,h)) =
  		if x <= mx && mx <= x + w &&
		   y <= my && my <= y + h
		then Just player else Nothing

updateUIState elapsedTime down gameState spanningTree =
	applyForces elapsedTime down gameState spanningTree .
	removePlayers gameState .
	addNewPlayers gameState .
	setHover

playedWith curGame gameState p1 p2 = let Just games = lookup curGame (gsGames gameState)
                                         pairings =   map fst games
                                     in  (p1,p2) `elem` pairings || (p2,p1) `elem` pairings

tick :: IORef GameState -> IORef UIState -> DrawingArea -> IO ()
tick gameStateRef uiStateRef canvas = do
	-- Simulations logic here
	gameState <- readIORef gameStateRef
	uiState <- readIORef uiStateRef

	now <- getCurrentTime
	backThen <- uisLastFrameTime <$> readIORef uiStateRef
	modifyIORef uiStateRef (\uis -> uis { uisLastFrameTime = now })

	let elapsedTime = realToFrac (now `diffUTCTime` backThen)

	let spanningTree = transHull $ sumGames (getGames (uisCurrentGame uiState) gameState)

	down <- readTilt
	modifyIORef uiStateRef (updateUIState elapsedTime down gameState spanningTree)


redraw :: IORef GameState -> IORef UIState -> DrawingArea -> IO ()
redraw gameStateRef uiStateRef canvas = do
	gameState <- readIORef gameStateRef
	uiState@(UIState {..}) <- readIORef uiStateRef

	let spanningTree = transHull $ sumGames (getGames uisCurrentGame gameState)

	-- Graphics here
	render canvas $ do

	-- Background
	preserve $ setSourceRGB 1 1 1 >> paint

	-- Setup
	setLineWidth (1/1000)
	setFontSize fontSize
	let padding = (2/1000)

	let important = msum [uisDragStart, uisHover]

	let gameCounts = countGames (getGames uisCurrentGame gameState)

	-- Calculate bounding boxes for player while drawing them
	bb <- forM uisPositions $ \(player, x :+ y) -> preserve $ do
		let text1 = player
		let text2 = printf " (%d)" (fromMaybe 0 (lookup player gameCounts))
		let text = if uisShowGameCount 
		           then text1 ++ text2
			   else text1
                TextExtents xb1 yb1 w1 h1 _ _ <- textExtents text1
                TextExtents xb  yb  w  h _ _ <- textExtents text
		scale 1 (-1)
                translate x (-y)
		let (ux, uy, uw, uh) = 
			    ( -w1/2 + xb1 - padding
			    ,  h1/2 + yb1 - padding
			    ,  w   +      2*padding
			    ,  h   +      2*padding
			    )
		roundedRect ux uy uw uh

		if        Just player == uisDragStart
	          then		setSourceRGB 0 0 1 >> fillPreserve >> setSourceRGB 0 0 0 
		  else if Just player == uisHover
		  then		setSourceRGB 1 0 0 >> fillPreserve >> setSourceRGB 0 0 0 
		  else when (maybe False (playedWith uisCurrentGame gameState player) important) $ 
		      		setSourceRGB 1 1 0 >> fillPreserve >> setSourceRGB 0 0 0 
		stroke

                moveTo (-w1/2) (h1/2)
                showText text1
		when uisShowGameCount $ do
			setSourceRGB 0.5 0.5 0.5
			showText text2

		(dx,dy) <- userToDevice ux uy
		(dw,dh) <- userToDeviceDistance uw uh
		return (player, (dx,dy,dw,dh))
	
	forM_ spanningTree $ \(p1,p2) -> preserve $ do
		setLineWidth (4/1000)
		setLineCap LineCapRound
		let Just v1@(x1 :+ y1) = lookup p1 uisPositions
		let Just v2@(x2 :+ y2) = lookup p2 uisPositions
		let d = signum (v2 - v1)

		let rv1@(rx1 :+ ry1) = v1 + 2/100 * d
		let rv2@(rx2 :+ ry2) = v2 - 2/100 * d

		let (tlx :+ tly) = rv2 + 1/100 * cis (3/4*pi) * d
		let (trx :+ try) = rv2 + 1/100 * cis (-3/4*pi) * d

		moveTo rx1 ry1
		lineTo rx2 ry2
		moveTo rx2 ry2
		lineTo tlx tly
		moveTo rx2 ry2
		lineTo trx try
		stroke
	
	case uisHover of
	  Just player -> showStats uiState gameState player
	  Nothing -> return ()

	-- Only show game name if there is more than one
	case gsGames gameState of
	  [] ->  return ()
	  [_] -> return ()
	  _ ->   showGames uiState gameState
	
	liftIO $ modifyIORef uiStateRef (\uis -> uis { uisBBoxes = bb })

showStats uiState gameState player = do
	let texts = map (\((p1,p2),(s1,s2)) -> printf "%s vs. %s: %d:%d" p1 p2 s1 s2) $
                    filter (\((p1,p2),_) -> p1 == player || p2 == player) $
		    getGames (uisCurrentGame uiState) gameState

        unless (null texts) $ preserve $ do
                scale 1 (-1)
                translate 0 (-2*fontSize)

                forM_ texts $ \text -> do
                        setSourceRGB 0 0 0
                        moveTo 0 0
                        showText text
                        translate 0 (-fontSize)

showGames uiState gameState = preserve $ do
	newPath
	let text = uisCurrentGame uiState
	scale 1 (-1)
	TextExtents xb yb w h _ _ <- textExtents text
	translate (1-w-xb- 1/2*fontSize) (-1/2*fontSize)
	showText text
	

gameDialog :: Window -> Player -> Player -> IO (Maybe Game)
gameDialog window player1 player2 = do
	dlg <- messageDialogNew (Just window) [] MessageQuestion ButtonsOk "Please enter the game scores!"
	upper <- dialogGetUpper dlg
	
	hbox <- hBoxNew False 10
	l1 <- labelNew (Just player1)
	l2 <- labelNew (Just "vs.")
	l3 <- labelNew (Just player2)
	e1 <- entryNew
	e2 <- entryNew
	set e1 [ entryAlignment := 1, entryWidthChars := 3, entryText := "1", entryActivatesDefault := True ]
	set e2 [ entryAlignment := 0, entryWidthChars := 3, entryText := "0", entryActivatesDefault := True ]
	boxPackStartDefaults hbox l1
	boxPackStartDefaults hbox e1
	boxPackStartDefaults hbox l2
	boxPackStartDefaults hbox e2
	boxPackStartDefaults hbox l3

	boxPackStartDefaults upper hbox

	dialogSetDefaultResponse dlg ResponseOk
	widgetShowAll dlg
	responseId <- dialogRun dlg

	ret <- if responseId == ResponseOk then do
		t1 <- get e1 entryText
		t2 <- get e2 entryText
		case (maybeRead t1, maybeRead t2) of
		 (Just s1, Just s2) -> return (Just ((player1,player2),(s1,s2)))
		 _ -> return Nothing
	  else return Nothing

	widgetDestroy dlg

	return ret

addNewPlayer :: Window -> IORef GameState -> IO ()
addNewPlayer window gameStateRef = do
	gameState <- readIORef gameStateRef
	dlg <- messageDialogNew (Just window) [] MessageQuestion ButtonsOk "Please enter the name of the player!"
	upper <- dialogGetUpper dlg

	let defaultText = "Player " ++ [head (drop (length (gsPlayer gameState)) (cycle $ ['X','Y','Z'] ++ ['A'..'W']))]
	
	hbox <- hBoxNew False 10
	e1 <- entryNew
	set e1 [ entryAlignment := 0, entryWidthChars := 10, entryText := defaultText, entryActivatesDefault := True ]
	boxPackStartDefaults hbox e1

	boxPackStartDefaults upper hbox

	dialogSetDefaultResponse dlg ResponseOk
	widgetShowAll dlg

	responseId <- dialogRun dlg

	when (responseId == ResponseOk) $ do
		name  <- get e1 entryText
		modifyIORef gameStateRef (\gs -> gs { gsPlayer = nub $ name : gsPlayer gs })
		saveGameState gameStateRef

	widgetDestroy dlg

editGameState :: Window -> IORef GameState -> IO ()
editGameState window gameStateRef = do
	dlg <- messageDialogNew (Just window) [] MessageQuestion ButtonsOk "Edit the game state."
	upper <- dialogGetUpper dlg
	
	tv <- textViewNew
	tb <- textViewGetBuffer tv
	set tv [ textViewWrapMode := WrapWord ]

	readIORef gameStateRef >>= textBufferInsertAtCursor tb . show
	
	boxPackStartDefaults upper tv

	dialogSetDefaultResponse dlg ResponseOk
	widgetShowAll dlg

	responseId <- dialogRun dlg

	when (responseId == ResponseOk) $ do
		text <- get tb textBufferText
	  	case maybeRead text of
		    Just gs -> writeIORef gameStateRef gs >> saveGameState gameStateRef
		    Nothing -> return ()

	widgetDestroy dlg

saveGameState :: IORef GameState -> IO ()
saveGameState gameStateRef = do
	home <- getHomeDirectory
	readIORef gameStateRef >>= writeFile (home </> ".DAG-Tournamet.backup") . show

main = do
	games <- (\a -> if null a then ["XXX"] else a) <$> getArgs
	now <- getCurrentTime
	gameStateRef <- newIORef $ GameState [] (map (\gn -> (gn,[])) games)
	uiStateRef <- newIORef $ UIState [] (head games) [] (0,0) Nothing Nothing False now

        initGUI
        window <- windowNew
        canvas <- drawingAreaNew
        onDestroy window mainQuit

	onExpose canvas $ const $ do
		tick gameStateRef uiStateRef canvas
		redraw gameStateRef uiStateRef canvas 
		return True

	onMotionNotify canvas False $ \e -> do 
		modifyIORef uiStateRef (\uis -> uis { uisMousePos = (eventX e, eventY e) })
		return True

	onButtonPress canvas $ \e -> do
                when (eventButton e == LeftButton && eventClick e == SingleClick) $ 
			modifyIORef uiStateRef $ \uis -> uis { uisDragStart = uisHover uis }

		when (eventButton e == LeftButton && eventClick e == DoubleClick) $
			addNewPlayer window gameStateRef

		return True

        onButtonRelease canvas $ \e -> do
		uiState <- readIORef uiStateRef
		case (uisDragStart uiState, uisHover uiState) of
		  (Just p1, Just p2) | p1 /= p2 -> do
		  	ret <- gameDialog window p1 p2
			case ret of
			 Just game -> do
			 	modifyIORef gameStateRef $ \gs ->
					gs { gsGames = map (\(gn,gs) ->
						(gn, if gn == uisCurrentGame uiState then game : gs else gs)
					) (gsGames gs) }
			        saveGameState gameStateRef
			 Nothing -> return ()
		  _ -> return ()
		modifyIORef uiStateRef $ \uis -> uis { uisDragStart = Nothing }
		return True

		return False

        onKeyPress window $ \e -> do
		when (eventModifier e == [Control] && eventKeyChar e == Just 'e') $
			editGameState window gameStateRef
		when (eventModifier e == [Control] && eventKeyChar e == Just 'r') $
			modifyIORef uiStateRef (\uis -> uis { uisPositions = [] })
		when (eventKeyName e == "Tab") $
			modifyIORef uiStateRef (\uis -> uis {
				uisCurrentGame = followingElement (uisCurrentGame uis) games
				})
		when (eventKeyName e `elem` ["Shift_L","Shift_R"]) $
			modifyIORef uiStateRef (\uis -> uis {
				uisShowGameCount = True })
                widgetQueueDraw canvas
		return False

        onKeyRelease window $ \e -> do
		when (eventKeyName e `elem` ["Shift_L","Shift_R"]) $
			modifyIORef uiStateRef (\uis -> uis {
				uisShowGameCount = False })
                widgetQueueDraw canvas
		return False

	flip timeoutAdd 30 $ do 
                widgetQueueDraw canvas
                return True
	
	set window [containerChild := canvas]
        windowFullscreen window
        widgetShowAll window
        mainGUI


render canvas r = do
        win <- widgetGetDrawWindow canvas
        (w, h) <- widgetGetSize canvas
        let s = fromIntegral w
        renderWithDrawable win $ do
                selectFontFace "DejaVu Sans" FontSlantNormal FontWeightNormal
		translate 0 (fromIntegral h)
                scale s (-s)
                r

roundedRect x y w h = do
	moveTo       x            (y+pad)
	lineTo       x            (y + h - pad)
	arcNegative (x + pad)     (y + h - pad) pad pi (pi/2)
	lineTo      (x + w - pad) (y + h)
	arcNegative (x + w - pad) (y + h - pad) pad (pi/2) 0
	lineTo      (x + w)       (y + pad)
	arcNegative (x + w - pad) (y + pad)     pad 0  (-pi/2)
	lineTo      (x + pad)      y 
	arcNegative (x + pad)     (y + pad)     pad (-pi/2) (-pi)
	closePath

  where pad = 1/10 * min w h 

preserve r = do
	save
	v <- r
	restore
	return v

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing

getGames :: GameName -> GameState -> [Game]
getGames gameName = fromJust . lookup gameName . gsGames

followingElement e l = go (cycle l)
  where go (x:xs) = if x == e then head xs else go xs
