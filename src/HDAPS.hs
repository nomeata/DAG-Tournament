module HDAPS where
import Data.Complex
import System.Directory
import System.IO

hdapsFile = "/sys/devices/platform/hdaps/position"

-- | Return unit vector pointing „down“, direction depending on the sideways
-- tilt, length 0 to 1 depending on the front-back-tilt
readTilt :: IO (Complex Double)
readTilt = do
	ex <- doesFileExist hdapsFile
	if not ex then return (0 :+ (-1)) else do
	h <- openFile hdapsFile ReadMode
	str <- hGetLine h
	let (x,y) = read str :: (Integer,Integer)
	let angle = (500 - fromIntegral x) / 300 * pi - (pi/2)
	let strength = (150 - abs (500-fromIntegral y)) / 150
	return $ cis angle * (strength :+ 0)
