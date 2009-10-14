module HDAPS where
import Data.Complex
import System.Directory
import System.IO

hdapsFile = "/sys/devices/platform/hdaps/position"

-- | Return unit vector pointing „down“
readTilt :: IO (Complex Double)
readTilt = do
	ex <- doesFileExist hdapsFile
	if not ex then return (0 :+ (-1)) else do
	h <- openFile hdapsFile ReadMode
	str <- hGetLine h
	let (x,_) = read str :: (Integer,Integer)
	let angle = (500 - fromIntegral x) / 300 * pi - (pi/2)
	return (cis angle)

