import Data.List
import System.IO

readFromFile = do
 theFile <- openFile "Tauler.txt" ReadMode
 contents <- hGetContents theFile
 putStrLn contents
 hClose theFile
 
forEach :: String -> Int
forEach [] = 0
forEach (char:string) = 1 + (forEach string)