module Main where
import qualified System.Directory as SysDir
import qualified System.Environment as SysEnv
import qualified System.Info as SysInf
import qualified Data.List as List

mapToSeqNum :: Int -> String -> [String] -> [(String, String)]
mapToSeqNum _ _ [] = []
mapToSeqNum i delimeter xs =
  map
  (\x -> let src = snd x
         in (src, fst x ++ delimeter ++ src))
  $ zip (map show [i..length xs]) xs

renameToSeqNum :: String -> String -> [String] -> IO ()
renameToSeqNum currentDir delimeter fileNames =
  sequence_ $ fmap (\x ->
                    let combineWithCurrDir = combineFilePath currentDir
                    in SysDir.renameFile (combineWithCurrDir $ fst x)
                                           $ combineWithCurrDir $ snd x)
                   $ mapToSeqNum 1 delimeter fileNames

getOsPathDelimeter :: String
getOsPathDelimeter =
  if List.isInfixOf "mingw" $ SysInf.os then "\\" else "/"

combineFilePath :: String -> String -> String
combineFilePath  = (++) . (++ getOsPathDelimeter)

splitArgAt :: String -> [String] -> ([String], [String])
splitArgAt opt args =
  let delemParamIdx = List.elemIndex opt args
  in case delemParamIdx of
     Just x -> List.splitAt x args
     Nothing -> ([], [])

data SeqArg = SeqArg {delimeter::String, fileNames::[String]} deriving(Show)

extractSeqArg :: String -> [String] -> Maybe SeqArg
extractSeqArg x args
    | x == "-d" =
        constructSeqArg "-f" fst snd
    | x == "-f" =
        constructSeqArg "-d" snd fst
    | otherwise = Nothing 
    where getElemSnd = head . tail
          constructSeqArg opt delimeterExct fileNamesExct =
              let tuppleArg = splitArgAt opt args
              in Just SeqArg{delimeter=getElemSnd $ delimeterExct tuppleArg,
                             fileNames=tail $ fileNamesExct tuppleArg}

    
main :: IO ()
main = do
  currentDir <- SysDir.getCurrentDirectory
  args <- SysEnv.getArgs

  let seqArg = extractSeqArg (head args) args 
  case seqArg of
    Just x -> do
      renameToSeqNum currentDir (delimeter x) $ fileNames x
      putStrLn "done !"
    Nothing -> putStrLn "nothing to be done"
 
