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

getOsPathDelimeter :: String
getOsPathDelimeter =
  if List.isInfixOf "mingw" $ SysInf.os then "\\" else "/"

combineFilePath :: String -> String -> String
combineFilePath  = (++) . (++ getOsPathDelimeter)

main :: IO ()
main = do
  currentDir <- SysDir.getCurrentDirectory
  args <- SysEnv.getArgs
  sequence_ $ fmap (\x ->
                      let combineWithCurrDir = combineFilePath currentDir
                      in SysDir.renameFile (combineWithCurrDir $ fst x)
                                           $ combineWithCurrDir $ snd x)
                   $ mapToSeqNum 1 "." args
  putStrLn "done !"
  




