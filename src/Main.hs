module Main where
import qualified System.Directory as SysDir
import qualified System.Environment as SysEnv
import qualified System.Info as SysInf
import qualified Data.List as List

renameWithSeqNum :: Int -> String -> [String] -> [(String, String)]
renameWithSeqNum _ _ [] = []
renameWithSeqNum i delimeter xs =
  map
  (\x -> let src = snd x
         in (src, fst x ++ delimeter ++ src))
  $ zip (map show [i..length xs]) xs

getOsPathDelimeter :: String
getOsPathDelimeter =
  if List.isInfixOf "mingw" $ SysInf.os then "\\" else "/"

combineFilePath :: String -> String -> String
combineFilePath currentDir fileName = currentDir ++ getOsPathDelimeter ++ fileName

main :: IO ()
main = do
  currentDir <- SysDir.getCurrentDirectory
  args <- SysEnv.getArgs
  sequence_ $ fmap (\x -> SysDir.renameFile
                              (combineFilePath currentDir (fst x))
                              (combineFilePath currentDir (snd x)))
                   $ renameWithSeqNum 1 "." args
  putStrLn "done !"
  




