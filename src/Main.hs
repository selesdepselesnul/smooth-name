module Main where

renameWithSeqNum :: Int -> [String] -> [(String, String)]
renameWithSeqNum _ [] = []
renameWithSeqNum i xs =
  map
  (\x -> (snd x, fst x ++ snd x))
  $ zip (map show [i..length xs]) xs

main :: IO ()
main = do
  putStrLn "init"
