module Main where

renameWithSeqNum :: Int -> String -> [String] -> [(String, String)]
renameWithSeqNum _ _ [] = []
renameWithSeqNum i delimeter xs =
  map
  (\x -> let src = snd x
         in (src, fst x ++ delimeter ++ src))
  $ zip (map show [i..length xs]) xs

main :: IO ()
main = do
  putStrLn "init"
