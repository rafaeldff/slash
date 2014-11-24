{-# LANGUAGE OverloadedStrings #-}
import Network.Wreq
import Control.Lens

main = do
  putStr "uri: "
  uri <- getLine
  response <- get uri
  putStrLn $ show (response ^. responseBody)
  main
