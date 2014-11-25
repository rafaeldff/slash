{-# LANGUAGE OverloadedStrings #-}
import Network.Wreq
import Control.Lens

main = do
  putStrLn "uri: "
  uri <- getLine
  response <- get uri
  putStrLn "response body: "
  putStrLn $ show (response ^. responseBody)
  main
