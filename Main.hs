{-# LANGUAGE OverloadedStrings #-}
import Network.Wreq
import Control.Lens
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C

fetch :: String -> Response ByteString -> ByteString
fetch "body"   response = response ^. responseBody
fetch "status" response = C.pack $ show $ (response ^. responseStatus . statusCode)
fetch _ response        = "<error>"

main = do
  putStrLn "uri: "
  uri <- getLine
  response <- get uri
  putStrLn "body or status?: "
  command <- getLine
  putStrLn $ show $ fetch command response
  main
