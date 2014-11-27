{-# LANGUAGE OverloadedStrings, FlexibleContexts, NoMonomorphismRestriction #-}
import Data.ByteString.Lazy (ByteString)

import qualified Data.ByteString.Lazy.Char8 as C
import Text.Parsec
import Text.Parsec.ByteString.Lazy

import Network.Wreq
import Control.Lens

import System.IO (hFlush,stdout)

data Command = Body | Status
               deriving (Eq, Show)

execute response Body   = response ^. responseBody
execute response Status = C.pack $ show $ (response ^. responseStatus . statusCode)


parseCommand s = case (parse command "<error>" s) of
                   Left _        -> Nothing
                   Right command -> Just command


command = try body <|> try status

status  = do _ <- string "status"
             return Status

body    = do _ <- string "body"
             return Body

prompt str = do putStr str
                hFlush stdout

main = do
  prompt "slash> "
  uri <- getLine
  response <- get uri
  prompt "slash> "
  command <- getLine
  Prelude.putStrLn $  maybe "error" show (fmap (execute response) (parseCommand command))
  main
