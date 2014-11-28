{-# LANGUAGE OverloadedStrings, FlexibleContexts, NoMonomorphismRestriction #-}
import qualified Data.Traversable as T
import Control.Lens

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Parsec
import Text.Parsec.ByteString.Lazy

import Network.Wreq

import System.IO (hFlush,stdout)

data Command = Body | Status | Get
               deriving (Eq, Show)

type Uri = String
data Request = Request Uri Options
data Env = Env Request (Maybe (Response ByteString))

-- TODO: refactor to use case 
execute :: Env -> Command -> IO (Env, ByteString)
execute (Env request (Just response)) Body   = return (Env request (Just response), response ^. responseBody)
execute (Env request (Just response)) Status = return (Env request (Just response), C.pack $ show $ (response ^. responseStatus . statusCode))

execute (Env request _              ) Get = do let Request uri _ = request
                                               newResponse <- get uri
                                               return (Env request (Just newResponse), "ok")

execute env@(Env _ Nothing) _  = return (env, "unknown")


parseCommand s = case (parse command "<error>" s) of
                   Left _        -> Nothing
                   Right command -> Just command


command = try body <|> try status <|> try getCommand

status  = do _ <- string "status"
             return Status

body    = do _ <- string "body"
             return Body

getCommand  = do _ <- string "get"
                 return Get

prompt str = do putStr str
                hFlush stdout

printResult (Just result) = Prelude.putStrLn (show result)
printResult Nothing       = return ()

mainLoop env = do prompt "slash> "
                  commandInput <- getLine
                  let command  =  parseCommand commandInput
                  result <-  T.sequence $ fmap (execute env) command
                  printResult $ fmap snd result
                  mainLoop $ maybe env fst result
                  

main = mainLoop (Env (Request "http://httpbin.org/get" defaults) Nothing)
