{-# LANGUAGE OverloadedStrings, FlexibleContexts, NoMonomorphismRestriction #-}
import qualified Data.Traversable as T
import Control.Lens

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Parsec
import Text.Parsec.ByteString.Lazy

import Network.Wreq

import System.IO (hFlush,stdout)

data Queries = Body | Status deriving (Eq, Show)
data Orders  = Get deriving (Eq, Show)
data Setters = SetUri [Char] deriving (Eq, Show)

data Command = Query Queries | Order Orders | Setter Setters 
               deriving (Eq, Show)

type Uri = String
data Request = Request Uri Options
data Env = Env Request (Maybe (Response ByteString))

query :: Queries -> Response ByteString -> ByteString
query Body   response = response ^. responseBody 
query Status response = C.pack $ show $ (response ^. responseStatus . statusCode)

order :: Orders -> Request -> IO (Response ByteString)
order Get request = do let Request uri _ = request
                       newResponse <- get uri
                       return newResponse

setter :: Setters -> Request -> Request
setter (SetUri newUri) (Request _ options) = Request newUri options

-- TODO: refactor to use case 
execute :: Command -> Env -> IO (Env, ByteString)
execute (Query q) env@(Env request maybeResponse) = case maybeResponse of
                                                    Nothing -> return (env, "no request made")
                                                    Just response -> return (env, query q response)
execute (Order o) env@(Env request response) = order o request >>= (\newResponse -> return (Env request (Just newResponse), "ok"))
execute (Main.Setter s) env@(Env request response) = return (Env (setter s request) response, "ok")


parseCommand s = case (parse command "<error>" s) of
                   Left _        -> Nothing
                   Right command -> Just command


whitespace  = many (char ' ')

command     = try setUri <|> try body <|> try status <|> try getOrder

setUri :: Stream s m Char => ParsecT s u m Command
setUri      = do string "uri"
                 whitespace
                 char '='
                 whitespace
                 u <- uri
                 return (Main.Setter (SetUri u))

uri         = many (Text.Parsec.noneOf "\n\r")
status      = string "status" >> return (Query Status)
body        = string "body" >> return (Query Body)
getOrder    = string "get" >> return (Order Get)

prompt str = do putStr str
                hFlush stdout

printResult (Just result) = Prelude.putStrLn (show result)
printResult Nothing       = return ()

mainLoop env = do prompt "slash> "
                  commandInput <- getLine
                  let command  =  parseCommand commandInput
                  result <-  T.sequence $ fmap ((flip execute) env) command
                  printResult $ fmap snd result
                  mainLoop $ maybe env fst result
                  

main = mainLoop (Env (Request "http://httpbin.org/get" defaults) Nothing)
