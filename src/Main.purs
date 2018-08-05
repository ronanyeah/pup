module Main where

import Data.Either (note, either)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Node.Encoding (Encoding(..))
import Node.Process (exit, lookupEnv)
import Node.HTTP (Request, Response, setHeader, createServer, listen, responseAsStream, setStatusCode)
import Node.Stream (end, writeString)
import Prelude (Unit, bind, discard, pure, unit, (<>), (>>=), show, (>>>), (>=>))
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main =
  lookupEnv "PORT"
  >>=
  (note "Missing PORT"
    >=> (fromString >>> note "Invalid PORT")
  )
  >>> either logError start

logError :: String -> Effect Unit
logError str = do
  log str
  exit 1

start :: Int -> Effect Unit
start port = do
  server <- createServer app
  listen server
    { hostname: "", port: port, backlog: Nothing }
    (log ("server listening on port " <> (show port)))

app :: Request -> Response -> Effect Unit
app req res = do
  let outputStream = responseAsStream res
  setStatusCode res 200
  setHeader res "Content-Type" "application/json"
  _ <- writeString outputStream UTF8 "{\"ronan\":\"yeah\"}" (pure unit)
  end outputStream (pure unit)
