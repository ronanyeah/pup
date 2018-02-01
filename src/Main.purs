module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (note, either)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Node.Encoding (Encoding(..))
import Node.Process (PROCESS, exit, lookupEnv)
import Node.HTTP (HTTP, Request, Response, setHeader, createServer, listen, responseAsStream, setStatusCode)
import Node.Stream (end, writeString)
import Prelude (Unit, bind, discard, pure, unit, (<>), (>>=), ($), show, (>>>), (>=>))

app :: forall eff. Request -> Response -> Eff (http :: HTTP | eff) Unit
app req res = do
  let outputStream = responseAsStream res
  setStatusCode res 200
  setHeader res "Content-Type" "application/json"
  _ <- writeString outputStream UTF8 "{\"ronan\":\"yeah\"}" (pure unit)
  end outputStream (pure unit)

main :: forall eff. Eff ( process :: PROCESS, http :: HTTP, console :: CONSOLE | eff ) Unit
main = lookupEnv "PORT" >>= (note "Missing PORT" >=> (fromString >>> note "Invalid PORT")) >>> either logError start

logError :: forall eff. String -> Eff ( process :: PROCESS, console :: CONSOLE | eff ) Unit
logError str = do
  log $ str
  exit 1

start :: forall eff. Int -> Eff ( http :: HTTP, console :: CONSOLE | eff ) Unit
start port = do
  let onReady = log ("server listening on port " <> (show port))
  server <- createServer app
  listen server { hostname: "", port: port, backlog: Nothing } onReady
