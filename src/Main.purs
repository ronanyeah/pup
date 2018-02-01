module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Node.Encoding (Encoding(..))
import Node.Process (PROCESS, exit, lookupEnv)
import Node.HTTP (HTTP, Request, Response, setHeader, createServer, listen, responseAsStream, setStatusCode)
import Node.Stream (end, writeString)
import Prelude (Unit, bind, discard, pure, unit, (<>), (>>=), ($))

app :: forall eff. Request -> Response -> Eff (http :: HTTP, console :: CONSOLE | eff) Unit
app req res = do
  let outputStream = responseAsStream res
  setStatusCode res 200
  setHeader res "Content-Type" "application/json"
  _ <- writeString outputStream UTF8 "{\"ronan\":\"yeah\"}" (pure unit)
  end outputStream (pure unit)
  log "server responded!"

main :: forall e. Eff ( process :: PROCESS, http :: HTTP , console :: CONSOLE | e ) Unit
main = lookupEnv "PORT" >>= case _ of
  Nothing -> do
    log $ "Unable to find PORT"
    exit 1
  Just portStr -> do
    case fromString portStr of
      Nothing -> do
        log $ "Bad PORT"
        exit 1
      Just port -> do
        let onReady = log ("server listening on port " <> portStr)
        server <- createServer app
        listen server { hostname: "", port: port, backlog: Nothing } onReady
