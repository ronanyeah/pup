module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Node.Encoding (Encoding(..))
import Node.HTTP (HTTP, ListenOptions, Request, Response, createServer, listen, responseAsStream, setStatusCode)
import Node.Stream (end, writeString)
import Prelude (Unit, bind, discard, pure, show, unit, (<>))

respond :: forall eff. Response -> Int -> String -> Eff (http :: HTTP | eff) Unit
respond res status str =
  do
    let outputStream = responseAsStream res
    setStatusCode res status
    _ <- writeString outputStream UTF8 str (pure unit)
    end outputStream (pure unit)

app :: forall eff. Request -> Response -> Eff (http :: HTTP, console :: CONSOLE | eff) Unit
app req res = do
  respond res 200 "hi there"
  log "responded"

listenOptions :: ListenOptions
listenOptions = { hostname: "127.0.0.1", port: 8000, backlog: Nothing }

main :: forall e. Eff ( http :: HTTP , console :: CONSOLE | e ) Unit
main = do
  let onReady = log ("server ready at http://" <> listenOptions.hostname <> ":" <> (show listenOptions.port))
  server <- createServer app
  listen server listenOptions onReady
