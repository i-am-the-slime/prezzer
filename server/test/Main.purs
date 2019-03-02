module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Main as Main

main :: Effect Unit
main = do
  log "You should add some tests."
