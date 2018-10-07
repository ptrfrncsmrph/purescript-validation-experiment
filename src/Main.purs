module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Semigroup as Semigroup
import Semiring as Semiring

main :: Effect Unit
main = do
  log "Semigroup Validation:"
  Semigroup.main
  log "\n"
  log "Semiring Validation:"
  Semiring.main