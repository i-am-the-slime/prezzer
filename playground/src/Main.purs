module Main where

import Prelude
import Effect.Console (logShow)

newtype ProductId = ProductId String
newtype MerchantId = MerchantId String

likesProduct :: ProductId -> MerchantId -> Boolean
likesProduct (ProductId pid) (MerchantId mid) =
  if pid == "good product" && mid == "mango"
    then true
    else false

main = do
  logShow $ likesProduct (ProductId "mango") (MerchantId "mediocre product")
