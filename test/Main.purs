module Test.Main where

import Prelude

import Chirashi as C
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant as V
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (class MonadEffect, liftEffect)
import Milkis as M
import Milkis.Impl.Node (nodeFetch)
import Ochadzuke as O
import Test.Assert (assertEqual)

testUrl :: M.URL
testUrl = M.URL "https://jsonplaceholder.typicode.com/todos/1"

fetch' :: O.Fetch'
fetch' = O.fetch' nodeFetch

assertEqual'
  :: forall a m
   . MonadEffect m
  => Eq a
  => Show a
  => { actual :: a
     , expected :: a
     }
  -> m Unit
assertEqual' = liftEffect <<< assertEqual

main :: Effect Unit
main = Aff.launchAff_ do

  -- works normally
  normal <- fetch' testUrl O.defaultFetchOptions'
  assertEqual'
    { expected: { "userId": 1 }
    , actual: normal
    }

  -- gives an error in readForeign on mismatched decoding type
  wrong <- Aff.attempt $ fetch' testUrl O.defaultFetchOptions'
  case wrong of
    Left e
      | Just (variant :: O.Error) <- C.readVariant e
      , Just multipleErrors <- V.prj O.readForeignS variant -> do
      pure unit
    Right (e :: { asdf :: String }) -> Aff.throwError $ Aff.error "False parsing result"
    Left e ->
      Aff.throwError e

  -- gives an error in otherS on random other failure
  wrong2 <- Aff.attempt $ fetch' (M.URL "sdflsjdfasdf") O.defaultFetchOptions'
  case wrong2 of
    Left e
      | Just (variant :: O.Error) <- C.readVariant e
      , Just other <- V.prj O.otherS variant ->
      assertEqual'
        { expected: "TypeError: Only absolute URLs are supported"
        , actual: Aff.message other
        }
    Right (e :: {}) -> Aff.throwError $ Aff.error "False parsing result"
    Left e -> Aff.throwError e
