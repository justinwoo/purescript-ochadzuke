module Ochadzuke where

import Prelude

import Chirashi as C
import Data.Either (Either(..))
import Data.Variant as V
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Exception as EE
import Foreign as F
import Milkis as M
import Milkis.Impl as FetchImpl
import Prim.Row as Row
import Simple.JSON as JSON
import Type.Prelude (SProxy(..))

otherS = SProxy :: SProxy "other"
readForeignS = SProxy :: SProxy "readForeign"

type Error = V.Variant (ErrorRow ())

type ErrorRow r =
  ( other :: EE.Error
  , readForeign :: F.MultipleErrors
  | r
  )

type Fetch'
   = forall options trash a
   . Row.Union options trash M.Options
  => JSON.ReadForeign a
  => M.URL
  -> { method :: M.Method | options }
  -> Aff a

defaultFetchOptions' ::
  { method :: M.Method
  , headers :: M.Headers
  }
defaultFetchOptions' =
  { method: M.getMethod
  , headers: defaultFetchHeaders'
  }

defaultFetchHeaders' :: M.Headers
defaultFetchHeaders' = M.makeHeaders
  { "Content-Type": "application/json"
  , "Accept": "application/json"
  }

readResponse :: forall a. JSON.ReadForeign a => M.Response -> Aff a
readResponse response = do
  f :: F.Foreign <- rethrowAsOther $ M.json response
  throwJsonE $ JSON.read f

throwJsonE :: forall a. JSON.E a -> Aff a
throwJsonE x = case x of
  Left e -> do
    let
      inner = V.inj readForeignS e
      err = C.mkVariantError inner
    Aff.throwError err
  Right value -> do
    pure value

rethrowAsOther :: forall a. Aff a -> Aff a
rethrowAsOther aff = do
  Aff.catchError aff \e -> do
    let
      inner = V.inj otherS e
      err = C.mkVariantError inner
    Aff.throwError err

fetch' :: FetchImpl.FetchImpl -> Fetch'
fetch' impl url opts = do
  resp <- rethrowAsOther $ M.fetch impl url opts
  readResponse resp
