# Purescript-Ochadzuke

A library that combines Simple-JSON, [Milkis](https://github.com/justinwoo/purescript-milkis) (Fetch), and [Chirashi](https://github.com/justinwoo/purescript-chirashi) (Error subtypes).

It's ochadzuke made by mixing Chirashi and Milkis.

![](https://i.imgur.com/sUeJgfi.png)

## Usage

```purs
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
```

See the [API Docs](https://pursuit.purescript.org/packages/purescript-ochadzuke/) or the [tests](test/Main.purs) for usage.
