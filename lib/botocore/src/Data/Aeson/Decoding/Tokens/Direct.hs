{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Data.Aeson.Decoding.Tokens.Direct
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Data.Aeson.Decoding.Tokens.Direct where

import Control.Lens ((.~), (^.))
import Data.Aeson qualified as Aeson
import Data.Aeson.Decoding.Tokens (Lit (..), TkRecord (..), Tokens (..))
import Data.Aeson.Key qualified as Key
import Data.Function ((&))
import Data.Functor.Barbie.Extended (bmap)
import Data.Functor.Barbie.Record (RecordB (..), ibfor)
import Data.Functor.Identity (Identity (..))
import Data.Some (Some (..))
import Data.Text (Text)
import GHC.Generics (Generic, (:+:) (..))

data JsonType = Null | Bool | String | Number | Array | Object
  deriving (Eq, Enum, Show, Generic)

data Error e
  = ExpectedGot JsonType JsonType
  | MissingKey Aeson.Key
  | DuplicateKey Aeson.Key
  | UnexpectedKey Aeson.Key
  | TokenError e
  deriving (Eq, Show, Generic)

newtype FieldParser k e a
  = FieldParser (Tokens (TkRecord k e) e -> Either (Error e) (TkRecord k e, a))

decodeRecordB :: forall b k e. (RecordB b) => b (FieldParser k e) -> Tokens k e -> Either (Error e) (k, b Identity)
decodeRecordB = withRecord . go . bmap L1
  where
    -- While processing the token stream, we annotate our RecordB with
    -- the functor sum of (FieldParser k e) (that we haven't used yet)
    -- and (Either (Error e)). As each field is parsed, it switches
    -- from holding a FieldParser to holding its parse result.
    go :: b (FieldParser k e :+: (Either (Error e))) -> TkRecord k e -> Either (Error e) (k, b Identity)
    go b = \case
      TkPair key tokens -> case parseField @b $ Key.toText key of
        Nothing -> Left $ UnexpectedKey key
        Just (Some field) -> case b ^. fieldLens field of
          -- Haven't seen this field before, parse it
          L1 (FieldParser f) -> do
            (tokens', a) <- f tokens
            let b' = b & fieldLens field .~ R1 (Right a)
            go b' tokens'
          -- Have seen this field before, replace with "duplicate field" error
          R1 _ -> Left $ DuplicateKey key
      -- Walk the record and check we have all our keys
      TkRecordEnd k -> fmap (k,) $ ibfor b $ \field -> \case
        L1 _ -> Left . MissingKey . Key.fromText $ fieldName field
        R1 a -> Identity <$> a
      TkRecordErr e -> Left $ TokenError e

withText :: (Text -> Either (Error e) a) -> Tokens k e -> Either (Error e) (k, a)
withText f = \case
  TkLit LitNull _ -> Left (ExpectedGot String Null)
  TkLit LitTrue _ -> Left (ExpectedGot String Bool)
  TkLit LitFalse _ -> Left (ExpectedGot String Bool)
  TkText t k -> (k,) <$> f t
  TkNumber _ _ -> Left (ExpectedGot String Number)
  TkArrayOpen _ -> Left (ExpectedGot String Array)
  TkRecordOpen _ -> Left (ExpectedGot String Object)
  TkErr e -> Left (TokenError e)

withRecord ::
  (TkRecord k e -> Either (Error e) a) -> Tokens k e -> Either (Error e) a
withRecord f = \case
  TkLit LitNull _ -> Left (ExpectedGot Object Null)
  TkLit LitTrue _ -> Left (ExpectedGot Object Bool)
  TkLit LitFalse _ -> Left (ExpectedGot Object Bool)
  TkText _ _ -> Left (ExpectedGot Object String)
  TkNumber _ _ -> Left (ExpectedGot Object Number)
  TkArrayOpen _ -> Left (ExpectedGot Object Array)
  TkRecordOpen tokens -> f tokens
  TkErr e -> Left (TokenError e)
