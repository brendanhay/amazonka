{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Data.Json.InsertOrdered
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Data.Json.InsertOrdered (InsOrdJson (..), parse) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Decoding.Tokens
  ( Lit (..),
    Number (..),
    TkArray (..),
    TkRecord (..),
    Tokens (..),
  )
import Data.Bifunctor (first)
import Data.Scientific (Scientific)
import Data.Sequence (Seq (..))
import Data.Text (Text)
import GHC.Generics (Generic)

data InsOrdJson
  = Object (Seq (Aeson.Key, InsOrdJson))
  | Array (Seq InsOrdJson)
  | String Text
  | Number Scientific
  | Bool Bool
  | Null
  deriving (Eq, Show, Generic)

parse :: Tokens k e -> Either e InsOrdJson
parse = fmap fst . parseTokens

parseTokens :: Tokens k e -> Either e (InsOrdJson, k)
parseTokens = \case
  TkLit lit k -> case lit of
    LitNull -> Right (Null, k)
    LitTrue -> Right (Bool True, k)
    LitFalse -> Right (Bool False, k)
  TkText text k -> Right (String text, k)
  TkNumber num k -> case num of
    NumInteger i -> Right (Number $ fromIntegral i, k)
    NumDecimal d -> Right (Number d, k)
    NumScientific s -> Right (Number s, k)
  TkArrayOpen a -> first Array <$> parseArray a
  TkRecordOpen r -> first Object <$> parseRecord r
  TkErr e -> Left e

parseArray :: TkArray k e -> Either e (Seq InsOrdJson, k)
parseArray = go mempty
  where
    go :: Seq InsOrdJson -> TkArray k e -> Either e (Seq InsOrdJson, k)
    go acc = \case
      TkItem tokens -> do
        (item, rest) <- parseTokens tokens
        go (acc :|> item) rest
      TkArrayEnd k -> Right (acc, k)
      TkArrayErr e -> Left e

parseRecord :: TkRecord k e -> Either e (Seq (Aeson.Key, InsOrdJson), k)
parseRecord = go mempty
  where
    go ::
      Seq (Aeson.Key, InsOrdJson) ->
      TkRecord k e ->
      Either e (Seq (Aeson.Key, InsOrdJson), k)
    go acc = \case
      TkPair key tokens -> do
        (value, rest) <- parseTokens tokens
        go (acc :|> (key, value)) rest
      TkRecordEnd k -> Right (acc, k)
      TkRecordErr e -> Left e
