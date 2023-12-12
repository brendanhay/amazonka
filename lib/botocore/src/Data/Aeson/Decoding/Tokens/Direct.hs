{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Data.Aeson.Decoding.Tokens.Direct
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Data.Aeson.Decoding.Tokens.Direct where

import Barbies.Bare (Bare, BareB, Covered, bstrip)
import Barbies.TH (AccessorsB (..), FieldNamesB, LensB (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Decoding.Tokens (Lit (..), TkRecord (..), Tokens (..))
import Data.Aeson.Key qualified as Key
import Data.Function ((&))
import Data.Functor.Barbie.Extended
  ( ApplicativeB (..),
    TraversableB,
    bfoldMap,
    bfor,
    bmap,
  )
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product (Product (..))
import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Map qualified as Map
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
  | UnrecognisedEnumValue Text
  | TokenError e
  deriving (Eq, Show, Generic)

newtype Parser tokenType k e a = Parser
  {runParser :: tokenType k e -> (Either (Error e) (k, a))}
  deriving (Functor)

execParser :: Parser tokenType k e a -> tokenType k e -> Either (Error e) a
execParser parser = fmap snd . runParser parser

andThen ::
  Parser tokenType k e a -> (a -> Either (Error e) b) -> Parser tokenType k e b
andThen (Parser parse) f = Parser $ \tokens -> do
  (tokens', a) <- parse tokens
  (tokens',) <$> f a

enum ::
  (Enum a, Bounded a, Ord a) =>
  (a -> Text) ->
  Parser Tokens k e a
enum name =
  let table = Map.fromList [(name e, e) | e <- [minBound .. maxBound]]
   in text `andThen` \t ->
        maybe (Left $ UnrecognisedEnumValue t) Right $ Map.lookup t table

data FieldParser k e a = FieldParser
  { fieldName :: Text,
    parseField :: Parser Tokens (TkRecord k e) e a,
    defaultValue :: Either (Error e) a
  }
  deriving (Generic)

field ::
  Text ->
  Parser Tokens (TkRecord k e) e a ->
  FieldParser k e a
field fieldName parseField =
  FieldParser
    { defaultValue = Left . MissingKey $ Key.fromText fieldName,
      ..
    }

optional :: FieldParser k e a -> FieldParser k e (Maybe a)
optional FieldParser {..} =
  FieldParser
    { parseField = Just <$> parseField,
      defaultValue = Right Nothing,
      ..
    }

record ::
  forall b k e.
  ( BareB b,
    AccessorsB (b Covered),
    ApplicativeB (b Covered),
    FieldNamesB (b Covered),
    TraversableB (b Covered)
  ) =>
  b Covered (FieldParser k e) ->
  Parser Tokens k e (b Bare Identity)
record bparsers =
  let fieldForKey :: Map Text (Some (LensB (b Covered)))
      fieldForKey =
        bfoldMap
          ( \(Pair parser blens) ->
              Map.singleton (fieldName parser) (Some blens)
          )
          (bprod bparsers baccessors)

      -- While processing the token stream, we annotate the record with
      -- the functor sum of the unused field parser and the parsed
      -- result. As each field is parsed, we replace the parser with its
      -- parse result.
      go ::
        b Covered (FieldParser k e :+: Either (Error e)) ->
        Parser TkRecord k e (b Covered Identity)
      go b = Parser $ \case
        TkPair key tokens -> case Map.lookup (Key.toText key) fieldForKey of
          Nothing -> Left $ UnexpectedKey key
          Just (Some f) -> case viewB f b of
            L1 FieldParser {parseField} -> case runParser parseField tokens of
              Left e -> Left e
              Right (tokens', a) ->
                let b' = b & setB f (R1 (Right a))
                 in runParser (go b') tokens'
            R1 _ -> Left $ DuplicateKey key
        TkRecordEnd tokens -> fmap (tokens,) $ bfor b $ \case
          L1 FieldParser {defaultValue} -> Identity <$> defaultValue
          R1 a -> Identity <$> a
        TkRecordErr e -> Left $ TokenError e
   in fmap bstrip . withRecord . go $ bmap L1 bparsers

text :: Parser Tokens k e Text
text = Parser $ \case
  TkLit LitNull _ -> Left (ExpectedGot String Null)
  TkLit LitTrue _ -> Left (ExpectedGot String Bool)
  TkLit LitFalse _ -> Left (ExpectedGot String Bool)
  TkText t k -> Right (k, t)
  TkNumber _ _ -> Left (ExpectedGot String Number)
  TkArrayOpen _ -> Left (ExpectedGot String Array)
  TkRecordOpen _ -> Left (ExpectedGot String Object)
  TkErr e -> Left (TokenError e)

--
-- withMap ::
--   forall key value k e.
--   (Ord key) =>
--   (Aeson.Key -> Tokens (TkRecord k e) e -> Either (Error e) (key, value)) ->
--   Tokens k e ->
--   Either (Error e) (k, Map key value)
-- withMap f = withRecord $ go mempty
--   where
--     go :: Map key value -> TkRecord k e -> Either (Error e) (Map key value)
--     go m = \case
--       TkRecordEnd k -> _
--       TkRecordErr e -> Left $ TokenError e
--
withRecord ::
  Parser TkRecord k e a -> Parser Tokens k e a
withRecord parser = Parser $ \case
  TkLit LitNull _ -> Left (ExpectedGot Object Null)
  TkLit LitTrue _ -> Left (ExpectedGot Object Bool)
  TkLit LitFalse _ -> Left (ExpectedGot Object Bool)
  TkText _ _ -> Left (ExpectedGot Object String)
  TkNumber _ _ -> Left (ExpectedGot Object Number)
  TkArrayOpen _ -> Left (ExpectedGot Object Array)
  TkRecordOpen tokens -> runParser parser tokens
  TkErr e -> Left (TokenError e)
