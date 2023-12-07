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

import Control.Lens ((.~), (^.))
import Control.Monad (foldM)
import Data.Aeson qualified as Aeson
import Data.Aeson.Decoding.Tokens (Lit (..), TkRecord (..), Tokens (..))
import Data.Aeson.Key qualified as Key
import Data.Function ((&))
import Data.Functor.Barbie.Extended (TraversableB, bfor, bmap)
import Data.Functor.Barbie.Record (RecordB (..), ibfor)
import Data.Functor.Identity (Identity (..))
import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Some (Some (..), withSome)
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

-- decodeEnum ::
--   (Enum a, Bounded a) =>
--   (a -> Text) ->
--   Parser Tokens k e a
-- decodeEnum f =
--   let table = Map.fromList [(f e, e) | e <- [minBound .. maxBound]]
--    in withText $ \t ->
--         maybe (Left $ UnrecognisedEnumValue t) Right $ Map.lookup t table

recordB ::
  forall b k e.
  (RecordB b) =>
  b (FieldParser k e) ->
  Parser Tokens k e (b Identity)
recordB parsersB =
  let fieldForKey :: Map Text (Some (Field b))
      fieldForKey =
        Map.fromList
          [ ( withSome someF $ \f -> parsersB ^. fieldLens f . #fieldName,
              someF
            )
            | someF <- allFields @b
          ]

      go ::
        b (FieldParser k e :+: Either (Error e)) ->
        Parser TkRecord k e (b Identity)
      go b = Parser $ \case
        TkPair key tokens -> case Map.lookup (Key.toText key) fieldForKey of
          Nothing -> Left $ UnexpectedKey key
          Just (Some f) -> case b ^. fieldLens f of
            L1 FieldParser {parseField} -> case runParser parseField tokens of
              Left e -> Left e
              Right (tokens', a) ->
                let b' = b & fieldLens f .~ R1 (Right a)
                 in runParser (go b') tokens'
            R1 _ -> Left $ DuplicateKey key
        TkRecordEnd tokens -> fmap (tokens,) $ bfor b $ \case
          L1 FieldParser {defaultValue} -> Identity <$> defaultValue
          R1 a -> Identity <$> a
        TkRecordErr e -> Left $ TokenError e
   in withRecord . go $ bmap L1 parsersB

--  where
--    fieldForKey =

-- While processing the token stream, we annotate our RecordB with
-- the functor sum of the unused field parser and the parsed
-- result. As each field is parsed, we replace the parser with its
-- parse result.
--    go ::
--      b (FieldParser k e :+: Either (Error e)) ->
--      Parser TkRecord k e (b Identity)
--    go b = Parser $ \case
--      TkPair key tokens -> _ $ bfor b $ \case
--        L1 parser@FieldParser{..}
--          | fieldName == Key.toText key -> _
--          | otherwise -> _

--  where
--    -- While processing the token stream, we annotate our RecordB with
--    -- the functor sum of the unused field parser and the parsed
--    -- result. As each field is parsed, we replace the parser with its
--    -- parse result.
--    go ::
--      b (FieldParser k e :+: Either (Error e)) ->
--      Parser TkRecord k e (b Identity)
--    go b = Parser $ \case
--      TkPair key tokens -> case parseFieldName @b $ Key.toText key of
--        Nothing -> Left $ UnexpectedKey key
--        Just (Some f) -> case b ^. fieldLens f of
--          -- Haven't seen this field before, parse it
--          L1 parser -> case runParser (parseField parser) tokens of
--            Left e -> Left e
--            Right (tokens', a) ->
--              let b' = b & fieldLens f .~ R1 (Right a)
--               in runParser (go b') tokens'
--          -- Have seen this field before, replace with "duplicate field" error
--          R1 _ -> Left $ DuplicateKey key
--      -- Walk the record and check we have all our keys
--      TkRecordEnd tokens -> fmap (tokens,) $ ibfor b $ \f -> \case
--        L1 _ -> MissingKey . Key.fromText $ fieldName f
--        R1 a -> Identity <$> a
--      TkRecordErr e -> Left $ TokenError e

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
