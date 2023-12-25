{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Aeson.Decoding.Tokens.Direct
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Data.Aeson.Decoding.Tokens.Direct where

import Barbies.Bare (Bare, BareB, Covered, bstrip)
import Barbies.TH (AccessorsB (..), FieldNamesB, LensB (..), passthroughBareB)
import Data.Aeson qualified as Aeson
import Data.Aeson.Decoding.Tokens
  ( Lit (..),
    Number (..),
    TkArray (..),
    TkRecord (..),
    Tokens (..),
  )
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
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Scientific (Scientific)
import Data.Some (Some (..))
import Data.Text (Text)
import GHC.Generics (Generic, (:+:) (..))

-- Helper type for parsing single-field objects. Up this high for TH reasons.
$(passthroughBareB [d|data OneFieldObject a = OneFieldObject {theField :: a}|])

data JsonType = Null | Bool | String | Number | Array | Object
  deriving (Eq, Enum, Show, Generic)

data Error e
  = EmptyList
  | ExpectedEmptyObject
  | ExpectedGot JsonType JsonType
  | MissingKey Aeson.Key
  | DuplicateKey Aeson.Key
  | NonIntegerNumber Number
  | UnexpectedKey Aeson.Key
  | UnrecognisedEnumValue Text
  | TokenError e
  deriving (Eq, Show, Generic)

newtype Parser tokenType k e a = Parser
  {runParser :: tokenType k e -> Either (Error e) (k, a)}
  deriving (Functor)

execParser :: Parser tokenType k e a -> tokenType k e -> Either (Error e) a
execParser parser = fmap snd . runParser parser

andThen ::
  Parser tokenType k e a -> (a -> Either (Error e) b) -> Parser tokenType k e b
andThen (Parser parse) f = Parser $ \tokens -> do
  (tokens', a) <- parse tokens
  (tokens',) <$> f a

alist ::
  forall k e a.
  Parser Tokens (TkRecord k e) e a ->
  Parser Tokens k e [(Text, a)]
alist parseValue = withRecord $ go id
  where
    go ::
      -- Difference list
      ([(Text, a)] -> [(Text, a)]) ->
      Parser TkRecord k e [(Text, a)]
    go acc = Parser $ \case
      TkPair key tokens -> case runParser parseValue tokens of
        Left e -> Left e
        Right (tokens', a) ->
          let acc' = acc . (++ [(Key.toText key, a)])
           in runParser (go acc') tokens'
      TkRecordEnd tokens -> Right (tokens, acc [])
      TkRecordErr e -> Left (TokenError e)

bool :: Parser Tokens k e Bool
bool = Parser $ \case
  TkLit LitNull _ -> Left $ ExpectedGot Bool Null
  TkLit LitTrue k -> Right (k, True)
  TkLit LitFalse k -> Right (k, False)
  TkText _ _ -> Left $ ExpectedGot Bool String
  TkNumber _ _ -> Left $ ExpectedGot Bool Number
  TkArrayOpen _ -> Left $ ExpectedGot Bool Array
  TkRecordOpen _ -> Left $ ExpectedGot Bool Object
  TkErr e -> Left $ TokenError e

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
  deriving (Functor, Generic)

emptyObject :: Parser Tokens k e ()
emptyObject = withRecord . Parser $ \case
  TkRecordEnd tokens -> Right (tokens, ())
  TkPair {} -> Left ExpectedEmptyObject
  TkRecordErr e -> Left $ TokenError e

field ::
  Text ->
  Parser Tokens (TkRecord k e) e a ->
  FieldParser k e a
field fieldName parseField =
  FieldParser
    { defaultValue = Left . MissingKey $ Key.fromText fieldName,
      ..
    }

int :: (Num a) => Parser Tokens k e a
int =
  number `andThen` \case
    NumInteger i -> Right $ fromInteger i
    n -> Left $ NonIntegerNumber n

list :: forall k e a. Parser Tokens (TkArray k e) e a -> Parser Tokens k e [a]
list parseValue = withArray $ go id
  where
    go ::
      -- Difference list
      ([a] -> [a]) ->
      Parser TkArray k e [a]
    go acc = Parser $ \case
      TkItem tokens -> case runParser parseValue tokens of
        Left e -> Left e
        Right (tokens', a) ->
          let acc' = acc . (++ [a])
           in runParser (go acc') tokens'
      TkArrayEnd tokens -> Right (tokens, acc [])
      TkArrayErr e -> Left $ TokenError e

map ::
  forall k e a.
  Parser Tokens (TkRecord k e) e a ->
  Parser Tokens k e (Map Text a)
map parseValue = withRecord $ go Map.empty
  where
    go :: Map Text a -> Parser TkRecord k e (Map Text a)
    go acc = Parser $ \case
      TkPair key tokens -> case runParser parseValue tokens of
        Left e -> Left e
        Right (tokens', a) ->
          let acc' = Map.insert (Key.toText key) a acc
           in runParser (go acc') tokens'
      TkRecordEnd tokens -> Right (tokens, acc)
      TkRecordErr e -> Left $ TokenError e

number :: Parser Tokens k e Number
number = Parser $ \case
  TkLit LitNull _ -> Left (ExpectedGot Number Null)
  TkLit LitTrue _ -> Left (ExpectedGot Number Bool)
  TkLit LitFalse _ -> Left (ExpectedGot Number Bool)
  TkText _ _ -> Left (ExpectedGot Number String)
  TkNumber n k -> Right (k, n)
  TkArrayOpen _ -> Left (ExpectedGot Number Array)
  TkRecordOpen _ -> Left (ExpectedGot Number Object)
  TkErr e -> Left (TokenError e)

nonEmpty :: Parser Tokens (TkArray k e) e a -> Parser Tokens k e (NonEmpty a)
nonEmpty parseValue =
  list parseValue `andThen` \xs ->
    maybe (Left EmptyList) Right $ NE.nonEmpty xs

oneFieldObject :: FieldParser k e a -> Parser Tokens k e a
oneFieldObject parseField =
  theField <$> record OneFieldObject {theField = parseField}

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
  let recordLensesByKey :: Map Text (Some (LensB (b Covered)))
      recordLensesByKey =
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
        TkPair key tokens ->
          case Map.lookup (Key.toText key) recordLensesByKey of
            Nothing -> Left $ UnexpectedKey key
            Just (Some f) -> case viewB f b of
              -- Haven't seen this key before: try to parse it
              L1 FieldParser {parseField} -> case runParser parseField tokens of
                Left e -> Left e
                Right (tokens', a) ->
                  let b' = b & setB f (R1 (Right a))
                   in runParser (go b') tokens'
              -- Duplicate key: report error
              R1 _ -> Left $ DuplicateKey key
        TkRecordEnd tokens -> fmap (tokens,) $ bfor b $ \case
          L1 FieldParser {defaultValue} -> Identity <$> defaultValue
          R1 a -> Identity <$> a
        TkRecordErr e -> Left $ TokenError e
   in fmap bstrip . withRecord . go $ bmap L1 bparsers

scientific :: Parser Tokens k e Scientific
scientific =
  number `andThen` \case
    NumInteger i -> Right $ fromInteger i
    NumDecimal d -> Right d
    NumScientific s -> Right s

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

withArray ::
  Parser TkArray k e a -> Parser Tokens k e a
withArray parser = Parser $ \case
  TkLit LitNull _ -> Left (ExpectedGot Array Null)
  TkLit LitTrue _ -> Left (ExpectedGot Array Bool)
  TkLit LitFalse _ -> Left (ExpectedGot Array Bool)
  TkText _ _ -> Left (ExpectedGot Array String)
  TkNumber _ _ -> Left (ExpectedGot Array Number)
  TkArrayOpen tokens -> runParser parser tokens
  TkRecordOpen _ -> Left (ExpectedGot Array Object)
  TkErr e -> Left (TokenError e)

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
