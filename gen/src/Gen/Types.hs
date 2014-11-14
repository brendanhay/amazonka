{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Gen.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Types where

import           Control.Applicative
import           Control.Lens         (Traversal', makeLenses)
import qualified Data.Aeson           as A
import           Data.Attoparsec.Text (Parser, parseOnly)
import qualified Data.Attoparsec.Text as AText
import           Data.Bifunctor
import           Data.CaseInsensitive (CI)
import           Data.Char
import           Data.Foldable        (Foldable)
import           Data.HashMap.Strict  (HashMap)
import           Data.HashSet         (HashSet)
import           Data.Jason.Types     hiding (Parser)
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Traversable     (Traversable, traverse)
import           Gen.JSON             ()
import           Gen.Names
import           Gen.TH
import           System.FilePath
import           Text.EDE             (Template)

default (Text)

-- NOTE: Keep the boto json structure completely intact.
-- FIXME: _retry.json
-- FIXME: _endpoints.json

class ToFilePath a where
    toFilePath :: a -> FilePath

instance ToFilePath FilePath where
    toFilePath = id

data OrdMap a = OrdMap { ordMap :: [(Text, a)] }
    deriving (Eq, Show, Functor, Foldable, Traversable)

makeLenses ''OrdMap

instance FromJSON a => FromJSON (OrdMap a) where
    parseJSON = withObject "ordered_map" $ \(unObject -> o) ->
        OrdMap <$> traverse (\(k, v) -> (k,) <$> parseJSON v) o

instance ToJSON a => ToJSON (OrdMap a) where
    toJSON = Object . mkObject . map (second toJSON) . ordMap

data Signature
    = V2
    | V3
    | V4
      deriving (Eq, Show)

instance FromJSON Signature where
    parseJSON = withText "signature" $ \case
        "v2"      -> pure V2
        "v3"      -> pure V3
        "v3https" -> pure V3
        "v4"      -> pure V4
        "s3"      -> pure V4
        e         -> fail ("Unknown Signature: " ++ Text.unpack e)

instance A.ToJSON Signature where
    toJSON = A.toJSON . show

data Protocol
    = Json
    | RestJson
    | Xml
    | RestXml
    | Query
    | Ec2
      deriving (Eq, Show)

nullary stage1 ''Protocol
nullary stage2 ''Protocol

data Timestamp
    = RFC822
    | ISO8601
    | POSIX
      deriving (Eq, Ord, Show)

instance FromJSON Timestamp where
    parseJSON = withText "timestamp" $ \case
        "rfc822"        -> pure RFC822
        "iso8601"       -> pure ISO8601
        "unixTimestamp" -> pure POSIX
        e               -> fail ("Unknown Timestamp: " ++ Text.unpack e)

timestamp :: Timestamp -> Text
timestamp = Text.pack . show

defaultTS :: Maybe Timestamp -> Timestamp
defaultTS = fromMaybe RFC822

data Checksum
    = MD5
    | SHA256
      deriving (Eq, Show)

nullary stage1 ''Checksum
nullary stage2 ''Checksum

data Method
    = GET
    | POST
    | HEAD
    | PUT
    | DELETE
      deriving (Eq, Show)

instance FromJSON Method where
    parseJSON = withText "method" $ \case
        "GET"    -> pure GET
        "POST"   -> pure POST
        "HEAD"   -> pure HEAD
        "PUT"    -> pure PUT
        "DELETE" -> pure DELETE
        e        -> fail ("Unknown Method: " ++ Text.unpack e)

nullary stage2 ''Method

data Location
    = Headers
    | Header
    | Uri
    | Querystring
    | StatusCode
    | BodyXml
    | BodyJson
    | Body
      deriving (Eq, Ord, Show)

instance FromJSON Location where
    parseJSON = withText "location" $ \case
        "headers"     -> pure Headers
        "header"      -> pure Header
        "uri"         -> pure Uri
        "querystring" -> pure Querystring
        "statusCode"  -> pure StatusCode
        e             -> fail ("Unknown Location: " ++ Text.unpack e)

nullary stage2 ''Location

location :: Protocol -> Bool -> Maybe Location -> Location
location _ True = const Body
location p _    = fromMaybe $
    case p of
        Json     -> BodyJson
        RestJson -> BodyJson
        Xml      -> BodyXml
        RestXml  -> BodyXml
        Query    -> Querystring
        Ec2      -> Querystring

data Seg
    = Seg Text
    | Var Text
      deriving (Eq, Show)

segParser :: Parser Seg
segParser = Seg <$> AText.takeWhile1 (end '{') <|> Var <$> var
  where
    var = AText.char '{' *> AText.takeWhile1 (end '}') <* AText.char '}'

    end x y | x == y = False
    end _ '?'        = False
    end _ _          = True

instance A.ToJSON Seg where
    toJSON = \case
        Seg t -> A.object ["type" A..= "const", "value" A..= t]
        Var t -> A.object ["type" A..= "var",   "value" A..= name t]
      where
        name = fieldName . Text.replace "+" ""

segVars :: Traversal' Seg Text
segVars f = \case
    Seg x -> pure (Seg x)
    Var x -> Var <$> f x

data URI = URI
    { _uriPath  :: [Seg]
    , _uriQuery :: [Seg]
    } deriving (Eq, Show)

uriParser :: Parser URI
uriParser = URI
    <$> some segParser
    <*> AText.option [] (AText.char '?' *> some segParser)
    <*  AText.endOfInput

instance FromJSON URI where
    parseJSON = withText "uri" (either fail return . parseOnly uriParser)

record stage2 ''URI

uriSegments :: Traversal' URI Seg
uriSegments f x = URI <$> traverse f (_uriPath x) <*> traverse f (_uriQuery x)

newtype Abbrev = Abbrev { unAbbrev :: Text }
    deriving (Eq, Show, A.ToJSON)

instance FromJSON Abbrev where
    parseJSON = withText "service_abbreviation" (pure . mkAbbrev)

mkAbbrev :: Text -> Abbrev
mkAbbrev = Abbrev . Text.replace "/" "" . Text.replace " " "" . stripAWS

maybeAbbrev :: Text -> Maybe Abbrev -> Abbrev
maybeAbbrev t = fromMaybe (mkAbbrev t)

newtype Library = Library Text
    deriving (Eq, Show, A.ToJSON, FromJSON)

instance ToFilePath Library where
    toFilePath (Library t) = Text.unpack t

newtype NS = NS [Text]
    deriving (Eq, Ord, Show)

instance A.ToJSON NS where
    toJSON (NS xs) = A.toJSON (Text.intercalate "." xs)

instance FromJSON NS where
    parseJSON = withText "namespace" (pure . NS . Text.split (== '.'))

instance ToFilePath NS where
    toFilePath (NS xs) = Text.unpack (Text.intercalate "/" xs) <.> "hs"

namespace :: [Text] -> NS
namespace = NS . ("Network":) . ("AWS":)

typesNS :: Abbrev -> NS
typesNS (Abbrev a) = namespace [a, "Types"]

operationNS :: Abbrev -> Text -> NS
operationNS (Abbrev a) o = namespace [a, Text.dropWhileEnd (not . isAlpha) o]

requestNS :: Protocol -> NS
requestNS p
    | p == Ec2 || p == Query = NS ["Network", "AWS", "Request", "Query"]
    | otherwise              = NS ["Network", "AWS", "Request"]

data Override = Override
    { _oRenameTo   :: Maybe Text             -- ^ Rename type
    , _oReplacedBy :: Maybe Text             -- ^ Existing type that supplants this type
    , _oSumPrefix  :: Maybe Text             -- ^ Sum constructor prefix
    , _oRequired   :: HashSet (CI Text)      -- ^ Required fields
    , _oRenamed    :: HashMap (CI Text) Text -- ^ Rename fields
    } deriving (Eq, Show)

makeLenses ''Override

instance FromJSON Override where
    parseJSON = withObject "override" $ \o -> Override
        <$> o .:? "rename_to"
        <*> o .:? "replaced_by"
        <*> o .:? "sum_prefix"
        <*> o .:? "required" .!= mempty
        <*> o .:? "renamed"  .!= mempty

data Overrides = Overrides
    { _oLibrary           :: !Library
    , _oOperationsModules :: [NS]
    , _oTypesModules      :: [NS]
    , _oOverrides         :: HashMap Text Override
    } deriving (Eq, Show)

makeLenses ''Overrides

instance FromJSON Overrides where
    parseJSON = withObject "overrides" $ \o -> Overrides
        <$> o .:  "library"
        <*> o .:? "operationsModules" .!= mempty
        <*> o .:? "typesModules"      .!= mempty
        <*> o .:? "overrides"         .!= mempty

data Model = Model
    { _mName      :: String
    , _mVersion   :: String
    , _mPath      :: FilePath
    , _mModel     :: Object
    , _mOverrides :: Overrides
    } deriving (Show, Eq)

makeLenses ''Model

instance Ord Model where
    compare a b = comparing _mName a b <> comparing _mVersion a b

data Templates = Templates
    { _tCabal    :: Template
    , _tService  :: Template
    , _tProtocol :: Protocol -> (Template, Template)
    }

dots :: FilePath -> Bool
dots "."  = False
dots ".." = False
dots _    = True
