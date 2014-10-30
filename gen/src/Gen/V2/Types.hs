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

-- Module      : Gen.V2.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.V2.Types where

import           Control.Applicative
import           Control.Lens         (makeLenses)
import qualified Data.Aeson           as A
import           Data.Attoparsec.Text (Parser, parseOnly)
import qualified Data.Attoparsec.Text as AText
import           Data.Bifunctor
import           Data.CaseInsensitive (CI)
import           Data.Foldable        (Foldable)
import           Data.HashMap.Strict  (HashMap)
import           Data.Jason.Types     hiding (Parser)
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Traversable     (Traversable, traverse)
import           Gen.V2.JSON          ()
import           Gen.V2.Names
import           Gen.V2.TH
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

nullary stage2 ''Signature

data Protocol
    = JSON
    | RestJSON
    | RestXML
    | Query
      deriving (Eq, Show)

instance FromJSON Protocol where
    parseJSON = withText "protocol" $ \case
        "json"      -> pure JSON
        "rest-json" -> pure RestJSON
        "rest-xml"  -> pure RestXML
        "query"     -> pure Query
        "ec2"       -> pure Query
        e           -> fail ("Unknown Protocol: " ++ Text.unpack e)

nullary stage2 ''Protocol

data Timestamp
    = RFC822
    | ISO8601
    | POSIX
      deriving (Eq, Show)

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
      deriving (Eq, Show)

nullary stage1 ''Location
nullary stage2 ''Location

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
        Var t -> A.object ["type" A..= "var",   "value" A..= t]

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

data Override = Override
    { _ovRenameTo  :: Maybe Text             -- ^ Rename type
    , _ovExistsAs  :: Maybe Text             -- ^ Existing type that supplants this type
    , _ovSumPrefix :: Maybe Text             -- ^ Sum constructor prefix
    , _ovRequired  :: [CI Text]              -- ^ Required fields
    , _ovIgnored   :: [CI Text]              -- ^ Ignored fields
    , _ovRenamed   :: HashMap (CI Text) Text -- ^ Rename fields
    , _ovTyped     :: HashMap (CI Text) Text -- ^ Field types
    } deriving (Eq, Show)

record stage1 ''Override

data Model = Model
    { _mName      :: String
    , _mVersion   :: String
    , _mPath      :: FilePath
    , _mModel     :: Object
    , _mOverrides :: HashMap (CI Text) Override
    } deriving (Show, Eq)

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
