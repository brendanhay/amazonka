{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Compiler.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Types where

import           Compiler.Orphans          ()
import           Control.Error
import           Control.Lens
import           Control.Monad
import           Data.Aeson                (ToJSON (..))
import qualified Data.Aeson                as A
import           Data.CaseInsensitive      (CI)
import           Data.Default.Class
import           Data.Hashable
import qualified Data.HashMap.Strict       as Map
import qualified Data.HashSet              as Set
import           Data.Jason                hiding (ToJSON (..))
import           Data.List                 (intersperse)
import           Data.Monoid
import qualified Data.SemVer               as SemVer
import           Data.String
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Lazy            as LText
import qualified Data.Text.Lazy.Builder    as Build
import           Data.Time
import qualified Filesystem.Path.CurrentOS as Path
import           Formatting
import           Formatting.Time
import           GHC.TypeLits
import           Text.EDE                  (Template)
import           Text.Pandoc               hiding (Format, Template)
import           Text.Pandoc.Pretty        (prefixed, render)

type Compiler = EitherT LazyText
type LazyText = LText.Text

type Set = Set.HashSet
type Map = Map.HashMap

joinKV :: [Text] -> Map Text Text
joinKV = Map.fromList . map (join (,))

mapMaybeV :: (Eq k, Hashable k)
          => (a -> Maybe b)
          -> Map.HashMap k a
          -> Map.HashMap k b
mapMaybeV f = Map.fromList . mapMaybe (\(k, v) -> (k,) <$> f v) . Map.toList

traverseKV :: (Eq k', Hashable k')
           => Traversal (Map k v) (Map k' v') (k, v) (k', v')
traverseKV f = fmap Map.fromList . traverse f . Map.toList

type Path = Path.FilePath

path :: Format a (Path -> a)
path = later (Build.fromText . toTextIgnore)

toTextIgnore :: Path -> Text
toTextIgnore = either id id . Path.toText

data Templates = Templates
    { cabalTemplate           :: Template
    , serviceTemplate         :: Template
    , waitersTemplate         :: Template
    , readmeTemplate          :: Template
    , exampleCabalTemplate    :: Template
    , exampleMakefileTemplate :: Template
    , operationTemplate       :: Template
    , typesTemplate           :: Template
    }

dateDashes :: Format a ([UTCTime] -> a)
dateDashes = later (list . map (bprint dateDash))
  where
    list = ("[" <>) . (<> "]") . mconcat . intersperse ","

failure :: Monad m => Format LText.Text (a -> e) -> a -> EitherT e m b
failure m = Control.Error.left . format m

makePrisms ''Identity

newtype NS = NS [Text]
    deriving (Eq, Ord, Show)

textToNS :: Text -> NS
textToNS = NS . Text.splitOn "."

instance IsString NS where
    fromString "" = mempty
    fromString s  = textToNS (fromString s)

instance Monoid NS where
    mempty = NS []
    mappend (NS xs) (NS ys)
        | null xs   = NS ys
        | null ys   = NS xs
        | otherwise = NS (xs <> ys)

instance FromJSON NS where
    parseJSON = withText "namespace" (pure . textToNS)

instance ToJSON NS where
    toJSON (NS xs) = toJSON (Text.intercalate "." xs)

newtype Help = Help Pandoc

instance IsString Help where
    fromString = Help . readHaddock def

instance FromJSON Help where
    parseJSON = withText "help" (pure . Help . readHtml def . Text.unpack)

instance ToJSON Help where
    toJSON = toJSON . mappend "-- |" . Text.drop 2 . helpToHaddock "-- "

newtype Desc = Desc Help

instance ToJSON Desc where
    toJSON (Desc h) = toJSON (helpToHaddock "" h)

asDesc :: Getter Help Desc
asDesc = to Desc

helpToHaddock :: Text -> Help -> Text
helpToHaddock sep (Help h) = Text.pack
    . render (Just 76)
    . prefixed (Text.unpack sep)
    . fromString
    $ writeHaddock def h

newtype Version (v :: Symbol) = Version SemVer.Version
    deriving (Eq, Show)

instance A.ToJSON (Version v) where
    toJSON (Version v) = A.toJSON (SemVer.toText v)

semver :: Format a (Version v -> a)
semver = later (\(Version v) -> Build.fromText (SemVer.toText v))

type LibraryVer = Version "library"
type ClientVer  = Version "client"
type CoreVer    = Version "core"

data Override = Override
    { _renamedTo      :: Maybe Text         -- ^ Rename type
    , _replacedBy     :: Maybe Text         -- ^ Existing type that supplants this type
    , _enumPrefix     :: Maybe Text         -- ^ Enum constructor prefix
    , _requiredFields :: Set (CI Text)      -- ^ Required fields
    , _optionalFields :: Set (CI Text)      -- ^ Optional fields
    , _renamedFields  :: Map (CI Text) Text -- ^ Rename fields
    } deriving (Eq, Show)

makeLenses ''Override

instance FromJSON Override where
    parseJSON = withObject "override" $ \o -> Override
        <$> o .:? "renamedTo"
        <*> o .:? "replacedBy"
        <*> o .:? "enumPrefix"
        <*> o .:? "requiredFields" .!= mempty
        <*> o .:? "optionalFields" .!= mempty
        <*> o .:? "renamedFields"  .!= mempty

-- FIXME: An Operation should end up referring to a Shape,
-- similarly to a Ref.

data Versions = Versions
    { _libraryVersion :: LibraryVer
    , _clientVersion  :: ClientVer
    , _coreVersion    :: CoreVer
    } deriving (Show)

makeClassy ''Versions

data Config = Config
    { _libraryName      :: Text
    , _referenceUrl     :: Text
    , _operationUrl     :: Text
    , _operationImports :: [NS]
    , _typeImports      :: [NS]
    , _typeOverrides    :: Map Text Override
    }

makeClassy ''Config

instance FromJSON Config where
    parseJSON = withObject "config" $ \o -> Config
        <$> o .:  "libraryName"
        <*> o .:  "referenceUrl"
        <*> o .:  "operationUrl"
        <*> o .:? "operationImports" .!= mempty
        <*> o .:? "typeImports"      .!= mempty
        <*> o .:? "typeOverrides"    .!= mempty
