{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeOperators          #-}

-- Module      : Gen.Types.Config
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.Types.Config where

import Control.Error
import Control.Lens  hiding ((.=))

import Data.Aeson
import Data.List   (sort, sortOn, (\\))
import Data.Monoid hiding (Product, Sum)
import Data.Ord
import Data.Text   (Text)
import Data.Time

import Formatting

import Gen.Text
import Gen.TH
import Gen.Types.Ann
import Gen.Types.Data
import Gen.Types.Id
import Gen.Types.Map
import Gen.Types.NS
import Gen.Types.Service
import Gen.Types.TypeOf

import GHC.Generics (Generic)
import GHC.TypeLits

import Text.EDE (Template)

import qualified Data.Text.Lazy            as LText
import qualified Data.Text.Lazy.Builder    as Build
import qualified Filesystem.Path.CurrentOS as Path

type Error = LText.Text
type Path  = Path.FilePath

toTextIgnore :: Path -> Text
toTextIgnore = either id id . Path.toText

data Replace = Replace
    { _replaceName     :: Id
    , _replaceUnderive :: [Derive]
    } deriving (Eq, Show, Generic)

makeLenses ''Replace

instance FromJSON Replace where
    parseJSON = gParseJSON' (lower & field %~ (. stripPrefix "replace"))

instance TypeOf Replace where
    typeOf Replace{..} =
        TType (typeId _replaceName) (derivingBase \\ _replaceUnderive)

data Override = Override
    { _renamedTo      :: Maybe Id      -- ^ Rename type
    , _replacedBy     :: Maybe Replace -- ^ Existing type that supplants this type
    , _enumPrefix     :: Maybe Text    -- ^ Enum constructor prefix
    , _requiredFields :: [Id]          -- ^ Required fields
    , _optionalFields :: [Id]          -- ^ Optional fields
    , _renamedFields  :: Map Id Id     -- ^ Rename fields
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

defaultOverride :: Override
defaultOverride = Override
    { _renamedTo      = Nothing
    , _replacedBy     = Nothing
    , _enumPrefix     = Nothing
    , _requiredFields = mempty
    , _optionalFields = mempty
    , _renamedFields  = mempty
    }

newtype Version (v :: Symbol) = Version Text
    deriving (Eq, Show)

instance ToJSON (Version v) where
    toJSON (Version v) = toJSON v

semver :: Format a (Version v -> a)
semver = later (\(Version v) -> Build.fromText v)

type LibraryVer = Version "library"
type ClientVer  = Version "client"
type CoreVer    = Version "core"

data Versions = Versions
    { _libraryVersion :: LibraryVer
    , _clientVersion  :: ClientVer
    , _coreVersion    :: CoreVer
    } deriving (Show)

makeClassy ''Versions

data Config = Config
    { _libraryName       :: Text
    , _operationModules  :: [NS]
    , _operationPlugins  :: Map Id [Text]
    , _typeModules       :: [NS]
    , _typeOverrides     :: Map Id Override
    , _ignoredWaiters    :: Set Id
    , _ignoredPaginators :: Set Id
    , _extraDependencies :: [Text]
    }

makeClassy ''Config

instance FromJSON Config where
    parseJSON = withObject "config" $ \o -> Config
        <$> o .:  "libraryName"
        <*> o .:? "operationModules"  .!= mempty
        <*> o .:? "operationPlugins"  .!= mempty
        <*> o .:? "typeModules"       .!= mempty
        <*> o .:? "typeOverrides"     .!= mempty
        <*> o .:? "ignoredWaiters"    .!= mempty
        <*> o .:? "ignoredPaginators" .!= mempty
        <*> o .:? "extraDependencies" .!= mempty

data Library = Library
    { _versions' :: Versions
    , _config'   :: Config
    , _service'  :: Service Identity SData SData WData
    , _instance' :: Fun
    }

makeLenses ''Library

instance HasMetadata Library Identity where
    metadata = service' . metadata'

instance HasService Library Identity SData SData WData where
    service = service'

instance HasConfig Library where
    config = config'

instance HasVersions Library where
    versions = versions'

instance ToJSON Library where
    toJSON l = Object (x <> y)
      where
        Object y = toJSON (l ^. metadata)
        Object x = object
            [ "documentation"     .= (l ^. documentation)
            , "libraryName"       .= (l ^. libraryName)
            , "libraryNamespace"  .= (l ^. libraryNS)
            , "libraryHyphenated" .= nsHyphenate (l ^. libraryNS)
            , "libraryVersion"    .= (l ^. libraryVersion)
            , "clientVersion"     .= (l ^. clientVersion)
            , "coreVersion"       .= (l ^. coreVersion)
            , "serviceInstance"   .= (l ^.  instance')
            , "typeModules"       .= sort (l ^. typeModules)
            , "operationModules"  .= sort (l ^. operationModules)
            , "exposedModules"    .= sort (l ^. exposedModules)
            , "otherModules"      .= sort (l ^. otherModules)
            , "extraDependencies" .= sort (l ^. extraDependencies)
            , "operations"        .= (l ^.. operations . each)
            , "shapes"            .= sort (l ^.. shapes . each)
            , "waiters"           .= (l ^.. waiters . each)
            ]

-- FIXME: Remove explicit construction of getters, just use functions.
libraryNS, typesNS, sumNS, productNS, waitersNS, fixturesNS :: Getter Library NS
libraryNS  = serviceAbbrev . to (mappend "Network.AWS"  . mkNS)
typesNS    = libraryNS     . to (<> "Types")
sumNS      = typesNS       . to (<> "Sum")
productNS  = typesNS       . to (<> "Product")
waitersNS  = libraryNS     . to (<> "Waiters")
fixturesNS = serviceAbbrev . to (mappend "Test.AWS.Gen" . mkNS)

otherModules :: Getter Library [NS]
otherModules = to f
  where
    f x = x ^. sumNS
        : x ^. productNS
        : x ^. operationModules
       <> x ^. typeModules

exposedModules :: Getter Library [NS]
exposedModules = to f
  where
    f x =
        let ns = x ^. libraryNS
         in x ^.  typesNS
          : x ^.  waitersNS
          : x ^.. operations . each . to (operationNS ns . view opName)

data Templates = Templates
    { cabalTemplate          :: !Template
    , tocTemplate            :: !Template
    , waitersTemplate        :: !Template
    , readmeTemplate         :: !Template
    , operationTemplate      :: !Template
    , typesTemplate          :: !Template
    , sumTemplate            :: !Template
    , productTemplate        :: !Template
    , testMainTemplate       :: !Template
    , testNamespaceTemplate  :: !Template
    , testInternalTemplate   :: !Template
    , fixturesTemplate       :: !Template
    , fixtureRequestTemplate :: !Template
    , blankTemplate          :: !Template
    }

data Model = Model
    { _modelName    :: Text
    , _modelVersion :: UTCTime
    , _modelPath    :: Path
    } deriving (Eq, Show)

makeLenses ''Model

configFile, annexFile :: Getter Model Path
configFile = to (flip Path.addExtension "json" . Path.fromText . _modelName)
annexFile  = configFile

serviceFile, waitersFile, pagersFile :: Getter Model Path
serviceFile = to (flip Path.append "service-2.json"    . _modelPath)
waitersFile = to (flip Path.append "waiters-2.json"    . _modelPath)
pagersFile  = to (flip Path.append "paginators-1.json" . _modelPath)

loadModel :: Path -> [Path] -> Either Error Model
loadModel p xs = uncurry (Model n) <$>
    headErr (format ("No valid model versions found in " % shown) xs) vs
  where
    vs = sortOn Down (mapMaybe parse xs)
    n  = toTextIgnore (Path.filename p)

    parse d = (,d) <$> parseTimeM True defaultTimeLocale
        (iso8601DateFormat Nothing)
        (Path.encodeString (Path.filename d))
