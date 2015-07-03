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

-- Module      : Gen.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Types
    ( module Gen.Types
    , module Types
    ) where

import           Control.Error
import           Control.Lens              hiding ((.=))
import           Data.Aeson
import           Data.Bifunctor
import           Data.List                 (nub, sort, sortOn)
import           Data.Monoid               hiding (Product, Sum)
import           Data.Ord
import qualified Data.SemVer               as SemVer
import           Data.Text                 (Text)
import qualified Data.Text.Lazy            as LText
import qualified Data.Text.Lazy.Builder    as Build
import           Data.Time
import qualified Filesystem.Path.CurrentOS as Path
import           Formatting
import           Gen.Text
import           Gen.TH
import           Gen.Types.Ann             as Types
import           Gen.Types.Data            as Types
import           Gen.Types.Help            as Types
import           Gen.Types.Id              as Types
import           Gen.Types.Map             as Types
import           Gen.Types.Notation        as Types
import           Gen.Types.NS              as Types
import           Gen.Types.Orphans         ()
import           Gen.Types.Pager           as Types
import           Gen.Types.Retry           as Types
import           Gen.Types.Service         as Types
import           Gen.Types.URI             as Types
import           Gen.Types.Waiter          as Types
import           GHC.Generics              (Generic)
import           GHC.TypeLits
import           Text.EDE                  (Template)

type Error = LText.Text
type Path  = Path.FilePath

toTextIgnore :: Path -> Text
toTextIgnore = either id id . Path.toText

data Replace = Replace
    { _replaceName     :: Id
    , _replaceDeriving :: Set Derive
    } deriving (Eq, Show, Generic)

makeLenses ''Replace

instance FromJSON Replace where
    parseJSON = gParseJSON' (lower & field %~ (. stripPrefix "replace"))

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

newtype Version (v :: Symbol) = Version SemVer.Version
    deriving (Eq, Show)

instance ToJSON (Version v) where
    toJSON (Version v) = toJSON (SemVer.toText v)

semver :: Format a (Version v -> a)
semver = later (\(Version v) -> Build.fromText (SemVer.toText v))

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
    { _libraryName      :: Text
    , _referenceUrl     :: Text
    , _operationUrl     :: Text
    , _operationModules :: [NS]
    , _typeModules      :: [NS]
    , _typeOverrides    :: Map Id Override
    , _ignoredWaiters   :: Set Id
    }

makeClassy ''Config

instance FromJSON Config where
    parseJSON = withObject "config" $ \o -> Config
        <$> o .:  "libraryName"
        <*> o .:  "referenceUrl"
        <*> o .:  "operationUrl"
        <*> o .:? "operationModules" .!= mempty
        <*> o .:? "typeModules"      .!= mempty
        <*> o .:? "typeOverrides"    .!= mempty
        <*> o .:? "ignoredWaiters"   .!= mempty

data Library = Library
    { _versions' :: Versions
    , _config'   :: Config
    , _service'  :: Service Identity SData SData WData
    , _instance' :: Rendered
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
            [ "referenceUrl"     .= (l ^. referenceUrl)
            , "operationUrl"     .= (l ^. operationUrl)
            , "plainDescription" .= Desc 0 (l ^. documentation)
            , "cabalDescription" .= Desc 4 (l ^. documentation)
            , "documentation"    .= (l ^. documentation)
            , "libraryName"      .= (l ^. libraryName)
            , "libraryNamespace" .= (l ^. libraryNS)
            , "libraryVersion"   .= (l ^. libraryVersion)
            , "clientVersion"    .= (l ^. clientVersion)
            , "coreVersion"      .= (l ^. coreVersion)
            , "typeModules"      .= sort (l ^.  typeModules)
            , "operationModules" .= sort (l ^.  operationModules)
            , "exposedModules"   .= sort (l ^.  exposedModules)
            , "otherModules"     .= sort (l ^.  otherModules)
            , "operations"       .= (l ^.. operations . each)
            , "shapes"           .= sort (l ^.. shapes  . each)
            , "waiters"          .= (l ^.. waiters . each)
            , "serviceInstance"  .= (l ^.  instance')
            ]

-- FIXME: Remove explicit construction of getters, just use functions.
libraryNS, typesNS, waitersNS, fixturesNS :: Getter Library NS
libraryNS  = serviceAbbrev . to (mappend "Network.AWS"  . mkNS)
typesNS    = libraryNS     . to (<> "Types")
waitersNS  = libraryNS     . to (<> "Waiters")
fixturesNS = serviceAbbrev . to (mappend "Test.AWS.Gen" . mkNS)

otherModules, exposedModules :: Getter Library [NS]
otherModules   = to (\l -> l ^. operationModules <> l ^. typeModules)
exposedModules = to f
  where
    f x =
        let ns = x ^. libraryNS
         in x ^.  typesNS
          : x ^.  waitersNS
          : x ^.. operations . each . to (operationNS ns . view opName)

data Templates = Templates
    { cabalTemplate           :: Template
    , tocTemplate             :: Template
    , waitersTemplate         :: Template
    , readmeTemplate          :: Template
    , exampleCabalTemplate    :: Template
    , exampleMakefileTemplate :: Template
    , exampleStackTemplate    :: Template
    , operationTemplate       :: Template
    , typesTemplate           :: Template
    , testsTemplate           :: Template
    , fixturesTemplate        :: Template
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

uniq :: Ord a => [a] -> [a]
uniq = sort . nub
