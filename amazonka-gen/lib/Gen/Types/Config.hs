{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Gen.Types.Config
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject Lens.to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.Types.Config where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.Ord as Ord
import Data.Aeson.Types (Value (..), (.:), (.:?), (.!=), (.=))
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Data.Time as Time
import System.FilePath ((</>))
import Gen.Prelude 
import qualified Gen.TH 
import  Gen.Text 
import  Gen.Types.Ann 
import  Gen.Types.Data 
import  Gen.Types.Id 
import  Gen.Types.Map 
import  Gen.Types.NS 
import  Gen.Types.Service 
import  Gen.Types.TypeOf 
import qualified System.FilePath as FilePath
import qualified Text.EDE as EDE
import Text.EDE (Template)

data Replace = Replace
  { _replaceName :: Id,
    _replaceUnderive :: [Derive]
  }
  deriving stock (Eq, Show, Generic)

$(Lens.makeLenses ''Replace)

instance FromJSON Replace where
  parseJSON =
    Gen.TH.genericParseJSON
      (Gen.TH.lower & Gen.TH.field %~ (. stripPrefix "replace"))

instance TypeOf Replace where
  typeOf Replace {..} =
    TType (typeId _replaceName) (derivingBase List.\\ _replaceUnderive)

data Override = Override
  { -- | Rename type
    _renamedTo :: Maybe Id,
    -- | Existing type that supplants this type
    _replacedBy :: Maybe Replace,
    -- | Enum constructor prefix
    _enumPrefix :: Maybe Text,
    -- | Required fields
    _requiredFields :: [Id],
    -- | Optional fields
    _optionalFields :: [Id],
    -- | Rename fields
    _renamedFields :: HashMap Id Id
  }
  deriving stock (Eq, Show)

$(Lens.makeLenses ''Override)

instance FromJSON Override where
  parseJSON = Aeson.withObject "override" $ \o ->
    Override
      <$> o .:? "renamedTo"
      <*> o .:? "replacedBy"
      <*> o .:? "enumPrefix"
      <*> o .:? "requiredFields" .!= mempty
      <*> o .:? "optionalFields" .!= mempty
      <*> o .:? "renamedFields" .!= mempty

defaultOverride :: Override
defaultOverride =
  Override
    { _renamedTo = Nothing,
      _replacedBy = Nothing,
      _enumPrefix = Nothing,
      _requiredFields = mempty,
      _optionalFields = mempty,
      _renamedFields = mempty
    }

newtype Version (v :: Symbol) = Version Text
  deriving stock (Eq, Show)

instance ToJSON (Version v) where
  toJSON (Version v) = Aeson.toJSON v

semver :: Version v -> String
semver (Version v) = Text.unpack v

type LibraryVer = Version "library"

type ClientVer = Version "client"

type CoreVer = Version "core"

data Versions = Versions
  { _libraryVersion :: LibraryVer,
    _clientVersion :: ClientVer,
    _coreVersion :: CoreVer
  }
  deriving stock (Show)

$(Lens.makeClassy ''Versions)

data Config = Config
  { _libraryName :: Text,
    _operationModules :: [NS],
    _operationPlugins :: HashMap Id [Text],
    _typeModules :: [NS],
    _typeOverrides :: HashMap Id Override,
    _extraDependencies :: [Text]
  }

$(Lens.makeClassy ''Config)

instance FromJSON Config where
  parseJSON = Aeson.withObject "config" $ \o ->
    Config
      <$> o .: "libraryName"
      <*> o .:? "operationModules" .!= mempty
      <*> o .:? "operationPlugins" .!= mempty
      <*> o .:? "typeModules" .!= mempty
      <*> o .:? "typeOverrides" .!= mempty
      <*> o .:? "extraDependencies" .!= mempty

data Library = Library
  { _versions' :: Versions,
    _config' :: Config,
    _service' :: Service Identity SData SData WData,
    _instance' :: Fun
  }

$(Lens.makeLenses ''Library)

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
      y = case Aeson.toJSON (l ^. metadata) of
        Object obj -> obj
        oops -> error $ "metadata: expected JSON object, got " ++ show oops
      x =
        mconcat
          [ "documentation" .= (l ^. documentation),
            "libraryName" .= (l ^. libraryName),
            "libraryNamespace" .= (l ^. libraryNS),
            "libraryHyphenated" .= nsHyphenate (l ^. libraryNS),
            "libraryVersion" .= (l ^. libraryVersion),
            "clientVersion" .= (l ^. clientVersion),
            "coreVersion" .= (l ^. coreVersion),
            "serviceInstance" .= (l ^. instance'),
            "typeModules" .= List.sort (l ^. typeModules),
            "operationModules" .= List.sort (l ^. operationModules),
            "exposedModules" .= List.sort (l ^. exposedModules),
            "otherModules" .= List.sort (l ^. otherModules),
            "extraDependencies" .= List.sort (l ^. extraDependencies),
            "operations" .= (l ^.. operations . Lens.each),
            "shapes" .= List.sort (l ^.. shapes . Lens.each),
            "waiters" .= (l ^.. waiters . Lens.each)
          ]

-- FIXME: Remove explicit construction of getters, just use functions.
libraryNS, typesNS, waitersNS, fixturesNS :: Getter Library NS
libraryNS = serviceAbbrev . Lens.to (mappend "Network.AWS" . mkNS)
typesNS = libraryNS . Lens.to (<> "Types")
waitersNS = libraryNS . Lens.to (<> "Waiters")
fixturesNS = serviceAbbrev . Lens.to (mappend "Test.AWS.Gen" . mkNS)

otherModules :: Getter Library [NS]
otherModules = Lens.to f
  where
    f x =
      x ^. operationModules
        <> x ^. typeModules
        <> mapMaybe (shapeNS x) (x ^.. shapes . Lens.each)
    shapeNS x s@(Prod _ _ _) = Just $ (x ^. typesNS) <> ((mkNS . typeId) $ identifier s)
    shapeNS x s@(Sum _ _ _) = Just $ (x ^. typesNS) <> ((mkNS . typeId) $ identifier s)
    shapeNS _ (Fun _) = Nothing

exposedModules :: Getter Library [NS]
exposedModules = Lens.to f
  where
    f x =
      let ns = x ^. libraryNS
       in x ^. typesNS :
          x ^. waitersNS :
          x ^.. operations . Lens.each . Lens.to (operationNS ns . Lens.view opName)

data Templates = Templates
  { cabalTemplate :: !Template,
    tocTemplate :: !Template,
    waitersTemplate :: !Template,
    readmeTemplate :: !Template,
    operationTemplate :: !Template,
    typesTemplate :: !Template,
    sumTemplate :: !Template,
    productTemplate :: !Template,
    testMainTemplate :: !Template,
    testNamespaceTemplate :: !Template,
    testInternalTemplate :: !Template,
    fixturesTemplate :: !Template,
    fixtureRequestTemplate :: !Template,
    blankTemplate :: !Template
  }

data Model = Model
  { _modelName :: Text,
    _modelVersion :: Day,
    _modelPath :: FilePath
  }
  deriving stock (Eq, Show)

$(Lens.makeLenses ''Model)

configFile, annexFile :: Model -> FilePath
configFile = flip FilePath.addExtension "json" . Text.unpack . _modelName
annexFile = configFile

serviceFile, waitersFile, pagersFile :: Model -> FilePath
serviceFile = flip FilePath.combine "service-2.json" . _modelPath
waitersFile = flip FilePath.combine "waiters-2.json" . _modelPath
pagersFile = flip FilePath.combine "paginators-1.json" . _modelPath

loadModel :: FilePath -> [FilePath] -> Either String Model
loadModel path xs = do
  (_modelVersion, _modelPath) <-
    case versions of
       [] -> Left ("No valid model versions found in " ++ show xs) 
       y:_ -> Right y

  pure
    Model
      { _modelName = Text.pack (FilePath.takeFileName path),
        _modelVersion,
        _modelPath
      }
  where
    versions = List.sortOn Ord.Down (mapMaybe parse xs)

    parse dir =
      fmap (,FilePath.normalise (path </> dir)) $
        Time.parseTimeM True Time.defaultTimeLocale "%Y-%-m-%-d" dir
