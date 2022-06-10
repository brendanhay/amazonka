{-# LANGUAGE TemplateHaskell #-}

module Gen.Types.Config where

import qualified Control.Lens as Lens
import Data.Aeson ((.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.List as List
import Data.Ord (Down (..))
import qualified Data.Text as Text
import qualified Data.Time as Time
import Gen.Prelude
import Gen.TH
import Gen.Text
import Gen.Types.Ann
import Gen.Types.Data
import Gen.Types.Id
import Gen.Types.NS
import Gen.Types.Service
import Gen.Types.TypeOf
import qualified System.FilePath as FilePath
import Text.EDE (Template)

data Replace = Replace
  { _replaceName :: Id,
    _replaceUnderive :: [Derive]
  }
  deriving (Eq, Show, Generic)

$(Lens.makeLenses ''Replace)

instance FromJSON Replace where
  parseJSON = gParseJSON' (lower & field %~ (. stripPrefix "replace"))

instance TypeOf Replace where
  typeOf Replace {..} =
    TType (typeId _replaceName) (derivingBase List.\\ _replaceUnderive)

data Override = Override
  { -- | Rename type
    _renamedTo :: Maybe Id,
    -- | Existing type that supplants this type
    _replacedBy :: Maybe Replace,
    -- | Required fields
    _requiredFields :: [Id],
    -- | Optional fields
    _optionalFields :: [Id],
    -- | Rename fields
    _renamedFields :: HashMap Id Id
  }
  deriving (Eq, Show)

$(Lens.makeLenses ''Override)

instance FromJSON Override where
  parseJSON =
    Aeson.withObject "override" $ \o ->
      Override
        <$> (o .:? "renamedTo" <&> fmap (\unsafe -> Id unsafe unsafe))
        <*> o .:? "replacedBy"
        <*> o .:? "requiredFields" .!= mempty
        <*> o .:? "optionalFields" .!= mempty
        <*> o .:? "renamedFields" .!= mempty

defaultOverride :: Override
defaultOverride =
  Override
    { _renamedTo = Nothing,
      _replacedBy = Nothing,
      _requiredFields = mempty,
      _optionalFields = mempty,
      _renamedFields = mempty
    }

newtype Version (v :: Symbol) = Version {semver :: Text}
  deriving (Eq, Show)

instance ToJSON (Version v) where
  toJSON (Version v) = Aeson.toJSON v

type LibraryVer = Version "library"

type ClientVer = Version "client"

data Versions = Versions
  { _libraryVersion :: LibraryVer,
    _clientVersion :: ClientVer
  }
  deriving (Show)

$(Lens.makeClassy ''Versions)

data Config = Config
  { _libraryName :: Text,
    _operationModules :: [NS],
    -- | Custom plugin functions to be applied to the generated 'AWSRequest.request'
    -- instance body. Each function is of the form @Request a -> Request a@.
    --
    -- Using a wildcard key of @*@ in the configuration results in the plugins
    -- being applied to _all_ operations. The wildcard is only applied if no
    -- matching operation name is found in the map.
    _operationPlugins :: HashMap Id [Text],
    _typeModules :: [NS],
    _typeOverrides :: HashMap Id Override,
    _ignoredWaiters :: HashSet Id,
    _ignoredPaginators :: HashSet Id,
    _extraDependencies :: [Text]
  }

$(Lens.makeClassy ''Config)

instance FromJSON Config where
  parseJSON =
    Aeson.withObject "config" $ \o ->
      Config
        <$> o .: "libraryName"
        <*> o .:? "operationModules" .!= mempty
        <*> o .:? "operationPlugins" .!= mempty
        <*> o .:? "typeModules" .!= mempty
        <*> o .:? "typeOverrides" .!= mempty
        <*> o .:? "ignoredWaiters" .!= mempty
        <*> o .:? "ignoredPaginators" .!= mempty
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
  toJSON l = Aeson.Object (x <> y)
    where
      y =
        case Aeson.toJSON (l ^. metadata) of
          Aeson.Object obj -> obj
          other -> error $ "metadata: expected JSON object, got " ++ show other

      x =
        mconcat
          [ "documentation" .= (l ^. documentation),
            "libraryName" .= (l ^. libraryName),
            "libraryNamespace" .= (l ^. libraryNS),
            "libraryHyphenated" .= nsHyphenate (l ^. libraryNS),
            "libraryVersion" .= (l ^. libraryVersion),
            "clientVersion" .= (l ^. clientVersion),
            "serviceInstance" .= (l ^. instance'),
            "typeModules" .= List.sort (l ^. typeModules),
            "operationModules" .= List.sort (l ^. operationModules),
            "exposedModules" .= List.sort (l ^. exposedModules),
            "otherModules" .= List.sort (l ^. otherModules),
            "extraDependencies" .= List.sort (l ^. extraDependencies),
            "operations"
              .= List.sortOn _opName (l ^.. operations . Lens.each),
            "shapes" .= List.sort (l ^.. shapes . Lens.each),
            "waiters" .= (l ^.. waiters . Lens.each)
          ]

-- FIXME: Remove explicit construction of getters, just use functions.
libraryNS, typesNS, waitersNS, fixturesNS, lensNS :: Getter Library NS
libraryNS = serviceAbbrev . Lens.to (mappend "Amazonka" . mkNS)
typesNS = libraryNS . Lens.to (<> "Types")
waitersNS = libraryNS . Lens.to (<> "Waiters")
fixturesNS = serviceAbbrev . Lens.to (mappend "Test.Amazonka.Gen" . mkNS)
lensNS = libraryNS . Lens.to (<> "Lens")

otherModules :: Getter Library [NS]
otherModules = Lens.to f
  where
    f x =
      x ^. operationModules
        <> x ^. typeModules
        <> mapMaybe (shapeNS x) (x ^.. shapes . Lens.each)

    shapeNS x = \case
      s@Prod {} -> Just $ (x ^. typesNS) <> ((mkNS . typeId) $ identifier s)
      s@Sum {} -> Just $ (x ^. typesNS) <> ((mkNS . typeId) $ identifier s)
      Fun {} -> Nothing

exposedModules :: Getter Library [NS]
exposedModules = Lens.to f
  where
    f x =
      let ns = x ^. libraryNS
       in x ^. typesNS :
          x ^. lensNS :
          x ^. waitersNS :
          x ^.. operations . Lens.each . Lens.to (operationNS ns . Lens.view opName)

data Templates = Templates
  { cabalTemplate :: Template,
    tocTemplate :: Template,
    waitersTemplate :: Template,
    readmeTemplate :: Template,
    operationTemplate :: Template,
    typesTemplate :: Template,
    lensTemplate :: Template,
    sumTemplate :: Template,
    productTemplate :: Template,
    testMainTemplate :: Template,
    testNamespaceTemplate :: Template,
    testInternalTemplate :: Template,
    fixturesTemplate :: Template,
    fixtureRequestTemplate :: Template,
    blankTemplate :: Template
  }

data Model = Model
  { _modelName :: Text,
    _modelVersions :: [UTCTime],
    _modelVersion :: UTCTime,
    _modelPath :: FilePath
  }
  deriving (Eq, Show)

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
  let sortedVersions =
        List.sortOn Down (mapMaybe parseVersion xs)

      parseVersion date =
        (,date)
          <$> Time.parseTimeM
            True
            Time.defaultTimeLocale
            (Time.iso8601DateFormat Nothing)
            (FilePath.takeFileName date)

  case sortedVersions of
    [] -> Left ("No valid model versions found in " ++ show xs)
    version : _ ->
      pure
        Model
          { _modelName = fromString (FilePath.takeFileName path),
            _modelVersions = map fst sortedVersions,
            _modelVersion = fst version,
            _modelPath = path </> snd version
          }
