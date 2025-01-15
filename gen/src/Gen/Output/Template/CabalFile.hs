module Gen.Output.Template.CabalFile
  ( CabalFile (..),
    arguments,
    fromLibrary,
  )
where

import Data.Aeson (toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Gen.Prelude
import Gen.Types.Config (Library)
import qualified Gen.Types.Config as Config
import Gen.Types.NS (unNS)
import Gen.Types.Service (metadata, service)
import qualified Gen.Types.Service as Service

-- | Arguments for the @cabal.ede@ template.
data CabalFile = CabalFile
  { libraryName :: Text,
    libraryVersion :: Text,
    serviceAbbrev :: Text,
    serviceFullName :: Text,
    apiVersion :: Text,
    clientVersion :: Text,
    exposedModules :: [Text],
    otherModules :: [Text],
    extraDependencies :: [Text]
  }
  deriving (Generic)

arguments :: CabalFile -> HashMap Text Aeson.Value
arguments CabalFile {..} =
  HashMap.fromList
    [ ("libraryName", toJSON libraryName),
      ("libraryVersion", toJSON libraryVersion),
      ("serviceAbbrev", toJSON serviceAbbrev),
      ("serviceFullName", toJSON serviceFullName),
      ("apiVersion", toJSON apiVersion),
      ("clientVersion", toJSON clientVersion),
      ("exposedModules", toJSON exposedModules),
      ("otherModules", toJSON otherModules),
      ("extraDependencies", toJSON extraDependencies)
    ]

fromLibrary :: Library -> CabalFile
fromLibrary lib =
  CabalFile
    { libraryName = lib ^. Config.libraryName,
      libraryVersion = Config.semver $ Config._version' lib,
      serviceAbbrev = lib ^. service . metadata . Service.serviceAbbrev,
      serviceFullName = lib ^. service . metadata . Service.serviceFullName,
      apiVersion = lib ^. service . metadata . Service.apiVersion,
      clientVersion = Config.semver $ Config._version' lib,
      exposedModules = unNS <$> lib ^. Config.exposedModules,
      otherModules = unNS <$> lib ^. Config.otherModules,
      extraDependencies = lib ^. Config.config . Config.extraDependencies
    }
