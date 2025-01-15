module Gen.Output.Template.CabalFile where

import Data.Aeson (toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Gen.Prelude

data CabalFile = CabalFile
  { libraryName :: Text,
    libraryVersion :: Text,
    serviceAbbrev :: Text,
    serviceFullName :: Text,
    apiVersion :: Text,
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
      ("exposedModules", toJSON exposedModules),
      ("otherModules", toJSON otherModules),
      ("extraDependencies", toJSON extraDependencies)
    ]
