{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.BundleType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.BundleType where

import Network.AWS.Prelude

data BundleType
  = JSON
  | TAR
  | TGZ
  | Yaml
  | Zip
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText BundleType where
  parser =
    takeLowerText >>= \case
      "json" -> pure JSON
      "tar" -> pure TAR
      "tgz" -> pure TGZ
      "yaml" -> pure Yaml
      "zip" -> pure Zip
      e ->
        fromTextError $
          "Failure parsing BundleType from value: '" <> e
            <> "'. Accepted values: json, tar, tgz, yaml, zip"

instance ToText BundleType where
  toText = \case
    JSON -> "JSON"
    TAR -> "tar"
    TGZ -> "tgz"
    Yaml -> "YAML"
    Zip -> "zip"

instance Hashable BundleType

instance NFData BundleType

instance ToByteString BundleType

instance ToQuery BundleType

instance ToHeader BundleType

instance ToJSON BundleType where
  toJSON = toJSONText

instance FromJSON BundleType where
  parseJSON = parseJSONText "BundleType"
