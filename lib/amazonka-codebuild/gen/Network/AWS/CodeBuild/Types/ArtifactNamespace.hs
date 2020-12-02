{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ArtifactNamespace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ArtifactNamespace where

import Network.AWS.Prelude

data ArtifactNamespace
  = ANBuildId
  | ANNone
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

instance FromText ArtifactNamespace where
  parser =
    takeLowerText >>= \case
      "build_id" -> pure ANBuildId
      "none" -> pure ANNone
      e ->
        fromTextError $
          "Failure parsing ArtifactNamespace from value: '" <> e
            <> "'. Accepted values: build_id, none"

instance ToText ArtifactNamespace where
  toText = \case
    ANBuildId -> "BUILD_ID"
    ANNone -> "NONE"

instance Hashable ArtifactNamespace

instance NFData ArtifactNamespace

instance ToByteString ArtifactNamespace

instance ToQuery ArtifactNamespace

instance ToHeader ArtifactNamespace

instance ToJSON ArtifactNamespace where
  toJSON = toJSONText

instance FromJSON ArtifactNamespace where
  parseJSON = parseJSONText "ArtifactNamespace"
