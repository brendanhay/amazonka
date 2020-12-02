{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ArtifactsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ArtifactsType where

import Network.AWS.Prelude

data ArtifactsType
  = Codepipeline
  | NoArtifacts
  | S3
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

instance FromText ArtifactsType where
  parser =
    takeLowerText >>= \case
      "codepipeline" -> pure Codepipeline
      "no_artifacts" -> pure NoArtifacts
      "s3" -> pure S3
      e ->
        fromTextError $
          "Failure parsing ArtifactsType from value: '" <> e
            <> "'. Accepted values: codepipeline, no_artifacts, s3"

instance ToText ArtifactsType where
  toText = \case
    Codepipeline -> "CODEPIPELINE"
    NoArtifacts -> "NO_ARTIFACTS"
    S3 -> "S3"

instance Hashable ArtifactsType

instance NFData ArtifactsType

instance ToByteString ArtifactsType

instance ToQuery ArtifactsType

instance ToHeader ArtifactsType

instance ToJSON ArtifactsType where
  toJSON = toJSONText

instance FromJSON ArtifactsType where
  parseJSON = parseJSONText "ArtifactsType"
