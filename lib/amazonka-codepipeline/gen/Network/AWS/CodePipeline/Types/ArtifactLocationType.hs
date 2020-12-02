{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ArtifactLocationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ArtifactLocationType where

import Network.AWS.Prelude

data ArtifactLocationType = ALTS3
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

instance FromText ArtifactLocationType where
  parser =
    takeLowerText >>= \case
      "s3" -> pure ALTS3
      e ->
        fromTextError $
          "Failure parsing ArtifactLocationType from value: '" <> e
            <> "'. Accepted values: s3"

instance ToText ArtifactLocationType where
  toText = \case
    ALTS3 -> "S3"

instance Hashable ArtifactLocationType

instance NFData ArtifactLocationType

instance ToByteString ArtifactLocationType

instance ToQuery ArtifactLocationType

instance ToHeader ArtifactLocationType

instance FromJSON ArtifactLocationType where
  parseJSON = parseJSONText "ArtifactLocationType"
