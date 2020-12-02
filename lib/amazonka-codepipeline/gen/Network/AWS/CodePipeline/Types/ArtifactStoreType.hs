{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ArtifactStoreType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ArtifactStoreType where

import Network.AWS.Prelude

data ArtifactStoreType = S3
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

instance FromText ArtifactStoreType where
  parser =
    takeLowerText >>= \case
      "s3" -> pure S3
      e ->
        fromTextError $
          "Failure parsing ArtifactStoreType from value: '" <> e
            <> "'. Accepted values: s3"

instance ToText ArtifactStoreType where
  toText = \case
    S3 -> "S3"

instance Hashable ArtifactStoreType

instance NFData ArtifactStoreType

instance ToByteString ArtifactStoreType

instance ToQuery ArtifactStoreType

instance ToHeader ArtifactStoreType

instance ToJSON ArtifactStoreType where
  toJSON = toJSONText

instance FromJSON ArtifactStoreType where
  parseJSON = parseJSONText "ArtifactStoreType"
