{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.S3DataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.S3DataType where

import Network.AWS.Prelude

data S3DataType
  = AugmentedManifestFile
  | ManifestFile
  | S3Prefix
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

instance FromText S3DataType where
  parser =
    takeLowerText >>= \case
      "augmentedmanifestfile" -> pure AugmentedManifestFile
      "manifestfile" -> pure ManifestFile
      "s3prefix" -> pure S3Prefix
      e ->
        fromTextError $
          "Failure parsing S3DataType from value: '" <> e
            <> "'. Accepted values: augmentedmanifestfile, manifestfile, s3prefix"

instance ToText S3DataType where
  toText = \case
    AugmentedManifestFile -> "AugmentedManifestFile"
    ManifestFile -> "ManifestFile"
    S3Prefix -> "S3Prefix"

instance Hashable S3DataType

instance NFData S3DataType

instance ToByteString S3DataType

instance ToQuery S3DataType

instance ToHeader S3DataType

instance ToJSON S3DataType where
  toJSON = toJSONText

instance FromJSON S3DataType where
  parseJSON = parseJSONText "S3DataType"
