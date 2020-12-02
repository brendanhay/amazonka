{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3DataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingS3DataType where

import Network.AWS.Prelude

data ProcessingS3DataType
  = PSDTManifestFile
  | PSDTS3Prefix
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

instance FromText ProcessingS3DataType where
  parser =
    takeLowerText >>= \case
      "manifestfile" -> pure PSDTManifestFile
      "s3prefix" -> pure PSDTS3Prefix
      e ->
        fromTextError $
          "Failure parsing ProcessingS3DataType from value: '" <> e
            <> "'. Accepted values: manifestfile, s3prefix"

instance ToText ProcessingS3DataType where
  toText = \case
    PSDTManifestFile -> "ManifestFile"
    PSDTS3Prefix -> "S3Prefix"

instance Hashable ProcessingS3DataType

instance NFData ProcessingS3DataType

instance ToByteString ProcessingS3DataType

instance ToQuery ProcessingS3DataType

instance ToHeader ProcessingS3DataType

instance ToJSON ProcessingS3DataType where
  toJSON = toJSONText

instance FromJSON ProcessingS3DataType where
  parseJSON = parseJSONText "ProcessingS3DataType"
