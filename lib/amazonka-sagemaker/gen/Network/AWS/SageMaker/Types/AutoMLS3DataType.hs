{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLS3DataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLS3DataType where

import Network.AWS.Prelude

data AutoMLS3DataType
  = AMLSDTManifestFile
  | AMLSDTS3Prefix
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

instance FromText AutoMLS3DataType where
  parser =
    takeLowerText >>= \case
      "manifestfile" -> pure AMLSDTManifestFile
      "s3prefix" -> pure AMLSDTS3Prefix
      e ->
        fromTextError $
          "Failure parsing AutoMLS3DataType from value: '" <> e
            <> "'. Accepted values: manifestfile, s3prefix"

instance ToText AutoMLS3DataType where
  toText = \case
    AMLSDTManifestFile -> "ManifestFile"
    AMLSDTS3Prefix -> "S3Prefix"

instance Hashable AutoMLS3DataType

instance NFData AutoMLS3DataType

instance ToByteString AutoMLS3DataType

instance ToQuery AutoMLS3DataType

instance ToHeader AutoMLS3DataType

instance ToJSON AutoMLS3DataType where
  toJSON = toJSONText

instance FromJSON AutoMLS3DataType where
  parseJSON = parseJSONText "AutoMLS3DataType"
