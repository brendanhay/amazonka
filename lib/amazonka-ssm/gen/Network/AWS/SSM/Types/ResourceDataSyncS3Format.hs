{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncS3Format
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceDataSyncS3Format where

import Network.AWS.Prelude

data ResourceDataSyncS3Format = JSONSerDe
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

instance FromText ResourceDataSyncS3Format where
  parser =
    takeLowerText >>= \case
      "jsonserde" -> pure JSONSerDe
      e ->
        fromTextError $
          "Failure parsing ResourceDataSyncS3Format from value: '" <> e
            <> "'. Accepted values: jsonserde"

instance ToText ResourceDataSyncS3Format where
  toText = \case
    JSONSerDe -> "JsonSerDe"

instance Hashable ResourceDataSyncS3Format

instance NFData ResourceDataSyncS3Format

instance ToByteString ResourceDataSyncS3Format

instance ToQuery ResourceDataSyncS3Format

instance ToHeader ResourceDataSyncS3Format

instance ToJSON ResourceDataSyncS3Format where
  toJSON = toJSONText

instance FromJSON ResourceDataSyncS3Format where
  parseJSON = parseJSONText "ResourceDataSyncS3Format"
