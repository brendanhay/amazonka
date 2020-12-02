{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.LoggerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.LoggerType where

import Network.AWS.Prelude

data LoggerType
  = AWSCloudWatch
  | FileSystem
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

instance FromText LoggerType where
  parser =
    takeLowerText >>= \case
      "awscloudwatch" -> pure AWSCloudWatch
      "filesystem" -> pure FileSystem
      e ->
        fromTextError $
          "Failure parsing LoggerType from value: '" <> e
            <> "'. Accepted values: awscloudwatch, filesystem"

instance ToText LoggerType where
  toText = \case
    AWSCloudWatch -> "AWSCloudWatch"
    FileSystem -> "FileSystem"

instance Hashable LoggerType

instance NFData LoggerType

instance ToByteString LoggerType

instance ToQuery LoggerType

instance ToHeader LoggerType

instance ToJSON LoggerType where
  toJSON = toJSONText

instance FromJSON LoggerType where
  parseJSON = parseJSONText "LoggerType"
