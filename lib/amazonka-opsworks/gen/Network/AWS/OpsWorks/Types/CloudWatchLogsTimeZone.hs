{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.CloudWatchLogsTimeZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.CloudWatchLogsTimeZone where

import Network.AWS.Prelude

-- | The preferred time zone for logs streamed to CloudWatch Logs. Valid values are @LOCAL@ and @UTC@ , for Coordinated Universal Time.
data CloudWatchLogsTimeZone
  = Local
  | Utc
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

instance FromText CloudWatchLogsTimeZone where
  parser =
    takeLowerText >>= \case
      "local" -> pure Local
      "utc" -> pure Utc
      e ->
        fromTextError $
          "Failure parsing CloudWatchLogsTimeZone from value: '" <> e
            <> "'. Accepted values: local, utc"

instance ToText CloudWatchLogsTimeZone where
  toText = \case
    Local -> "LOCAL"
    Utc -> "UTC"

instance Hashable CloudWatchLogsTimeZone

instance NFData CloudWatchLogsTimeZone

instance ToByteString CloudWatchLogsTimeZone

instance ToQuery CloudWatchLogsTimeZone

instance ToHeader CloudWatchLogsTimeZone

instance ToJSON CloudWatchLogsTimeZone where
  toJSON = toJSONText

instance FromJSON CloudWatchLogsTimeZone where
  parseJSON = parseJSONText "CloudWatchLogsTimeZone"
