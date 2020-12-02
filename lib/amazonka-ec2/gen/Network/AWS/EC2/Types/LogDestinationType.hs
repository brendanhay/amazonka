{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LogDestinationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LogDestinationType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data LogDestinationType
  = CloudWatchLogs
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

instance FromText LogDestinationType where
  parser =
    takeLowerText >>= \case
      "cloud-watch-logs" -> pure CloudWatchLogs
      "s3" -> pure S3
      e ->
        fromTextError $
          "Failure parsing LogDestinationType from value: '" <> e
            <> "'. Accepted values: cloud-watch-logs, s3"

instance ToText LogDestinationType where
  toText = \case
    CloudWatchLogs -> "cloud-watch-logs"
    S3 -> "s3"

instance Hashable LogDestinationType

instance NFData LogDestinationType

instance ToByteString LogDestinationType

instance ToQuery LogDestinationType

instance ToHeader LogDestinationType

instance FromXML LogDestinationType where
  parseXML = parseXMLText "LogDestinationType"
