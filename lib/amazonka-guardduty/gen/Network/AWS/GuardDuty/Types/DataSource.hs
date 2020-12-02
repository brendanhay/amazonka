{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DataSource where

import Network.AWS.Prelude

data DataSource
  = CloudTrail
  | DNSLogs
  | FlowLogs
  | S3Logs
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

instance FromText DataSource where
  parser =
    takeLowerText >>= \case
      "cloud_trail" -> pure CloudTrail
      "dns_logs" -> pure DNSLogs
      "flow_logs" -> pure FlowLogs
      "s3_logs" -> pure S3Logs
      e ->
        fromTextError $
          "Failure parsing DataSource from value: '" <> e
            <> "'. Accepted values: cloud_trail, dns_logs, flow_logs, s3_logs"

instance ToText DataSource where
  toText = \case
    CloudTrail -> "CLOUD_TRAIL"
    DNSLogs -> "DNS_LOGS"
    FlowLogs -> "FLOW_LOGS"
    S3Logs -> "S3_LOGS"

instance Hashable DataSource

instance NFData DataSource

instance ToByteString DataSource

instance ToQuery DataSource

instance ToHeader DataSource

instance ToJSON DataSource where
  toJSON = toJSONText

instance FromJSON DataSource where
  parseJSON = parseJSONText "DataSource"
