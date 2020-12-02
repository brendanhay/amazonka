{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseMetricName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseMetricName where

import Network.AWS.Prelude

data RelationalDatabaseMetricName
  = CPUUtilization
  | DatabaseConnections
  | DiskQueueDepth
  | FreeStorageSpace
  | NetworkReceiveThroughput
  | NetworkTransmitThroughput
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

instance FromText RelationalDatabaseMetricName where
  parser =
    takeLowerText >>= \case
      "cpuutilization" -> pure CPUUtilization
      "databaseconnections" -> pure DatabaseConnections
      "diskqueuedepth" -> pure DiskQueueDepth
      "freestoragespace" -> pure FreeStorageSpace
      "networkreceivethroughput" -> pure NetworkReceiveThroughput
      "networktransmitthroughput" -> pure NetworkTransmitThroughput
      e ->
        fromTextError $
          "Failure parsing RelationalDatabaseMetricName from value: '" <> e
            <> "'. Accepted values: cpuutilization, databaseconnections, diskqueuedepth, freestoragespace, networkreceivethroughput, networktransmitthroughput"

instance ToText RelationalDatabaseMetricName where
  toText = \case
    CPUUtilization -> "CPUUtilization"
    DatabaseConnections -> "DatabaseConnections"
    DiskQueueDepth -> "DiskQueueDepth"
    FreeStorageSpace -> "FreeStorageSpace"
    NetworkReceiveThroughput -> "NetworkReceiveThroughput"
    NetworkTransmitThroughput -> "NetworkTransmitThroughput"

instance Hashable RelationalDatabaseMetricName

instance NFData RelationalDatabaseMetricName

instance ToByteString RelationalDatabaseMetricName

instance ToQuery RelationalDatabaseMetricName

instance ToHeader RelationalDatabaseMetricName

instance ToJSON RelationalDatabaseMetricName where
  toJSON = toJSONText

instance FromJSON RelationalDatabaseMetricName where
  parseJSON = parseJSONText "RelationalDatabaseMetricName"
