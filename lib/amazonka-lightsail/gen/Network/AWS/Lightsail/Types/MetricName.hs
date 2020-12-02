{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.MetricName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.MetricName where

import Network.AWS.Prelude

data MetricName
  = MNBurstCapacityPercentage
  | MNBurstCapacityTime
  | MNCPUUtilization
  | MNClientTLSNegotiationErrorCount
  | MNDatabaseConnections
  | MNDiskQueueDepth
  | MNFreeStorageSpace
  | MNHTTPCodeInstance2XXCount
  | MNHTTPCodeInstance3XXCount
  | MNHTTPCodeInstance4XXCount
  | MNHTTPCodeInstance5XXCount
  | MNHTTPCodeLb4XXCount
  | MNHTTPCodeLb5XXCount
  | MNHealthyHostCount
  | MNInstanceResponseTime
  | MNNetworkIn
  | MNNetworkOut
  | MNNetworkReceiveThroughput
  | MNNetworkTransmitThroughput
  | MNRejectedConnectionCount
  | MNRequestCount
  | MNStatusCheckFailed
  | MNStatusCheckFailedInstance
  | MNStatusCheckFailedSystem
  | MNUnhealthyHostCount
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

instance FromText MetricName where
  parser =
    takeLowerText >>= \case
      "burstcapacitypercentage" -> pure MNBurstCapacityPercentage
      "burstcapacitytime" -> pure MNBurstCapacityTime
      "cpuutilization" -> pure MNCPUUtilization
      "clienttlsnegotiationerrorcount" -> pure MNClientTLSNegotiationErrorCount
      "databaseconnections" -> pure MNDatabaseConnections
      "diskqueuedepth" -> pure MNDiskQueueDepth
      "freestoragespace" -> pure MNFreeStorageSpace
      "httpcode_instance_2xx_count" -> pure MNHTTPCodeInstance2XXCount
      "httpcode_instance_3xx_count" -> pure MNHTTPCodeInstance3XXCount
      "httpcode_instance_4xx_count" -> pure MNHTTPCodeInstance4XXCount
      "httpcode_instance_5xx_count" -> pure MNHTTPCodeInstance5XXCount
      "httpcode_lb_4xx_count" -> pure MNHTTPCodeLb4XXCount
      "httpcode_lb_5xx_count" -> pure MNHTTPCodeLb5XXCount
      "healthyhostcount" -> pure MNHealthyHostCount
      "instanceresponsetime" -> pure MNInstanceResponseTime
      "networkin" -> pure MNNetworkIn
      "networkout" -> pure MNNetworkOut
      "networkreceivethroughput" -> pure MNNetworkReceiveThroughput
      "networktransmitthroughput" -> pure MNNetworkTransmitThroughput
      "rejectedconnectioncount" -> pure MNRejectedConnectionCount
      "requestcount" -> pure MNRequestCount
      "statuscheckfailed" -> pure MNStatusCheckFailed
      "statuscheckfailed_instance" -> pure MNStatusCheckFailedInstance
      "statuscheckfailed_system" -> pure MNStatusCheckFailedSystem
      "unhealthyhostcount" -> pure MNUnhealthyHostCount
      e ->
        fromTextError $
          "Failure parsing MetricName from value: '" <> e
            <> "'. Accepted values: burstcapacitypercentage, burstcapacitytime, cpuutilization, clienttlsnegotiationerrorcount, databaseconnections, diskqueuedepth, freestoragespace, httpcode_instance_2xx_count, httpcode_instance_3xx_count, httpcode_instance_4xx_count, httpcode_instance_5xx_count, httpcode_lb_4xx_count, httpcode_lb_5xx_count, healthyhostcount, instanceresponsetime, networkin, networkout, networkreceivethroughput, networktransmitthroughput, rejectedconnectioncount, requestcount, statuscheckfailed, statuscheckfailed_instance, statuscheckfailed_system, unhealthyhostcount"

instance ToText MetricName where
  toText = \case
    MNBurstCapacityPercentage -> "BurstCapacityPercentage"
    MNBurstCapacityTime -> "BurstCapacityTime"
    MNCPUUtilization -> "CPUUtilization"
    MNClientTLSNegotiationErrorCount -> "ClientTLSNegotiationErrorCount"
    MNDatabaseConnections -> "DatabaseConnections"
    MNDiskQueueDepth -> "DiskQueueDepth"
    MNFreeStorageSpace -> "FreeStorageSpace"
    MNHTTPCodeInstance2XXCount -> "HTTPCode_Instance_2XX_Count"
    MNHTTPCodeInstance3XXCount -> "HTTPCode_Instance_3XX_Count"
    MNHTTPCodeInstance4XXCount -> "HTTPCode_Instance_4XX_Count"
    MNHTTPCodeInstance5XXCount -> "HTTPCode_Instance_5XX_Count"
    MNHTTPCodeLb4XXCount -> "HTTPCode_LB_4XX_Count"
    MNHTTPCodeLb5XXCount -> "HTTPCode_LB_5XX_Count"
    MNHealthyHostCount -> "HealthyHostCount"
    MNInstanceResponseTime -> "InstanceResponseTime"
    MNNetworkIn -> "NetworkIn"
    MNNetworkOut -> "NetworkOut"
    MNNetworkReceiveThroughput -> "NetworkReceiveThroughput"
    MNNetworkTransmitThroughput -> "NetworkTransmitThroughput"
    MNRejectedConnectionCount -> "RejectedConnectionCount"
    MNRequestCount -> "RequestCount"
    MNStatusCheckFailed -> "StatusCheckFailed"
    MNStatusCheckFailedInstance -> "StatusCheckFailed_Instance"
    MNStatusCheckFailedSystem -> "StatusCheckFailed_System"
    MNUnhealthyHostCount -> "UnhealthyHostCount"

instance Hashable MetricName

instance NFData MetricName

instance ToByteString MetricName

instance ToQuery MetricName

instance ToHeader MetricName

instance ToJSON MetricName where
  toJSON = toJSONText

instance FromJSON MetricName where
  parseJSON = parseJSONText "MetricName"
