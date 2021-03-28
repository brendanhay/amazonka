{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.MetricName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.MetricName
  ( MetricName
    ( MetricName'
    , MetricNameCPUUtilization
    , MetricNameNetworkIn
    , MetricNameNetworkOut
    , MetricNameStatusCheckFailed
    , MetricNameStatusCheckFailedInstance
    , MetricNameStatusCheckFailedSystem
    , MetricNameClientTLSNegotiationErrorCount
    , MetricNameHealthyHostCount
    , MetricNameUnhealthyHostCount
    , MetricNameHTTPCodeLb4XXCount
    , MetricNameHTTPCodeLb5XXCount
    , MetricNameHTTPCodeInstance2XXCount
    , MetricNameHTTPCodeInstance3XXCount
    , MetricNameHTTPCodeInstance4XXCount
    , MetricNameHTTPCodeInstance5XXCount
    , MetricNameInstanceResponseTime
    , MetricNameRejectedConnectionCount
    , MetricNameRequestCount
    , MetricNameDatabaseConnections
    , MetricNameDiskQueueDepth
    , MetricNameFreeStorageSpace
    , MetricNameNetworkReceiveThroughput
    , MetricNameNetworkTransmitThroughput
    , MetricNameBurstCapacityTime
    , MetricNameBurstCapacityPercentage
    , fromMetricName
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype MetricName = MetricName'{fromMetricName :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern MetricNameCPUUtilization :: MetricName
pattern MetricNameCPUUtilization = MetricName' "CPUUtilization"

pattern MetricNameNetworkIn :: MetricName
pattern MetricNameNetworkIn = MetricName' "NetworkIn"

pattern MetricNameNetworkOut :: MetricName
pattern MetricNameNetworkOut = MetricName' "NetworkOut"

pattern MetricNameStatusCheckFailed :: MetricName
pattern MetricNameStatusCheckFailed = MetricName' "StatusCheckFailed"

pattern MetricNameStatusCheckFailedInstance :: MetricName
pattern MetricNameStatusCheckFailedInstance = MetricName' "StatusCheckFailed_Instance"

pattern MetricNameStatusCheckFailedSystem :: MetricName
pattern MetricNameStatusCheckFailedSystem = MetricName' "StatusCheckFailed_System"

pattern MetricNameClientTLSNegotiationErrorCount :: MetricName
pattern MetricNameClientTLSNegotiationErrorCount = MetricName' "ClientTLSNegotiationErrorCount"

pattern MetricNameHealthyHostCount :: MetricName
pattern MetricNameHealthyHostCount = MetricName' "HealthyHostCount"

pattern MetricNameUnhealthyHostCount :: MetricName
pattern MetricNameUnhealthyHostCount = MetricName' "UnhealthyHostCount"

pattern MetricNameHTTPCodeLb4XXCount :: MetricName
pattern MetricNameHTTPCodeLb4XXCount = MetricName' "HTTPCode_LB_4XX_Count"

pattern MetricNameHTTPCodeLb5XXCount :: MetricName
pattern MetricNameHTTPCodeLb5XXCount = MetricName' "HTTPCode_LB_5XX_Count"

pattern MetricNameHTTPCodeInstance2XXCount :: MetricName
pattern MetricNameHTTPCodeInstance2XXCount = MetricName' "HTTPCode_Instance_2XX_Count"

pattern MetricNameHTTPCodeInstance3XXCount :: MetricName
pattern MetricNameHTTPCodeInstance3XXCount = MetricName' "HTTPCode_Instance_3XX_Count"

pattern MetricNameHTTPCodeInstance4XXCount :: MetricName
pattern MetricNameHTTPCodeInstance4XXCount = MetricName' "HTTPCode_Instance_4XX_Count"

pattern MetricNameHTTPCodeInstance5XXCount :: MetricName
pattern MetricNameHTTPCodeInstance5XXCount = MetricName' "HTTPCode_Instance_5XX_Count"

pattern MetricNameInstanceResponseTime :: MetricName
pattern MetricNameInstanceResponseTime = MetricName' "InstanceResponseTime"

pattern MetricNameRejectedConnectionCount :: MetricName
pattern MetricNameRejectedConnectionCount = MetricName' "RejectedConnectionCount"

pattern MetricNameRequestCount :: MetricName
pattern MetricNameRequestCount = MetricName' "RequestCount"

pattern MetricNameDatabaseConnections :: MetricName
pattern MetricNameDatabaseConnections = MetricName' "DatabaseConnections"

pattern MetricNameDiskQueueDepth :: MetricName
pattern MetricNameDiskQueueDepth = MetricName' "DiskQueueDepth"

pattern MetricNameFreeStorageSpace :: MetricName
pattern MetricNameFreeStorageSpace = MetricName' "FreeStorageSpace"

pattern MetricNameNetworkReceiveThroughput :: MetricName
pattern MetricNameNetworkReceiveThroughput = MetricName' "NetworkReceiveThroughput"

pattern MetricNameNetworkTransmitThroughput :: MetricName
pattern MetricNameNetworkTransmitThroughput = MetricName' "NetworkTransmitThroughput"

pattern MetricNameBurstCapacityTime :: MetricName
pattern MetricNameBurstCapacityTime = MetricName' "BurstCapacityTime"

pattern MetricNameBurstCapacityPercentage :: MetricName
pattern MetricNameBurstCapacityPercentage = MetricName' "BurstCapacityPercentage"

{-# COMPLETE 
  MetricNameCPUUtilization,

  MetricNameNetworkIn,

  MetricNameNetworkOut,

  MetricNameStatusCheckFailed,

  MetricNameStatusCheckFailedInstance,

  MetricNameStatusCheckFailedSystem,

  MetricNameClientTLSNegotiationErrorCount,

  MetricNameHealthyHostCount,

  MetricNameUnhealthyHostCount,

  MetricNameHTTPCodeLb4XXCount,

  MetricNameHTTPCodeLb5XXCount,

  MetricNameHTTPCodeInstance2XXCount,

  MetricNameHTTPCodeInstance3XXCount,

  MetricNameHTTPCodeInstance4XXCount,

  MetricNameHTTPCodeInstance5XXCount,

  MetricNameInstanceResponseTime,

  MetricNameRejectedConnectionCount,

  MetricNameRequestCount,

  MetricNameDatabaseConnections,

  MetricNameDiskQueueDepth,

  MetricNameFreeStorageSpace,

  MetricNameNetworkReceiveThroughput,

  MetricNameNetworkTransmitThroughput,

  MetricNameBurstCapacityTime,

  MetricNameBurstCapacityPercentage,
  MetricName'
  #-}
