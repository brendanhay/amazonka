{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.MetricName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.MetricName
  ( MetricName
      ( MetricName',
        CPUUtilization,
        NetworkIn,
        NetworkOut,
        StatusCheckFailed,
        StatusCheckFailedInstance,
        StatusCheckFailedSystem,
        ClientTLSNegotiationErrorCount,
        HealthyHostCount,
        UnhealthyHostCount,
        HTTPCodeLb4XXCount,
        HTTPCodeLb5XXCount,
        HTTPCodeInstance2XXCount,
        HTTPCodeInstance3XXCount,
        HTTPCodeInstance4XXCount,
        HTTPCodeInstance5XXCount,
        InstanceResponseTime,
        RejectedConnectionCount,
        RequestCount,
        DatabaseConnections,
        DiskQueueDepth,
        FreeStorageSpace,
        NetworkReceiveThroughput,
        NetworkTransmitThroughput,
        BurstCapacityTime,
        BurstCapacityPercentage
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MetricName = MetricName' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CPUUtilization :: MetricName
pattern CPUUtilization = MetricName' "CPUUtilization"

pattern NetworkIn :: MetricName
pattern NetworkIn = MetricName' "NetworkIn"

pattern NetworkOut :: MetricName
pattern NetworkOut = MetricName' "NetworkOut"

pattern StatusCheckFailed :: MetricName
pattern StatusCheckFailed = MetricName' "StatusCheckFailed"

pattern StatusCheckFailedInstance :: MetricName
pattern StatusCheckFailedInstance = MetricName' "StatusCheckFailed_Instance"

pattern StatusCheckFailedSystem :: MetricName
pattern StatusCheckFailedSystem = MetricName' "StatusCheckFailed_System"

pattern ClientTLSNegotiationErrorCount :: MetricName
pattern ClientTLSNegotiationErrorCount = MetricName' "ClientTLSNegotiationErrorCount"

pattern HealthyHostCount :: MetricName
pattern HealthyHostCount = MetricName' "HealthyHostCount"

pattern UnhealthyHostCount :: MetricName
pattern UnhealthyHostCount = MetricName' "UnhealthyHostCount"

pattern HTTPCodeLb4XXCount :: MetricName
pattern HTTPCodeLb4XXCount = MetricName' "HTTPCode_LB_4XX_Count"

pattern HTTPCodeLb5XXCount :: MetricName
pattern HTTPCodeLb5XXCount = MetricName' "HTTPCode_LB_5XX_Count"

pattern HTTPCodeInstance2XXCount :: MetricName
pattern HTTPCodeInstance2XXCount = MetricName' "HTTPCode_Instance_2XX_Count"

pattern HTTPCodeInstance3XXCount :: MetricName
pattern HTTPCodeInstance3XXCount = MetricName' "HTTPCode_Instance_3XX_Count"

pattern HTTPCodeInstance4XXCount :: MetricName
pattern HTTPCodeInstance4XXCount = MetricName' "HTTPCode_Instance_4XX_Count"

pattern HTTPCodeInstance5XXCount :: MetricName
pattern HTTPCodeInstance5XXCount = MetricName' "HTTPCode_Instance_5XX_Count"

pattern InstanceResponseTime :: MetricName
pattern InstanceResponseTime = MetricName' "InstanceResponseTime"

pattern RejectedConnectionCount :: MetricName
pattern RejectedConnectionCount = MetricName' "RejectedConnectionCount"

pattern RequestCount :: MetricName
pattern RequestCount = MetricName' "RequestCount"

pattern DatabaseConnections :: MetricName
pattern DatabaseConnections = MetricName' "DatabaseConnections"

pattern DiskQueueDepth :: MetricName
pattern DiskQueueDepth = MetricName' "DiskQueueDepth"

pattern FreeStorageSpace :: MetricName
pattern FreeStorageSpace = MetricName' "FreeStorageSpace"

pattern NetworkReceiveThroughput :: MetricName
pattern NetworkReceiveThroughput = MetricName' "NetworkReceiveThroughput"

pattern NetworkTransmitThroughput :: MetricName
pattern NetworkTransmitThroughput = MetricName' "NetworkTransmitThroughput"

pattern BurstCapacityTime :: MetricName
pattern BurstCapacityTime = MetricName' "BurstCapacityTime"

pattern BurstCapacityPercentage :: MetricName
pattern BurstCapacityPercentage = MetricName' "BurstCapacityPercentage"

{-# COMPLETE
  CPUUtilization,
  NetworkIn,
  NetworkOut,
  StatusCheckFailed,
  StatusCheckFailedInstance,
  StatusCheckFailedSystem,
  ClientTLSNegotiationErrorCount,
  HealthyHostCount,
  UnhealthyHostCount,
  HTTPCodeLb4XXCount,
  HTTPCodeLb5XXCount,
  HTTPCodeInstance2XXCount,
  HTTPCodeInstance3XXCount,
  HTTPCodeInstance4XXCount,
  HTTPCodeInstance5XXCount,
  InstanceResponseTime,
  RejectedConnectionCount,
  RequestCount,
  DatabaseConnections,
  DiskQueueDepth,
  FreeStorageSpace,
  NetworkReceiveThroughput,
  NetworkTransmitThroughput,
  BurstCapacityTime,
  BurstCapacityPercentage,
  MetricName'
  #-}
