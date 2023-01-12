{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Lightsail.Types.MetricName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.MetricName
  ( MetricName
      ( ..,
        MetricName_BurstCapacityPercentage,
        MetricName_BurstCapacityTime,
        MetricName_CPUUtilization,
        MetricName_ClientTLSNegotiationErrorCount,
        MetricName_DatabaseConnections,
        MetricName_DiskQueueDepth,
        MetricName_FreeStorageSpace,
        MetricName_HTTPCode_Instance_2XX_Count,
        MetricName_HTTPCode_Instance_3XX_Count,
        MetricName_HTTPCode_Instance_4XX_Count,
        MetricName_HTTPCode_Instance_5XX_Count,
        MetricName_HTTPCode_LB_4XX_Count,
        MetricName_HTTPCode_LB_5XX_Count,
        MetricName_HealthyHostCount,
        MetricName_InstanceResponseTime,
        MetricName_NetworkIn,
        MetricName_NetworkOut,
        MetricName_NetworkReceiveThroughput,
        MetricName_NetworkTransmitThroughput,
        MetricName_RejectedConnectionCount,
        MetricName_RequestCount,
        MetricName_StatusCheckFailed,
        MetricName_StatusCheckFailed_Instance,
        MetricName_StatusCheckFailed_System,
        MetricName_UnhealthyHostCount
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MetricName = MetricName'
  { fromMetricName ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern MetricName_BurstCapacityPercentage :: MetricName
pattern MetricName_BurstCapacityPercentage = MetricName' "BurstCapacityPercentage"

pattern MetricName_BurstCapacityTime :: MetricName
pattern MetricName_BurstCapacityTime = MetricName' "BurstCapacityTime"

pattern MetricName_CPUUtilization :: MetricName
pattern MetricName_CPUUtilization = MetricName' "CPUUtilization"

pattern MetricName_ClientTLSNegotiationErrorCount :: MetricName
pattern MetricName_ClientTLSNegotiationErrorCount = MetricName' "ClientTLSNegotiationErrorCount"

pattern MetricName_DatabaseConnections :: MetricName
pattern MetricName_DatabaseConnections = MetricName' "DatabaseConnections"

pattern MetricName_DiskQueueDepth :: MetricName
pattern MetricName_DiskQueueDepth = MetricName' "DiskQueueDepth"

pattern MetricName_FreeStorageSpace :: MetricName
pattern MetricName_FreeStorageSpace = MetricName' "FreeStorageSpace"

pattern MetricName_HTTPCode_Instance_2XX_Count :: MetricName
pattern MetricName_HTTPCode_Instance_2XX_Count = MetricName' "HTTPCode_Instance_2XX_Count"

pattern MetricName_HTTPCode_Instance_3XX_Count :: MetricName
pattern MetricName_HTTPCode_Instance_3XX_Count = MetricName' "HTTPCode_Instance_3XX_Count"

pattern MetricName_HTTPCode_Instance_4XX_Count :: MetricName
pattern MetricName_HTTPCode_Instance_4XX_Count = MetricName' "HTTPCode_Instance_4XX_Count"

pattern MetricName_HTTPCode_Instance_5XX_Count :: MetricName
pattern MetricName_HTTPCode_Instance_5XX_Count = MetricName' "HTTPCode_Instance_5XX_Count"

pattern MetricName_HTTPCode_LB_4XX_Count :: MetricName
pattern MetricName_HTTPCode_LB_4XX_Count = MetricName' "HTTPCode_LB_4XX_Count"

pattern MetricName_HTTPCode_LB_5XX_Count :: MetricName
pattern MetricName_HTTPCode_LB_5XX_Count = MetricName' "HTTPCode_LB_5XX_Count"

pattern MetricName_HealthyHostCount :: MetricName
pattern MetricName_HealthyHostCount = MetricName' "HealthyHostCount"

pattern MetricName_InstanceResponseTime :: MetricName
pattern MetricName_InstanceResponseTime = MetricName' "InstanceResponseTime"

pattern MetricName_NetworkIn :: MetricName
pattern MetricName_NetworkIn = MetricName' "NetworkIn"

pattern MetricName_NetworkOut :: MetricName
pattern MetricName_NetworkOut = MetricName' "NetworkOut"

pattern MetricName_NetworkReceiveThroughput :: MetricName
pattern MetricName_NetworkReceiveThroughput = MetricName' "NetworkReceiveThroughput"

pattern MetricName_NetworkTransmitThroughput :: MetricName
pattern MetricName_NetworkTransmitThroughput = MetricName' "NetworkTransmitThroughput"

pattern MetricName_RejectedConnectionCount :: MetricName
pattern MetricName_RejectedConnectionCount = MetricName' "RejectedConnectionCount"

pattern MetricName_RequestCount :: MetricName
pattern MetricName_RequestCount = MetricName' "RequestCount"

pattern MetricName_StatusCheckFailed :: MetricName
pattern MetricName_StatusCheckFailed = MetricName' "StatusCheckFailed"

pattern MetricName_StatusCheckFailed_Instance :: MetricName
pattern MetricName_StatusCheckFailed_Instance = MetricName' "StatusCheckFailed_Instance"

pattern MetricName_StatusCheckFailed_System :: MetricName
pattern MetricName_StatusCheckFailed_System = MetricName' "StatusCheckFailed_System"

pattern MetricName_UnhealthyHostCount :: MetricName
pattern MetricName_UnhealthyHostCount = MetricName' "UnhealthyHostCount"

{-# COMPLETE
  MetricName_BurstCapacityPercentage,
  MetricName_BurstCapacityTime,
  MetricName_CPUUtilization,
  MetricName_ClientTLSNegotiationErrorCount,
  MetricName_DatabaseConnections,
  MetricName_DiskQueueDepth,
  MetricName_FreeStorageSpace,
  MetricName_HTTPCode_Instance_2XX_Count,
  MetricName_HTTPCode_Instance_3XX_Count,
  MetricName_HTTPCode_Instance_4XX_Count,
  MetricName_HTTPCode_Instance_5XX_Count,
  MetricName_HTTPCode_LB_4XX_Count,
  MetricName_HTTPCode_LB_5XX_Count,
  MetricName_HealthyHostCount,
  MetricName_InstanceResponseTime,
  MetricName_NetworkIn,
  MetricName_NetworkOut,
  MetricName_NetworkReceiveThroughput,
  MetricName_NetworkTransmitThroughput,
  MetricName_RejectedConnectionCount,
  MetricName_RequestCount,
  MetricName_StatusCheckFailed,
  MetricName_StatusCheckFailed_Instance,
  MetricName_StatusCheckFailed_System,
  MetricName_UnhealthyHostCount,
  MetricName'
  #-}
