{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseMetricName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseMetricName
  ( RelationalDatabaseMetricName
      ( RelationalDatabaseMetricName',
        RelationalDatabaseMetricNameCPUUtilization,
        RelationalDatabaseMetricNameDatabaseConnections,
        RelationalDatabaseMetricNameDiskQueueDepth,
        RelationalDatabaseMetricNameFreeStorageSpace,
        RelationalDatabaseMetricNameNetworkReceiveThroughput,
        RelationalDatabaseMetricNameNetworkTransmitThroughput,
        fromRelationalDatabaseMetricName
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype RelationalDatabaseMetricName = RelationalDatabaseMetricName'
  { fromRelationalDatabaseMetricName ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern RelationalDatabaseMetricNameCPUUtilization :: RelationalDatabaseMetricName
pattern RelationalDatabaseMetricNameCPUUtilization = RelationalDatabaseMetricName' "CPUUtilization"

pattern RelationalDatabaseMetricNameDatabaseConnections :: RelationalDatabaseMetricName
pattern RelationalDatabaseMetricNameDatabaseConnections = RelationalDatabaseMetricName' "DatabaseConnections"

pattern RelationalDatabaseMetricNameDiskQueueDepth :: RelationalDatabaseMetricName
pattern RelationalDatabaseMetricNameDiskQueueDepth = RelationalDatabaseMetricName' "DiskQueueDepth"

pattern RelationalDatabaseMetricNameFreeStorageSpace :: RelationalDatabaseMetricName
pattern RelationalDatabaseMetricNameFreeStorageSpace = RelationalDatabaseMetricName' "FreeStorageSpace"

pattern RelationalDatabaseMetricNameNetworkReceiveThroughput :: RelationalDatabaseMetricName
pattern RelationalDatabaseMetricNameNetworkReceiveThroughput = RelationalDatabaseMetricName' "NetworkReceiveThroughput"

pattern RelationalDatabaseMetricNameNetworkTransmitThroughput :: RelationalDatabaseMetricName
pattern RelationalDatabaseMetricNameNetworkTransmitThroughput = RelationalDatabaseMetricName' "NetworkTransmitThroughput"

{-# COMPLETE
  RelationalDatabaseMetricNameCPUUtilization,
  RelationalDatabaseMetricNameDatabaseConnections,
  RelationalDatabaseMetricNameDiskQueueDepth,
  RelationalDatabaseMetricNameFreeStorageSpace,
  RelationalDatabaseMetricNameNetworkReceiveThroughput,
  RelationalDatabaseMetricNameNetworkTransmitThroughput,
  RelationalDatabaseMetricName'
  #-}
