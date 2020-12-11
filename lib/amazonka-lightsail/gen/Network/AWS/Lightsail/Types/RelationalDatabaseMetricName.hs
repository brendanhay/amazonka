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
        CPUUtilization,
        DatabaseConnections,
        DiskQueueDepth,
        FreeStorageSpace,
        NetworkReceiveThroughput,
        NetworkTransmitThroughput
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype RelationalDatabaseMetricName = RelationalDatabaseMetricName' Lude.Text
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

pattern CPUUtilization :: RelationalDatabaseMetricName
pattern CPUUtilization = RelationalDatabaseMetricName' "CPUUtilization"

pattern DatabaseConnections :: RelationalDatabaseMetricName
pattern DatabaseConnections = RelationalDatabaseMetricName' "DatabaseConnections"

pattern DiskQueueDepth :: RelationalDatabaseMetricName
pattern DiskQueueDepth = RelationalDatabaseMetricName' "DiskQueueDepth"

pattern FreeStorageSpace :: RelationalDatabaseMetricName
pattern FreeStorageSpace = RelationalDatabaseMetricName' "FreeStorageSpace"

pattern NetworkReceiveThroughput :: RelationalDatabaseMetricName
pattern NetworkReceiveThroughput = RelationalDatabaseMetricName' "NetworkReceiveThroughput"

pattern NetworkTransmitThroughput :: RelationalDatabaseMetricName
pattern NetworkTransmitThroughput = RelationalDatabaseMetricName' "NetworkTransmitThroughput"

{-# COMPLETE
  CPUUtilization,
  DatabaseConnections,
  DiskQueueDepth,
  FreeStorageSpace,
  NetworkReceiveThroughput,
  NetworkTransmitThroughput,
  RelationalDatabaseMetricName'
  #-}
