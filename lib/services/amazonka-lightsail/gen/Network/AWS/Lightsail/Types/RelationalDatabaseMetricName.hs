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
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseMetricName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseMetricName
  ( RelationalDatabaseMetricName
      ( ..,
        RelationalDatabaseMetricName_CPUUtilization,
        RelationalDatabaseMetricName_DatabaseConnections,
        RelationalDatabaseMetricName_DiskQueueDepth,
        RelationalDatabaseMetricName_FreeStorageSpace,
        RelationalDatabaseMetricName_NetworkReceiveThroughput,
        RelationalDatabaseMetricName_NetworkTransmitThroughput
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype RelationalDatabaseMetricName = RelationalDatabaseMetricName'
  { fromRelationalDatabaseMetricName ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern RelationalDatabaseMetricName_CPUUtilization :: RelationalDatabaseMetricName
pattern RelationalDatabaseMetricName_CPUUtilization = RelationalDatabaseMetricName' "CPUUtilization"

pattern RelationalDatabaseMetricName_DatabaseConnections :: RelationalDatabaseMetricName
pattern RelationalDatabaseMetricName_DatabaseConnections = RelationalDatabaseMetricName' "DatabaseConnections"

pattern RelationalDatabaseMetricName_DiskQueueDepth :: RelationalDatabaseMetricName
pattern RelationalDatabaseMetricName_DiskQueueDepth = RelationalDatabaseMetricName' "DiskQueueDepth"

pattern RelationalDatabaseMetricName_FreeStorageSpace :: RelationalDatabaseMetricName
pattern RelationalDatabaseMetricName_FreeStorageSpace = RelationalDatabaseMetricName' "FreeStorageSpace"

pattern RelationalDatabaseMetricName_NetworkReceiveThroughput :: RelationalDatabaseMetricName
pattern RelationalDatabaseMetricName_NetworkReceiveThroughput = RelationalDatabaseMetricName' "NetworkReceiveThroughput"

pattern RelationalDatabaseMetricName_NetworkTransmitThroughput :: RelationalDatabaseMetricName
pattern RelationalDatabaseMetricName_NetworkTransmitThroughput = RelationalDatabaseMetricName' "NetworkTransmitThroughput"

{-# COMPLETE
  RelationalDatabaseMetricName_CPUUtilization,
  RelationalDatabaseMetricName_DatabaseConnections,
  RelationalDatabaseMetricName_DiskQueueDepth,
  RelationalDatabaseMetricName_FreeStorageSpace,
  RelationalDatabaseMetricName_NetworkReceiveThroughput,
  RelationalDatabaseMetricName_NetworkTransmitThroughput,
  RelationalDatabaseMetricName'
  #-}
