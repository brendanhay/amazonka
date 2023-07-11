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
-- Module      : Amazonka.Lightsail.Types.RelationalDatabaseMetricName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.RelationalDatabaseMetricName
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RelationalDatabaseMetricName = RelationalDatabaseMetricName'
  { fromRelationalDatabaseMetricName ::
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
