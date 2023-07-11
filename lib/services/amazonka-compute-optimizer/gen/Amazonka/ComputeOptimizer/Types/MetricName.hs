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
-- Module      : Amazonka.ComputeOptimizer.Types.MetricName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.MetricName
  ( MetricName
      ( ..,
        MetricName_Cpu,
        MetricName_DISK_READ_BYTES_PER_SECOND,
        MetricName_DISK_READ_OPS_PER_SECOND,
        MetricName_DISK_WRITE_BYTES_PER_SECOND,
        MetricName_DISK_WRITE_OPS_PER_SECOND,
        MetricName_EBS_READ_BYTES_PER_SECOND,
        MetricName_EBS_READ_OPS_PER_SECOND,
        MetricName_EBS_WRITE_BYTES_PER_SECOND,
        MetricName_EBS_WRITE_OPS_PER_SECOND,
        MetricName_Memory,
        MetricName_NETWORK_IN_BYTES_PER_SECOND,
        MetricName_NETWORK_OUT_BYTES_PER_SECOND,
        MetricName_NETWORK_PACKETS_IN_PER_SECOND,
        MetricName_NETWORK_PACKETS_OUT_PER_SECOND
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

pattern MetricName_Cpu :: MetricName
pattern MetricName_Cpu = MetricName' "Cpu"

pattern MetricName_DISK_READ_BYTES_PER_SECOND :: MetricName
pattern MetricName_DISK_READ_BYTES_PER_SECOND = MetricName' "DISK_READ_BYTES_PER_SECOND"

pattern MetricName_DISK_READ_OPS_PER_SECOND :: MetricName
pattern MetricName_DISK_READ_OPS_PER_SECOND = MetricName' "DISK_READ_OPS_PER_SECOND"

pattern MetricName_DISK_WRITE_BYTES_PER_SECOND :: MetricName
pattern MetricName_DISK_WRITE_BYTES_PER_SECOND = MetricName' "DISK_WRITE_BYTES_PER_SECOND"

pattern MetricName_DISK_WRITE_OPS_PER_SECOND :: MetricName
pattern MetricName_DISK_WRITE_OPS_PER_SECOND = MetricName' "DISK_WRITE_OPS_PER_SECOND"

pattern MetricName_EBS_READ_BYTES_PER_SECOND :: MetricName
pattern MetricName_EBS_READ_BYTES_PER_SECOND = MetricName' "EBS_READ_BYTES_PER_SECOND"

pattern MetricName_EBS_READ_OPS_PER_SECOND :: MetricName
pattern MetricName_EBS_READ_OPS_PER_SECOND = MetricName' "EBS_READ_OPS_PER_SECOND"

pattern MetricName_EBS_WRITE_BYTES_PER_SECOND :: MetricName
pattern MetricName_EBS_WRITE_BYTES_PER_SECOND = MetricName' "EBS_WRITE_BYTES_PER_SECOND"

pattern MetricName_EBS_WRITE_OPS_PER_SECOND :: MetricName
pattern MetricName_EBS_WRITE_OPS_PER_SECOND = MetricName' "EBS_WRITE_OPS_PER_SECOND"

pattern MetricName_Memory :: MetricName
pattern MetricName_Memory = MetricName' "Memory"

pattern MetricName_NETWORK_IN_BYTES_PER_SECOND :: MetricName
pattern MetricName_NETWORK_IN_BYTES_PER_SECOND = MetricName' "NETWORK_IN_BYTES_PER_SECOND"

pattern MetricName_NETWORK_OUT_BYTES_PER_SECOND :: MetricName
pattern MetricName_NETWORK_OUT_BYTES_PER_SECOND = MetricName' "NETWORK_OUT_BYTES_PER_SECOND"

pattern MetricName_NETWORK_PACKETS_IN_PER_SECOND :: MetricName
pattern MetricName_NETWORK_PACKETS_IN_PER_SECOND = MetricName' "NETWORK_PACKETS_IN_PER_SECOND"

pattern MetricName_NETWORK_PACKETS_OUT_PER_SECOND :: MetricName
pattern MetricName_NETWORK_PACKETS_OUT_PER_SECOND = MetricName' "NETWORK_PACKETS_OUT_PER_SECOND"

{-# COMPLETE
  MetricName_Cpu,
  MetricName_DISK_READ_BYTES_PER_SECOND,
  MetricName_DISK_READ_OPS_PER_SECOND,
  MetricName_DISK_WRITE_BYTES_PER_SECOND,
  MetricName_DISK_WRITE_OPS_PER_SECOND,
  MetricName_EBS_READ_BYTES_PER_SECOND,
  MetricName_EBS_READ_OPS_PER_SECOND,
  MetricName_EBS_WRITE_BYTES_PER_SECOND,
  MetricName_EBS_WRITE_OPS_PER_SECOND,
  MetricName_Memory,
  MetricName_NETWORK_IN_BYTES_PER_SECOND,
  MetricName_NETWORK_OUT_BYTES_PER_SECOND,
  MetricName_NETWORK_PACKETS_IN_PER_SECOND,
  MetricName_NETWORK_PACKETS_OUT_PER_SECOND,
  MetricName'
  #-}
