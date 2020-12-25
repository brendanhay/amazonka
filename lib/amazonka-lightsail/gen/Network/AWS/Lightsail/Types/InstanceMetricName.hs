{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceMetricName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceMetricName
  ( InstanceMetricName
      ( InstanceMetricName',
        InstanceMetricNameCPUUtilization,
        InstanceMetricNameNetworkIn,
        InstanceMetricNameNetworkOut,
        InstanceMetricNameStatusCheckFailed,
        InstanceMetricNameStatusCheckFailedInstance,
        InstanceMetricNameStatusCheckFailedSystem,
        InstanceMetricNameBurstCapacityTime,
        InstanceMetricNameBurstCapacityPercentage,
        fromInstanceMetricName
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype InstanceMetricName = InstanceMetricName'
  { fromInstanceMetricName ::
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

pattern InstanceMetricNameCPUUtilization :: InstanceMetricName
pattern InstanceMetricNameCPUUtilization = InstanceMetricName' "CPUUtilization"

pattern InstanceMetricNameNetworkIn :: InstanceMetricName
pattern InstanceMetricNameNetworkIn = InstanceMetricName' "NetworkIn"

pattern InstanceMetricNameNetworkOut :: InstanceMetricName
pattern InstanceMetricNameNetworkOut = InstanceMetricName' "NetworkOut"

pattern InstanceMetricNameStatusCheckFailed :: InstanceMetricName
pattern InstanceMetricNameStatusCheckFailed = InstanceMetricName' "StatusCheckFailed"

pattern InstanceMetricNameStatusCheckFailedInstance :: InstanceMetricName
pattern InstanceMetricNameStatusCheckFailedInstance = InstanceMetricName' "StatusCheckFailed_Instance"

pattern InstanceMetricNameStatusCheckFailedSystem :: InstanceMetricName
pattern InstanceMetricNameStatusCheckFailedSystem = InstanceMetricName' "StatusCheckFailed_System"

pattern InstanceMetricNameBurstCapacityTime :: InstanceMetricName
pattern InstanceMetricNameBurstCapacityTime = InstanceMetricName' "BurstCapacityTime"

pattern InstanceMetricNameBurstCapacityPercentage :: InstanceMetricName
pattern InstanceMetricNameBurstCapacityPercentage = InstanceMetricName' "BurstCapacityPercentage"

{-# COMPLETE
  InstanceMetricNameCPUUtilization,
  InstanceMetricNameNetworkIn,
  InstanceMetricNameNetworkOut,
  InstanceMetricNameStatusCheckFailed,
  InstanceMetricNameStatusCheckFailedInstance,
  InstanceMetricNameStatusCheckFailedSystem,
  InstanceMetricNameBurstCapacityTime,
  InstanceMetricNameBurstCapacityPercentage,
  InstanceMetricName'
  #-}
