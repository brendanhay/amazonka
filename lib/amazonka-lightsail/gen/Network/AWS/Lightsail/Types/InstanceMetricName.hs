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
        IMNCPUUtilization,
        IMNNetworkIn,
        IMNNetworkOut,
        IMNStatusCheckFailed,
        IMNStatusCheckFailedInstance,
        IMNStatusCheckFailedSystem,
        IMNBurstCapacityTime,
        IMNBurstCapacityPercentage
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InstanceMetricName = InstanceMetricName' Lude.Text
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

pattern IMNCPUUtilization :: InstanceMetricName
pattern IMNCPUUtilization = InstanceMetricName' "CPUUtilization"

pattern IMNNetworkIn :: InstanceMetricName
pattern IMNNetworkIn = InstanceMetricName' "NetworkIn"

pattern IMNNetworkOut :: InstanceMetricName
pattern IMNNetworkOut = InstanceMetricName' "NetworkOut"

pattern IMNStatusCheckFailed :: InstanceMetricName
pattern IMNStatusCheckFailed = InstanceMetricName' "StatusCheckFailed"

pattern IMNStatusCheckFailedInstance :: InstanceMetricName
pattern IMNStatusCheckFailedInstance = InstanceMetricName' "StatusCheckFailed_Instance"

pattern IMNStatusCheckFailedSystem :: InstanceMetricName
pattern IMNStatusCheckFailedSystem = InstanceMetricName' "StatusCheckFailed_System"

pattern IMNBurstCapacityTime :: InstanceMetricName
pattern IMNBurstCapacityTime = InstanceMetricName' "BurstCapacityTime"

pattern IMNBurstCapacityPercentage :: InstanceMetricName
pattern IMNBurstCapacityPercentage = InstanceMetricName' "BurstCapacityPercentage"

{-# COMPLETE
  IMNCPUUtilization,
  IMNNetworkIn,
  IMNNetworkOut,
  IMNStatusCheckFailed,
  IMNStatusCheckFailedInstance,
  IMNStatusCheckFailedSystem,
  IMNBurstCapacityTime,
  IMNBurstCapacityPercentage,
  InstanceMetricName'
  #-}
