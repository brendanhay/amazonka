{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.MetricType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.MetricType
  ( MetricType
      ( MetricType',
        MetricTypeASGAverageCPUUtilization,
        MetricTypeASGAverageNetworkIn,
        MetricTypeASGAverageNetworkOut,
        MetricTypeALBRequestCountPerTarget,
        fromMetricType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype MetricType = MetricType' {fromMetricType :: Core.Text}
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

pattern MetricTypeASGAverageCPUUtilization :: MetricType
pattern MetricTypeASGAverageCPUUtilization = MetricType' "ASGAverageCPUUtilization"

pattern MetricTypeASGAverageNetworkIn :: MetricType
pattern MetricTypeASGAverageNetworkIn = MetricType' "ASGAverageNetworkIn"

pattern MetricTypeASGAverageNetworkOut :: MetricType
pattern MetricTypeASGAverageNetworkOut = MetricType' "ASGAverageNetworkOut"

pattern MetricTypeALBRequestCountPerTarget :: MetricType
pattern MetricTypeALBRequestCountPerTarget = MetricType' "ALBRequestCountPerTarget"

{-# COMPLETE
  MetricTypeASGAverageCPUUtilization,
  MetricTypeASGAverageNetworkIn,
  MetricTypeASGAverageNetworkOut,
  MetricTypeALBRequestCountPerTarget,
  MetricType'
  #-}
