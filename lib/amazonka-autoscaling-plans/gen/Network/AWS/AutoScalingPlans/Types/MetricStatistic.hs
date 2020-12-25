{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.MetricStatistic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.MetricStatistic
  ( MetricStatistic
      ( MetricStatistic',
        MetricStatisticAverage,
        MetricStatisticMinimum,
        MetricStatisticMaximum,
        MetricStatisticSampleCount,
        MetricStatisticSum,
        fromMetricStatistic
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype MetricStatistic = MetricStatistic'
  { fromMetricStatistic ::
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

pattern MetricStatisticAverage :: MetricStatistic
pattern MetricStatisticAverage = MetricStatistic' "Average"

pattern MetricStatisticMinimum :: MetricStatistic
pattern MetricStatisticMinimum = MetricStatistic' "Minimum"

pattern MetricStatisticMaximum :: MetricStatistic
pattern MetricStatisticMaximum = MetricStatistic' "Maximum"

pattern MetricStatisticSampleCount :: MetricStatistic
pattern MetricStatisticSampleCount = MetricStatistic' "SampleCount"

pattern MetricStatisticSum :: MetricStatistic
pattern MetricStatisticSum = MetricStatistic' "Sum"

{-# COMPLETE
  MetricStatisticAverage,
  MetricStatisticMinimum,
  MetricStatisticMaximum,
  MetricStatisticSampleCount,
  MetricStatisticSum,
  MetricStatistic'
  #-}
