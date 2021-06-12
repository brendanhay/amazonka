{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.StatisticSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.StatisticSet where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a set of statistics that describes a specific metric.
--
-- /See:/ 'newStatisticSet' smart constructor.
data StatisticSet = StatisticSet'
  { -- | The number of samples used for the statistic set.
    sampleCount :: Core.Double,
    -- | The sum of values for the sample set.
    sum :: Core.Double,
    -- | The minimum value of the sample set.
    minimum :: Core.Double,
    -- | The maximum value of the sample set.
    maximum :: Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StatisticSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sampleCount', 'statisticSet_sampleCount' - The number of samples used for the statistic set.
--
-- 'sum', 'statisticSet_sum' - The sum of values for the sample set.
--
-- 'minimum', 'statisticSet_minimum' - The minimum value of the sample set.
--
-- 'maximum', 'statisticSet_maximum' - The maximum value of the sample set.
newStatisticSet ::
  -- | 'sampleCount'
  Core.Double ->
  -- | 'sum'
  Core.Double ->
  -- | 'minimum'
  Core.Double ->
  -- | 'maximum'
  Core.Double ->
  StatisticSet
newStatisticSet
  pSampleCount_
  pSum_
  pMinimum_
  pMaximum_ =
    StatisticSet'
      { sampleCount = pSampleCount_,
        sum = pSum_,
        minimum = pMinimum_,
        maximum = pMaximum_
      }

-- | The number of samples used for the statistic set.
statisticSet_sampleCount :: Lens.Lens' StatisticSet Core.Double
statisticSet_sampleCount = Lens.lens (\StatisticSet' {sampleCount} -> sampleCount) (\s@StatisticSet' {} a -> s {sampleCount = a} :: StatisticSet)

-- | The sum of values for the sample set.
statisticSet_sum :: Lens.Lens' StatisticSet Core.Double
statisticSet_sum = Lens.lens (\StatisticSet' {sum} -> sum) (\s@StatisticSet' {} a -> s {sum = a} :: StatisticSet)

-- | The minimum value of the sample set.
statisticSet_minimum :: Lens.Lens' StatisticSet Core.Double
statisticSet_minimum = Lens.lens (\StatisticSet' {minimum} -> minimum) (\s@StatisticSet' {} a -> s {minimum = a} :: StatisticSet)

-- | The maximum value of the sample set.
statisticSet_maximum :: Lens.Lens' StatisticSet Core.Double
statisticSet_maximum = Lens.lens (\StatisticSet' {maximum} -> maximum) (\s@StatisticSet' {} a -> s {maximum = a} :: StatisticSet)

instance Core.Hashable StatisticSet

instance Core.NFData StatisticSet

instance Core.ToQuery StatisticSet where
  toQuery StatisticSet' {..} =
    Core.mconcat
      [ "SampleCount" Core.=: sampleCount,
        "Sum" Core.=: sum,
        "Minimum" Core.=: minimum,
        "Maximum" Core.=: maximum
      ]
