{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a set of statistics that describes a specific metric.
--
-- /See:/ 'newStatisticSet' smart constructor.
data StatisticSet = StatisticSet'
  { -- | The number of samples used for the statistic set.
    sampleCount :: Prelude.Double,
    -- | The sum of values for the sample set.
    sum :: Prelude.Double,
    -- | The minimum value of the sample set.
    minimum :: Prelude.Double,
    -- | The maximum value of the sample set.
    maximum :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Double ->
  -- | 'sum'
  Prelude.Double ->
  -- | 'minimum'
  Prelude.Double ->
  -- | 'maximum'
  Prelude.Double ->
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
statisticSet_sampleCount :: Lens.Lens' StatisticSet Prelude.Double
statisticSet_sampleCount = Lens.lens (\StatisticSet' {sampleCount} -> sampleCount) (\s@StatisticSet' {} a -> s {sampleCount = a} :: StatisticSet)

-- | The sum of values for the sample set.
statisticSet_sum :: Lens.Lens' StatisticSet Prelude.Double
statisticSet_sum = Lens.lens (\StatisticSet' {sum} -> sum) (\s@StatisticSet' {} a -> s {sum = a} :: StatisticSet)

-- | The minimum value of the sample set.
statisticSet_minimum :: Lens.Lens' StatisticSet Prelude.Double
statisticSet_minimum = Lens.lens (\StatisticSet' {minimum} -> minimum) (\s@StatisticSet' {} a -> s {minimum = a} :: StatisticSet)

-- | The maximum value of the sample set.
statisticSet_maximum :: Lens.Lens' StatisticSet Prelude.Double
statisticSet_maximum = Lens.lens (\StatisticSet' {maximum} -> maximum) (\s@StatisticSet' {} a -> s {maximum = a} :: StatisticSet)

instance Prelude.Hashable StatisticSet

instance Prelude.NFData StatisticSet

instance Prelude.ToQuery StatisticSet where
  toQuery StatisticSet' {..} =
    Prelude.mconcat
      [ "SampleCount" Prelude.=: sampleCount,
        "Sum" Prelude.=: sum,
        "Minimum" Prelude.=: minimum,
        "Maximum" Prelude.=: maximum
      ]
