{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.StatisticSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.StatisticSet
  ( StatisticSet (..),

    -- * Smart constructor
    mkStatisticSet,

    -- * Lenses
    ssSampleCount,
    ssSum,
    ssMinimum,
    ssMaximum,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a set of statistics that describes a specific metric.
--
-- /See:/ 'mkStatisticSet' smart constructor.
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
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StatisticSet' value with any optional fields omitted.
mkStatisticSet ::
  -- | 'sampleCount'
  Core.Double ->
  -- | 'sum'
  Core.Double ->
  -- | 'minimum'
  Core.Double ->
  -- | 'maximum'
  Core.Double ->
  StatisticSet
mkStatisticSet sampleCount sum minimum maximum =
  StatisticSet' {sampleCount, sum, minimum, maximum}

-- | The number of samples used for the statistic set.
--
-- /Note:/ Consider using 'sampleCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssSampleCount :: Lens.Lens' StatisticSet Core.Double
ssSampleCount = Lens.field @"sampleCount"
{-# DEPRECATED ssSampleCount "Use generic-lens or generic-optics with 'sampleCount' instead." #-}

-- | The sum of values for the sample set.
--
-- /Note:/ Consider using 'sum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssSum :: Lens.Lens' StatisticSet Core.Double
ssSum = Lens.field @"sum"
{-# DEPRECATED ssSum "Use generic-lens or generic-optics with 'sum' instead." #-}

-- | The minimum value of the sample set.
--
-- /Note:/ Consider using 'minimum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssMinimum :: Lens.Lens' StatisticSet Core.Double
ssMinimum = Lens.field @"minimum"
{-# DEPRECATED ssMinimum "Use generic-lens or generic-optics with 'minimum' instead." #-}

-- | The maximum value of the sample set.
--
-- /Note:/ Consider using 'maximum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssMaximum :: Lens.Lens' StatisticSet Core.Double
ssMaximum = Lens.field @"maximum"
{-# DEPRECATED ssMaximum "Use generic-lens or generic-optics with 'maximum' instead." #-}
