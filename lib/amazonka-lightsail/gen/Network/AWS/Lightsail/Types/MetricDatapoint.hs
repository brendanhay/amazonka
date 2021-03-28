{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.MetricDatapoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.MetricDatapoint
  ( MetricDatapoint (..)
  -- * Smart constructor
  , mkMetricDatapoint
  -- * Lenses
  , mdAverage
  , mdMaximum
  , mdMinimum
  , mdSampleCount
  , mdSum
  , mdTimestamp
  , mdUnit
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.MetricUnit as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the metric data point.
--
-- /See:/ 'mkMetricDatapoint' smart constructor.
data MetricDatapoint = MetricDatapoint'
  { average :: Core.Maybe Core.Double
    -- ^ The average.
  , maximum :: Core.Maybe Core.Double
    -- ^ The maximum.
  , minimum :: Core.Maybe Core.Double
    -- ^ The minimum.
  , sampleCount :: Core.Maybe Core.Double
    -- ^ The sample count.
  , sum :: Core.Maybe Core.Double
    -- ^ The sum.
  , timestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp (e.g., @1479816991.349@ ).
  , unit :: Core.Maybe Types.MetricUnit
    -- ^ The unit. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'MetricDatapoint' value with any optional fields omitted.
mkMetricDatapoint
    :: MetricDatapoint
mkMetricDatapoint
  = MetricDatapoint'{average = Core.Nothing, maximum = Core.Nothing,
                     minimum = Core.Nothing, sampleCount = Core.Nothing,
                     sum = Core.Nothing, timestamp = Core.Nothing, unit = Core.Nothing}

-- | The average.
--
-- /Note:/ Consider using 'average' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdAverage :: Lens.Lens' MetricDatapoint (Core.Maybe Core.Double)
mdAverage = Lens.field @"average"
{-# INLINEABLE mdAverage #-}
{-# DEPRECATED average "Use generic-lens or generic-optics with 'average' instead"  #-}

-- | The maximum.
--
-- /Note:/ Consider using 'maximum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdMaximum :: Lens.Lens' MetricDatapoint (Core.Maybe Core.Double)
mdMaximum = Lens.field @"maximum"
{-# INLINEABLE mdMaximum #-}
{-# DEPRECATED maximum "Use generic-lens or generic-optics with 'maximum' instead"  #-}

-- | The minimum.
--
-- /Note:/ Consider using 'minimum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdMinimum :: Lens.Lens' MetricDatapoint (Core.Maybe Core.Double)
mdMinimum = Lens.field @"minimum"
{-# INLINEABLE mdMinimum #-}
{-# DEPRECATED minimum "Use generic-lens or generic-optics with 'minimum' instead"  #-}

-- | The sample count.
--
-- /Note:/ Consider using 'sampleCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdSampleCount :: Lens.Lens' MetricDatapoint (Core.Maybe Core.Double)
mdSampleCount = Lens.field @"sampleCount"
{-# INLINEABLE mdSampleCount #-}
{-# DEPRECATED sampleCount "Use generic-lens or generic-optics with 'sampleCount' instead"  #-}

-- | The sum.
--
-- /Note:/ Consider using 'sum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdSum :: Lens.Lens' MetricDatapoint (Core.Maybe Core.Double)
mdSum = Lens.field @"sum"
{-# INLINEABLE mdSum #-}
{-# DEPRECATED sum "Use generic-lens or generic-optics with 'sum' instead"  #-}

-- | The timestamp (e.g., @1479816991.349@ ).
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdTimestamp :: Lens.Lens' MetricDatapoint (Core.Maybe Core.NominalDiffTime)
mdTimestamp = Lens.field @"timestamp"
{-# INLINEABLE mdTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

-- | The unit. 
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdUnit :: Lens.Lens' MetricDatapoint (Core.Maybe Types.MetricUnit)
mdUnit = Lens.field @"unit"
{-# INLINEABLE mdUnit #-}
{-# DEPRECATED unit "Use generic-lens or generic-optics with 'unit' instead"  #-}

instance Core.FromJSON MetricDatapoint where
        parseJSON
          = Core.withObject "MetricDatapoint" Core.$
              \ x ->
                MetricDatapoint' Core.<$>
                  (x Core..:? "average") Core.<*> x Core..:? "maximum" Core.<*>
                    x Core..:? "minimum"
                    Core.<*> x Core..:? "sampleCount"
                    Core.<*> x Core..:? "sum"
                    Core.<*> x Core..:? "timestamp"
                    Core.<*> x Core..:? "unit"
