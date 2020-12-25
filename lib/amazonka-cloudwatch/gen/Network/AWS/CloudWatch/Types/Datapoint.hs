{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.Datapoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.Datapoint
  ( Datapoint (..),

    -- * Smart constructor
    mkDatapoint,

    -- * Lenses
    dAverage,
    dExtendedStatistics,
    dMaximum,
    dMinimum,
    dSampleCount,
    dSum,
    dTimestamp,
    dUnit,
  )
where

import qualified Network.AWS.CloudWatch.Types.ExtendedStatistic as Types
import qualified Network.AWS.CloudWatch.Types.StandardUnit as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Encapsulates the statistical data that CloudWatch computes from metric data.
--
-- /See:/ 'mkDatapoint' smart constructor.
data Datapoint = Datapoint'
  { -- | The average of the metric values that correspond to the data point.
    average :: Core.Maybe Core.Double,
    -- | The percentile statistic for the data point.
    extendedStatistics :: Core.Maybe (Core.HashMap Types.ExtendedStatistic Core.Double),
    -- | The maximum metric value for the data point.
    maximum :: Core.Maybe Core.Double,
    -- | The minimum metric value for the data point.
    minimum :: Core.Maybe Core.Double,
    -- | The number of metric values that contributed to the aggregate value of this data point.
    sampleCount :: Core.Maybe Core.Double,
    -- | The sum of the metric values for the data point.
    sum :: Core.Maybe Core.Double,
    -- | The time stamp used for the data point.
    timestamp :: Core.Maybe Core.UTCTime,
    -- | The standard unit for the data point.
    unit :: Core.Maybe Types.StandardUnit
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Datapoint' value with any optional fields omitted.
mkDatapoint ::
  Datapoint
mkDatapoint =
  Datapoint'
    { average = Core.Nothing,
      extendedStatistics = Core.Nothing,
      maximum = Core.Nothing,
      minimum = Core.Nothing,
      sampleCount = Core.Nothing,
      sum = Core.Nothing,
      timestamp = Core.Nothing,
      unit = Core.Nothing
    }

-- | The average of the metric values that correspond to the data point.
--
-- /Note:/ Consider using 'average' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAverage :: Lens.Lens' Datapoint (Core.Maybe Core.Double)
dAverage = Lens.field @"average"
{-# DEPRECATED dAverage "Use generic-lens or generic-optics with 'average' instead." #-}

-- | The percentile statistic for the data point.
--
-- /Note:/ Consider using 'extendedStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dExtendedStatistics :: Lens.Lens' Datapoint (Core.Maybe (Core.HashMap Types.ExtendedStatistic Core.Double))
dExtendedStatistics = Lens.field @"extendedStatistics"
{-# DEPRECATED dExtendedStatistics "Use generic-lens or generic-optics with 'extendedStatistics' instead." #-}

-- | The maximum metric value for the data point.
--
-- /Note:/ Consider using 'maximum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaximum :: Lens.Lens' Datapoint (Core.Maybe Core.Double)
dMaximum = Lens.field @"maximum"
{-# DEPRECATED dMaximum "Use generic-lens or generic-optics with 'maximum' instead." #-}

-- | The minimum metric value for the data point.
--
-- /Note:/ Consider using 'minimum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMinimum :: Lens.Lens' Datapoint (Core.Maybe Core.Double)
dMinimum = Lens.field @"minimum"
{-# DEPRECATED dMinimum "Use generic-lens or generic-optics with 'minimum' instead." #-}

-- | The number of metric values that contributed to the aggregate value of this data point.
--
-- /Note:/ Consider using 'sampleCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSampleCount :: Lens.Lens' Datapoint (Core.Maybe Core.Double)
dSampleCount = Lens.field @"sampleCount"
{-# DEPRECATED dSampleCount "Use generic-lens or generic-optics with 'sampleCount' instead." #-}

-- | The sum of the metric values for the data point.
--
-- /Note:/ Consider using 'sum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSum :: Lens.Lens' Datapoint (Core.Maybe Core.Double)
dSum = Lens.field @"sum"
{-# DEPRECATED dSum "Use generic-lens or generic-optics with 'sum' instead." #-}

-- | The time stamp used for the data point.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTimestamp :: Lens.Lens' Datapoint (Core.Maybe Core.UTCTime)
dTimestamp = Lens.field @"timestamp"
{-# DEPRECATED dTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The standard unit for the data point.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dUnit :: Lens.Lens' Datapoint (Core.Maybe Types.StandardUnit)
dUnit = Lens.field @"unit"
{-# DEPRECATED dUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

instance Core.FromXML Datapoint where
  parseXML x =
    Datapoint'
      Core.<$> (x Core..@? "Average")
      Core.<*> ( x Core..@? "ExtendedStatistics"
                   Core..<@> Core.parseXMLMap "entry" "key" "value"
               )
      Core.<*> (x Core..@? "Maximum")
      Core.<*> (x Core..@? "Minimum")
      Core.<*> (x Core..@? "SampleCount")
      Core.<*> (x Core..@? "Sum")
      Core.<*> (x Core..@? "Timestamp")
      Core.<*> (x Core..@? "Unit")
