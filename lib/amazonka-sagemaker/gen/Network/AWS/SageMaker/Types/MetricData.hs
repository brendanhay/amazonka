{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MetricData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.MetricData
  ( MetricData (..)
  -- * Smart constructor
  , mkMetricData
  -- * Lenses
  , mdMetricName
  , mdTimestamp
  , mdValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.MetricName as Types

-- | The name, value, and date and time of a metric that was emitted to Amazon CloudWatch.
--
-- /See:/ 'mkMetricData' smart constructor.
data MetricData = MetricData'
  { metricName :: Core.Maybe Types.MetricName
    -- ^ The name of the metric.
  , timestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the algorithm emitted the metric.
  , value :: Core.Maybe Core.Double
    -- ^ The value of the metric.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'MetricData' value with any optional fields omitted.
mkMetricData
    :: MetricData
mkMetricData
  = MetricData'{metricName = Core.Nothing, timestamp = Core.Nothing,
                value = Core.Nothing}

-- | The name of the metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdMetricName :: Lens.Lens' MetricData (Core.Maybe Types.MetricName)
mdMetricName = Lens.field @"metricName"
{-# INLINEABLE mdMetricName #-}
{-# DEPRECATED metricName "Use generic-lens or generic-optics with 'metricName' instead"  #-}

-- | The date and time that the algorithm emitted the metric.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdTimestamp :: Lens.Lens' MetricData (Core.Maybe Core.NominalDiffTime)
mdTimestamp = Lens.field @"timestamp"
{-# INLINEABLE mdTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

-- | The value of the metric.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdValue :: Lens.Lens' MetricData (Core.Maybe Core.Double)
mdValue = Lens.field @"value"
{-# INLINEABLE mdValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON MetricData where
        parseJSON
          = Core.withObject "MetricData" Core.$
              \ x ->
                MetricData' Core.<$>
                  (x Core..:? "MetricName") Core.<*> x Core..:? "Timestamp" Core.<*>
                    x Core..:? "Value"
