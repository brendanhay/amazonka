{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.MetricFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchLogs.Types.MetricFilter
  ( MetricFilter (..)
  -- * Smart constructor
  , mkMetricFilter
  -- * Lenses
  , mfCreationTime
  , mfFilterName
  , mfFilterPattern
  , mfLogGroupName
  , mfMetricTransformations
  ) where

import qualified Network.AWS.CloudWatchLogs.Types.FilterName as Types
import qualified Network.AWS.CloudWatchLogs.Types.FilterPattern as Types
import qualified Network.AWS.CloudWatchLogs.Types.LogGroupName as Types
import qualified Network.AWS.CloudWatchLogs.Types.MetricTransformation as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Metric filters express how CloudWatch Logs would extract metric observations from ingested log events and transform them into metric data in a CloudWatch metric.
--
-- /See:/ 'mkMetricFilter' smart constructor.
data MetricFilter = MetricFilter'
  { creationTime :: Core.Maybe Core.Natural
    -- ^ The creation time of the metric filter, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
  , filterName :: Core.Maybe Types.FilterName
    -- ^ The name of the metric filter.
  , filterPattern :: Core.Maybe Types.FilterPattern
  , logGroupName :: Core.Maybe Types.LogGroupName
    -- ^ The name of the log group.
  , metricTransformations :: Core.Maybe (Core.NonEmpty Types.MetricTransformation)
    -- ^ The metric transformations.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MetricFilter' value with any optional fields omitted.
mkMetricFilter
    :: MetricFilter
mkMetricFilter
  = MetricFilter'{creationTime = Core.Nothing,
                  filterName = Core.Nothing, filterPattern = Core.Nothing,
                  logGroupName = Core.Nothing, metricTransformations = Core.Nothing}

-- | The creation time of the metric filter, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfCreationTime :: Lens.Lens' MetricFilter (Core.Maybe Core.Natural)
mfCreationTime = Lens.field @"creationTime"
{-# INLINEABLE mfCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The name of the metric filter.
--
-- /Note:/ Consider using 'filterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfFilterName :: Lens.Lens' MetricFilter (Core.Maybe Types.FilterName)
mfFilterName = Lens.field @"filterName"
{-# INLINEABLE mfFilterName #-}
{-# DEPRECATED filterName "Use generic-lens or generic-optics with 'filterName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'filterPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfFilterPattern :: Lens.Lens' MetricFilter (Core.Maybe Types.FilterPattern)
mfFilterPattern = Lens.field @"filterPattern"
{-# INLINEABLE mfFilterPattern #-}
{-# DEPRECATED filterPattern "Use generic-lens or generic-optics with 'filterPattern' instead"  #-}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfLogGroupName :: Lens.Lens' MetricFilter (Core.Maybe Types.LogGroupName)
mfLogGroupName = Lens.field @"logGroupName"
{-# INLINEABLE mfLogGroupName #-}
{-# DEPRECATED logGroupName "Use generic-lens or generic-optics with 'logGroupName' instead"  #-}

-- | The metric transformations.
--
-- /Note:/ Consider using 'metricTransformations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfMetricTransformations :: Lens.Lens' MetricFilter (Core.Maybe (Core.NonEmpty Types.MetricTransformation))
mfMetricTransformations = Lens.field @"metricTransformations"
{-# INLINEABLE mfMetricTransformations #-}
{-# DEPRECATED metricTransformations "Use generic-lens or generic-optics with 'metricTransformations' instead"  #-}

instance Core.FromJSON MetricFilter where
        parseJSON
          = Core.withObject "MetricFilter" Core.$
              \ x ->
                MetricFilter' Core.<$>
                  (x Core..:? "creationTime") Core.<*> x Core..:? "filterName"
                    Core.<*> x Core..:? "filterPattern"
                    Core.<*> x Core..:? "logGroupName"
                    Core.<*> x Core..:? "metricTransformations"
