{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentMetricSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentMetricSummary
  ( TrialComponentMetricSummary (..),

    -- * Smart constructor
    mkTrialComponentMetricSummary,

    -- * Lenses
    tcmsAvg,
    tcmsCount,
    tcmsLast,
    tcmsMax,
    tcmsMetricName,
    tcmsMin,
    tcmsSourceArn,
    tcmsStdDev,
    tcmsTimeStamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.MetricName as Types
import qualified Network.AWS.SageMaker.Types.SourceArn as Types

-- | A summary of the metrics of a trial component.
--
-- /See:/ 'mkTrialComponentMetricSummary' smart constructor.
data TrialComponentMetricSummary = TrialComponentMetricSummary'
  { -- | The average value of the metric.
    avg :: Core.Maybe Core.Double,
    -- | The number of samples used to generate the metric.
    count :: Core.Maybe Core.Int,
    -- | The most recent value of the metric.
    last :: Core.Maybe Core.Double,
    -- | The maximum value of the metric.
    max :: Core.Maybe Core.Double,
    -- | The name of the metric.
    metricName :: Core.Maybe Types.MetricName,
    -- | The minimum value of the metric.
    min :: Core.Maybe Core.Double,
    -- | The Amazon Resource Name (ARN) of the source.
    sourceArn :: Core.Maybe Types.SourceArn,
    -- | The standard deviation of the metric.
    stdDev :: Core.Maybe Core.Double,
    -- | When the metric was last updated.
    timeStamp :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TrialComponentMetricSummary' value with any optional fields omitted.
mkTrialComponentMetricSummary ::
  TrialComponentMetricSummary
mkTrialComponentMetricSummary =
  TrialComponentMetricSummary'
    { avg = Core.Nothing,
      count = Core.Nothing,
      last = Core.Nothing,
      max = Core.Nothing,
      metricName = Core.Nothing,
      min = Core.Nothing,
      sourceArn = Core.Nothing,
      stdDev = Core.Nothing,
      timeStamp = Core.Nothing
    }

-- | The average value of the metric.
--
-- /Note:/ Consider using 'avg' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcmsAvg :: Lens.Lens' TrialComponentMetricSummary (Core.Maybe Core.Double)
tcmsAvg = Lens.field @"avg"
{-# DEPRECATED tcmsAvg "Use generic-lens or generic-optics with 'avg' instead." #-}

-- | The number of samples used to generate the metric.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcmsCount :: Lens.Lens' TrialComponentMetricSummary (Core.Maybe Core.Int)
tcmsCount = Lens.field @"count"
{-# DEPRECATED tcmsCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The most recent value of the metric.
--
-- /Note:/ Consider using 'last' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcmsLast :: Lens.Lens' TrialComponentMetricSummary (Core.Maybe Core.Double)
tcmsLast = Lens.field @"last"
{-# DEPRECATED tcmsLast "Use generic-lens or generic-optics with 'last' instead." #-}

-- | The maximum value of the metric.
--
-- /Note:/ Consider using 'max' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcmsMax :: Lens.Lens' TrialComponentMetricSummary (Core.Maybe Core.Double)
tcmsMax = Lens.field @"max"
{-# DEPRECATED tcmsMax "Use generic-lens or generic-optics with 'max' instead." #-}

-- | The name of the metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcmsMetricName :: Lens.Lens' TrialComponentMetricSummary (Core.Maybe Types.MetricName)
tcmsMetricName = Lens.field @"metricName"
{-# DEPRECATED tcmsMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The minimum value of the metric.
--
-- /Note:/ Consider using 'min' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcmsMin :: Lens.Lens' TrialComponentMetricSummary (Core.Maybe Core.Double)
tcmsMin = Lens.field @"min"
{-# DEPRECATED tcmsMin "Use generic-lens or generic-optics with 'min' instead." #-}

-- | The Amazon Resource Name (ARN) of the source.
--
-- /Note:/ Consider using 'sourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcmsSourceArn :: Lens.Lens' TrialComponentMetricSummary (Core.Maybe Types.SourceArn)
tcmsSourceArn = Lens.field @"sourceArn"
{-# DEPRECATED tcmsSourceArn "Use generic-lens or generic-optics with 'sourceArn' instead." #-}

-- | The standard deviation of the metric.
--
-- /Note:/ Consider using 'stdDev' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcmsStdDev :: Lens.Lens' TrialComponentMetricSummary (Core.Maybe Core.Double)
tcmsStdDev = Lens.field @"stdDev"
{-# DEPRECATED tcmsStdDev "Use generic-lens or generic-optics with 'stdDev' instead." #-}

-- | When the metric was last updated.
--
-- /Note:/ Consider using 'timeStamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcmsTimeStamp :: Lens.Lens' TrialComponentMetricSummary (Core.Maybe Core.NominalDiffTime)
tcmsTimeStamp = Lens.field @"timeStamp"
{-# DEPRECATED tcmsTimeStamp "Use generic-lens or generic-optics with 'timeStamp' instead." #-}

instance Core.FromJSON TrialComponentMetricSummary where
  parseJSON =
    Core.withObject "TrialComponentMetricSummary" Core.$
      \x ->
        TrialComponentMetricSummary'
          Core.<$> (x Core..:? "Avg")
          Core.<*> (x Core..:? "Count")
          Core.<*> (x Core..:? "Last")
          Core.<*> (x Core..:? "Max")
          Core.<*> (x Core..:? "MetricName")
          Core.<*> (x Core..:? "Min")
          Core.<*> (x Core..:? "SourceArn")
          Core.<*> (x Core..:? "StdDev")
          Core.<*> (x Core..:? "TimeStamp")
