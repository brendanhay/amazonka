{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.InsightRuleMetricDatapoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.InsightRuleMetricDatapoint
  ( InsightRuleMetricDatapoint (..),

    -- * Smart constructor
    mkInsightRuleMetricDatapoint,

    -- * Lenses
    irmdTimestamp,
    irmdAverage,
    irmdMaxContributorValue,
    irmdMaximum,
    irmdMinimum,
    irmdSampleCount,
    irmdSum,
    irmdUniqueContributors,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | One data point from the metric time series returned in a Contributor Insights rule report.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetInsightRuleReport.html GetInsightRuleReport> .
--
-- /See:/ 'mkInsightRuleMetricDatapoint' smart constructor.
data InsightRuleMetricDatapoint = InsightRuleMetricDatapoint'
  { -- | The timestamp of the data point.
    timestamp :: Core.UTCTime,
    -- | The average value from all contributors during the time period represented by that data point.
    --
    -- This statistic is returned only if you included it in the @Metrics@ array in your request.
    average :: Core.Maybe Core.Double,
    -- | The maximum value provided by one contributor during this timestamp. Each timestamp is evaluated separately, so the identity of the max contributor could be different for each timestamp.
    --
    -- This statistic is returned only if you included it in the @Metrics@ array in your request.
    maxContributorValue :: Core.Maybe Core.Double,
    -- | The maximum value from a single occurence from a single contributor during the time period represented by that data point.
    --
    -- This statistic is returned only if you included it in the @Metrics@ array in your request.
    maximum :: Core.Maybe Core.Double,
    -- | The minimum value from a single contributor during the time period represented by that data point.
    --
    -- This statistic is returned only if you included it in the @Metrics@ array in your request.
    minimum :: Core.Maybe Core.Double,
    -- | The number of occurrences that matched the rule during this data point.
    --
    -- This statistic is returned only if you included it in the @Metrics@ array in your request.
    sampleCount :: Core.Maybe Core.Double,
    -- | The sum of the values from all contributors during the time period represented by that data point.
    --
    -- This statistic is returned only if you included it in the @Metrics@ array in your request.
    sum :: Core.Maybe Core.Double,
    -- | The number of unique contributors who published data during this timestamp.
    --
    -- This statistic is returned only if you included it in the @Metrics@ array in your request.
    uniqueContributors :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'InsightRuleMetricDatapoint' value with any optional fields omitted.
mkInsightRuleMetricDatapoint ::
  -- | 'timestamp'
  Core.UTCTime ->
  InsightRuleMetricDatapoint
mkInsightRuleMetricDatapoint timestamp =
  InsightRuleMetricDatapoint'
    { timestamp,
      average = Core.Nothing,
      maxContributorValue = Core.Nothing,
      maximum = Core.Nothing,
      minimum = Core.Nothing,
      sampleCount = Core.Nothing,
      sum = Core.Nothing,
      uniqueContributors = Core.Nothing
    }

-- | The timestamp of the data point.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irmdTimestamp :: Lens.Lens' InsightRuleMetricDatapoint Core.UTCTime
irmdTimestamp = Lens.field @"timestamp"
{-# DEPRECATED irmdTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The average value from all contributors during the time period represented by that data point.
--
-- This statistic is returned only if you included it in the @Metrics@ array in your request.
--
-- /Note:/ Consider using 'average' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irmdAverage :: Lens.Lens' InsightRuleMetricDatapoint (Core.Maybe Core.Double)
irmdAverage = Lens.field @"average"
{-# DEPRECATED irmdAverage "Use generic-lens or generic-optics with 'average' instead." #-}

-- | The maximum value provided by one contributor during this timestamp. Each timestamp is evaluated separately, so the identity of the max contributor could be different for each timestamp.
--
-- This statistic is returned only if you included it in the @Metrics@ array in your request.
--
-- /Note:/ Consider using 'maxContributorValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irmdMaxContributorValue :: Lens.Lens' InsightRuleMetricDatapoint (Core.Maybe Core.Double)
irmdMaxContributorValue = Lens.field @"maxContributorValue"
{-# DEPRECATED irmdMaxContributorValue "Use generic-lens or generic-optics with 'maxContributorValue' instead." #-}

-- | The maximum value from a single occurence from a single contributor during the time period represented by that data point.
--
-- This statistic is returned only if you included it in the @Metrics@ array in your request.
--
-- /Note:/ Consider using 'maximum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irmdMaximum :: Lens.Lens' InsightRuleMetricDatapoint (Core.Maybe Core.Double)
irmdMaximum = Lens.field @"maximum"
{-# DEPRECATED irmdMaximum "Use generic-lens or generic-optics with 'maximum' instead." #-}

-- | The minimum value from a single contributor during the time period represented by that data point.
--
-- This statistic is returned only if you included it in the @Metrics@ array in your request.
--
-- /Note:/ Consider using 'minimum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irmdMinimum :: Lens.Lens' InsightRuleMetricDatapoint (Core.Maybe Core.Double)
irmdMinimum = Lens.field @"minimum"
{-# DEPRECATED irmdMinimum "Use generic-lens or generic-optics with 'minimum' instead." #-}

-- | The number of occurrences that matched the rule during this data point.
--
-- This statistic is returned only if you included it in the @Metrics@ array in your request.
--
-- /Note:/ Consider using 'sampleCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irmdSampleCount :: Lens.Lens' InsightRuleMetricDatapoint (Core.Maybe Core.Double)
irmdSampleCount = Lens.field @"sampleCount"
{-# DEPRECATED irmdSampleCount "Use generic-lens or generic-optics with 'sampleCount' instead." #-}

-- | The sum of the values from all contributors during the time period represented by that data point.
--
-- This statistic is returned only if you included it in the @Metrics@ array in your request.
--
-- /Note:/ Consider using 'sum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irmdSum :: Lens.Lens' InsightRuleMetricDatapoint (Core.Maybe Core.Double)
irmdSum = Lens.field @"sum"
{-# DEPRECATED irmdSum "Use generic-lens or generic-optics with 'sum' instead." #-}

-- | The number of unique contributors who published data during this timestamp.
--
-- This statistic is returned only if you included it in the @Metrics@ array in your request.
--
-- /Note:/ Consider using 'uniqueContributors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irmdUniqueContributors :: Lens.Lens' InsightRuleMetricDatapoint (Core.Maybe Core.Double)
irmdUniqueContributors = Lens.field @"uniqueContributors"
{-# DEPRECATED irmdUniqueContributors "Use generic-lens or generic-optics with 'uniqueContributors' instead." #-}

instance Core.FromXML InsightRuleMetricDatapoint where
  parseXML x =
    InsightRuleMetricDatapoint'
      Core.<$> (x Core..@ "Timestamp")
      Core.<*> (x Core..@? "Average")
      Core.<*> (x Core..@? "MaxContributorValue")
      Core.<*> (x Core..@? "Maximum")
      Core.<*> (x Core..@? "Minimum")
      Core.<*> (x Core..@? "SampleCount")
      Core.<*> (x Core..@? "Sum")
      Core.<*> (x Core..@? "UniqueContributors")
