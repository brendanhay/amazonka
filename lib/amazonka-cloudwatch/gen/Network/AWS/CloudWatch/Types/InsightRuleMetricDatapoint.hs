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
    irmdMaxContributorValue,
    irmdSampleCount,
    irmdMaximum,
    irmdAverage,
    irmdMinimum,
    irmdUniqueContributors,
    irmdSum,
    irmdTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | One data point from the metric time series returned in a Contributor Insights rule report.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetInsightRuleReport.html GetInsightRuleReport> .
--
-- /See:/ 'mkInsightRuleMetricDatapoint' smart constructor.
data InsightRuleMetricDatapoint = InsightRuleMetricDatapoint'
  { -- | The maximum value provided by one contributor during this timestamp. Each timestamp is evaluated separately, so the identity of the max contributor could be different for each timestamp.
    --
    -- This statistic is returned only if you included it in the @Metrics@ array in your request.
    maxContributorValue :: Lude.Maybe Lude.Double,
    -- | The number of occurrences that matched the rule during this data point.
    --
    -- This statistic is returned only if you included it in the @Metrics@ array in your request.
    sampleCount :: Lude.Maybe Lude.Double,
    -- | The maximum value from a single occurence from a single contributor during the time period represented by that data point.
    --
    -- This statistic is returned only if you included it in the @Metrics@ array in your request.
    maximum :: Lude.Maybe Lude.Double,
    -- | The average value from all contributors during the time period represented by that data point.
    --
    -- This statistic is returned only if you included it in the @Metrics@ array in your request.
    average :: Lude.Maybe Lude.Double,
    -- | The minimum value from a single contributor during the time period represented by that data point.
    --
    -- This statistic is returned only if you included it in the @Metrics@ array in your request.
    minimum :: Lude.Maybe Lude.Double,
    -- | The number of unique contributors who published data during this timestamp.
    --
    -- This statistic is returned only if you included it in the @Metrics@ array in your request.
    uniqueContributors :: Lude.Maybe Lude.Double,
    -- | The sum of the values from all contributors during the time period represented by that data point.
    --
    -- This statistic is returned only if you included it in the @Metrics@ array in your request.
    sum :: Lude.Maybe Lude.Double,
    -- | The timestamp of the data point.
    timestamp :: Lude.DateTime
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InsightRuleMetricDatapoint' with the minimum fields required to make a request.
--
-- * 'maxContributorValue' - The maximum value provided by one contributor during this timestamp. Each timestamp is evaluated separately, so the identity of the max contributor could be different for each timestamp.
--
-- This statistic is returned only if you included it in the @Metrics@ array in your request.
-- * 'sampleCount' - The number of occurrences that matched the rule during this data point.
--
-- This statistic is returned only if you included it in the @Metrics@ array in your request.
-- * 'maximum' - The maximum value from a single occurence from a single contributor during the time period represented by that data point.
--
-- This statistic is returned only if you included it in the @Metrics@ array in your request.
-- * 'average' - The average value from all contributors during the time period represented by that data point.
--
-- This statistic is returned only if you included it in the @Metrics@ array in your request.
-- * 'minimum' - The minimum value from a single contributor during the time period represented by that data point.
--
-- This statistic is returned only if you included it in the @Metrics@ array in your request.
-- * 'uniqueContributors' - The number of unique contributors who published data during this timestamp.
--
-- This statistic is returned only if you included it in the @Metrics@ array in your request.
-- * 'sum' - The sum of the values from all contributors during the time period represented by that data point.
--
-- This statistic is returned only if you included it in the @Metrics@ array in your request.
-- * 'timestamp' - The timestamp of the data point.
mkInsightRuleMetricDatapoint ::
  -- | 'timestamp'
  Lude.DateTime ->
  InsightRuleMetricDatapoint
mkInsightRuleMetricDatapoint pTimestamp_ =
  InsightRuleMetricDatapoint'
    { maxContributorValue = Lude.Nothing,
      sampleCount = Lude.Nothing,
      maximum = Lude.Nothing,
      average = Lude.Nothing,
      minimum = Lude.Nothing,
      uniqueContributors = Lude.Nothing,
      sum = Lude.Nothing,
      timestamp = pTimestamp_
    }

-- | The maximum value provided by one contributor during this timestamp. Each timestamp is evaluated separately, so the identity of the max contributor could be different for each timestamp.
--
-- This statistic is returned only if you included it in the @Metrics@ array in your request.
--
-- /Note:/ Consider using 'maxContributorValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irmdMaxContributorValue :: Lens.Lens' InsightRuleMetricDatapoint (Lude.Maybe Lude.Double)
irmdMaxContributorValue = Lens.lens (maxContributorValue :: InsightRuleMetricDatapoint -> Lude.Maybe Lude.Double) (\s a -> s {maxContributorValue = a} :: InsightRuleMetricDatapoint)
{-# DEPRECATED irmdMaxContributorValue "Use generic-lens or generic-optics with 'maxContributorValue' instead." #-}

-- | The number of occurrences that matched the rule during this data point.
--
-- This statistic is returned only if you included it in the @Metrics@ array in your request.
--
-- /Note:/ Consider using 'sampleCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irmdSampleCount :: Lens.Lens' InsightRuleMetricDatapoint (Lude.Maybe Lude.Double)
irmdSampleCount = Lens.lens (sampleCount :: InsightRuleMetricDatapoint -> Lude.Maybe Lude.Double) (\s a -> s {sampleCount = a} :: InsightRuleMetricDatapoint)
{-# DEPRECATED irmdSampleCount "Use generic-lens or generic-optics with 'sampleCount' instead." #-}

-- | The maximum value from a single occurence from a single contributor during the time period represented by that data point.
--
-- This statistic is returned only if you included it in the @Metrics@ array in your request.
--
-- /Note:/ Consider using 'maximum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irmdMaximum :: Lens.Lens' InsightRuleMetricDatapoint (Lude.Maybe Lude.Double)
irmdMaximum = Lens.lens (maximum :: InsightRuleMetricDatapoint -> Lude.Maybe Lude.Double) (\s a -> s {maximum = a} :: InsightRuleMetricDatapoint)
{-# DEPRECATED irmdMaximum "Use generic-lens or generic-optics with 'maximum' instead." #-}

-- | The average value from all contributors during the time period represented by that data point.
--
-- This statistic is returned only if you included it in the @Metrics@ array in your request.
--
-- /Note:/ Consider using 'average' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irmdAverage :: Lens.Lens' InsightRuleMetricDatapoint (Lude.Maybe Lude.Double)
irmdAverage = Lens.lens (average :: InsightRuleMetricDatapoint -> Lude.Maybe Lude.Double) (\s a -> s {average = a} :: InsightRuleMetricDatapoint)
{-# DEPRECATED irmdAverage "Use generic-lens or generic-optics with 'average' instead." #-}

-- | The minimum value from a single contributor during the time period represented by that data point.
--
-- This statistic is returned only if you included it in the @Metrics@ array in your request.
--
-- /Note:/ Consider using 'minimum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irmdMinimum :: Lens.Lens' InsightRuleMetricDatapoint (Lude.Maybe Lude.Double)
irmdMinimum = Lens.lens (minimum :: InsightRuleMetricDatapoint -> Lude.Maybe Lude.Double) (\s a -> s {minimum = a} :: InsightRuleMetricDatapoint)
{-# DEPRECATED irmdMinimum "Use generic-lens or generic-optics with 'minimum' instead." #-}

-- | The number of unique contributors who published data during this timestamp.
--
-- This statistic is returned only if you included it in the @Metrics@ array in your request.
--
-- /Note:/ Consider using 'uniqueContributors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irmdUniqueContributors :: Lens.Lens' InsightRuleMetricDatapoint (Lude.Maybe Lude.Double)
irmdUniqueContributors = Lens.lens (uniqueContributors :: InsightRuleMetricDatapoint -> Lude.Maybe Lude.Double) (\s a -> s {uniqueContributors = a} :: InsightRuleMetricDatapoint)
{-# DEPRECATED irmdUniqueContributors "Use generic-lens or generic-optics with 'uniqueContributors' instead." #-}

-- | The sum of the values from all contributors during the time period represented by that data point.
--
-- This statistic is returned only if you included it in the @Metrics@ array in your request.
--
-- /Note:/ Consider using 'sum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irmdSum :: Lens.Lens' InsightRuleMetricDatapoint (Lude.Maybe Lude.Double)
irmdSum = Lens.lens (sum :: InsightRuleMetricDatapoint -> Lude.Maybe Lude.Double) (\s a -> s {sum = a} :: InsightRuleMetricDatapoint)
{-# DEPRECATED irmdSum "Use generic-lens or generic-optics with 'sum' instead." #-}

-- | The timestamp of the data point.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irmdTimestamp :: Lens.Lens' InsightRuleMetricDatapoint Lude.DateTime
irmdTimestamp = Lens.lens (timestamp :: InsightRuleMetricDatapoint -> Lude.DateTime) (\s a -> s {timestamp = a} :: InsightRuleMetricDatapoint)
{-# DEPRECATED irmdTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromXML InsightRuleMetricDatapoint where
  parseXML x =
    InsightRuleMetricDatapoint'
      Lude.<$> (x Lude..@? "MaxContributorValue")
      Lude.<*> (x Lude..@? "SampleCount")
      Lude.<*> (x Lude..@? "Maximum")
      Lude.<*> (x Lude..@? "Average")
      Lude.<*> (x Lude..@? "Minimum")
      Lude.<*> (x Lude..@? "UniqueContributors")
      Lude.<*> (x Lude..@? "Sum")
      Lude.<*> (x Lude..@ "Timestamp")
