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
    tcmsMax,
    tcmsSourceARN,
    tcmsAvg,
    tcmsCount,
    tcmsMetricName,
    tcmsStdDev,
    tcmsMin,
    tcmsLast,
    tcmsTimeStamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A summary of the metrics of a trial component.
--
-- /See:/ 'mkTrialComponentMetricSummary' smart constructor.
data TrialComponentMetricSummary = TrialComponentMetricSummary'
  { -- | The maximum value of the metric.
    max :: Lude.Maybe Lude.Double,
    -- | The Amazon Resource Name (ARN) of the source.
    sourceARN :: Lude.Maybe Lude.Text,
    -- | The average value of the metric.
    avg :: Lude.Maybe Lude.Double,
    -- | The number of samples used to generate the metric.
    count :: Lude.Maybe Lude.Int,
    -- | The name of the metric.
    metricName :: Lude.Maybe Lude.Text,
    -- | The standard deviation of the metric.
    stdDev :: Lude.Maybe Lude.Double,
    -- | The minimum value of the metric.
    min :: Lude.Maybe Lude.Double,
    -- | The most recent value of the metric.
    last :: Lude.Maybe Lude.Double,
    -- | When the metric was last updated.
    timeStamp :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrialComponentMetricSummary' with the minimum fields required to make a request.
--
-- * 'max' - The maximum value of the metric.
-- * 'sourceARN' - The Amazon Resource Name (ARN) of the source.
-- * 'avg' - The average value of the metric.
-- * 'count' - The number of samples used to generate the metric.
-- * 'metricName' - The name of the metric.
-- * 'stdDev' - The standard deviation of the metric.
-- * 'min' - The minimum value of the metric.
-- * 'last' - The most recent value of the metric.
-- * 'timeStamp' - When the metric was last updated.
mkTrialComponentMetricSummary ::
  TrialComponentMetricSummary
mkTrialComponentMetricSummary =
  TrialComponentMetricSummary'
    { max = Lude.Nothing,
      sourceARN = Lude.Nothing,
      avg = Lude.Nothing,
      count = Lude.Nothing,
      metricName = Lude.Nothing,
      stdDev = Lude.Nothing,
      min = Lude.Nothing,
      last = Lude.Nothing,
      timeStamp = Lude.Nothing
    }

-- | The maximum value of the metric.
--
-- /Note:/ Consider using 'max' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcmsMax :: Lens.Lens' TrialComponentMetricSummary (Lude.Maybe Lude.Double)
tcmsMax = Lens.lens (max :: TrialComponentMetricSummary -> Lude.Maybe Lude.Double) (\s a -> s {max = a} :: TrialComponentMetricSummary)
{-# DEPRECATED tcmsMax "Use generic-lens or generic-optics with 'max' instead." #-}

-- | The Amazon Resource Name (ARN) of the source.
--
-- /Note:/ Consider using 'sourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcmsSourceARN :: Lens.Lens' TrialComponentMetricSummary (Lude.Maybe Lude.Text)
tcmsSourceARN = Lens.lens (sourceARN :: TrialComponentMetricSummary -> Lude.Maybe Lude.Text) (\s a -> s {sourceARN = a} :: TrialComponentMetricSummary)
{-# DEPRECATED tcmsSourceARN "Use generic-lens or generic-optics with 'sourceARN' instead." #-}

-- | The average value of the metric.
--
-- /Note:/ Consider using 'avg' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcmsAvg :: Lens.Lens' TrialComponentMetricSummary (Lude.Maybe Lude.Double)
tcmsAvg = Lens.lens (avg :: TrialComponentMetricSummary -> Lude.Maybe Lude.Double) (\s a -> s {avg = a} :: TrialComponentMetricSummary)
{-# DEPRECATED tcmsAvg "Use generic-lens or generic-optics with 'avg' instead." #-}

-- | The number of samples used to generate the metric.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcmsCount :: Lens.Lens' TrialComponentMetricSummary (Lude.Maybe Lude.Int)
tcmsCount = Lens.lens (count :: TrialComponentMetricSummary -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: TrialComponentMetricSummary)
{-# DEPRECATED tcmsCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The name of the metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcmsMetricName :: Lens.Lens' TrialComponentMetricSummary (Lude.Maybe Lude.Text)
tcmsMetricName = Lens.lens (metricName :: TrialComponentMetricSummary -> Lude.Maybe Lude.Text) (\s a -> s {metricName = a} :: TrialComponentMetricSummary)
{-# DEPRECATED tcmsMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The standard deviation of the metric.
--
-- /Note:/ Consider using 'stdDev' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcmsStdDev :: Lens.Lens' TrialComponentMetricSummary (Lude.Maybe Lude.Double)
tcmsStdDev = Lens.lens (stdDev :: TrialComponentMetricSummary -> Lude.Maybe Lude.Double) (\s a -> s {stdDev = a} :: TrialComponentMetricSummary)
{-# DEPRECATED tcmsStdDev "Use generic-lens or generic-optics with 'stdDev' instead." #-}

-- | The minimum value of the metric.
--
-- /Note:/ Consider using 'min' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcmsMin :: Lens.Lens' TrialComponentMetricSummary (Lude.Maybe Lude.Double)
tcmsMin = Lens.lens (min :: TrialComponentMetricSummary -> Lude.Maybe Lude.Double) (\s a -> s {min = a} :: TrialComponentMetricSummary)
{-# DEPRECATED tcmsMin "Use generic-lens or generic-optics with 'min' instead." #-}

-- | The most recent value of the metric.
--
-- /Note:/ Consider using 'last' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcmsLast :: Lens.Lens' TrialComponentMetricSummary (Lude.Maybe Lude.Double)
tcmsLast = Lens.lens (last :: TrialComponentMetricSummary -> Lude.Maybe Lude.Double) (\s a -> s {last = a} :: TrialComponentMetricSummary)
{-# DEPRECATED tcmsLast "Use generic-lens or generic-optics with 'last' instead." #-}

-- | When the metric was last updated.
--
-- /Note:/ Consider using 'timeStamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcmsTimeStamp :: Lens.Lens' TrialComponentMetricSummary (Lude.Maybe Lude.Timestamp)
tcmsTimeStamp = Lens.lens (timeStamp :: TrialComponentMetricSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {timeStamp = a} :: TrialComponentMetricSummary)
{-# DEPRECATED tcmsTimeStamp "Use generic-lens or generic-optics with 'timeStamp' instead." #-}

instance Lude.FromJSON TrialComponentMetricSummary where
  parseJSON =
    Lude.withObject
      "TrialComponentMetricSummary"
      ( \x ->
          TrialComponentMetricSummary'
            Lude.<$> (x Lude..:? "Max")
            Lude.<*> (x Lude..:? "SourceArn")
            Lude.<*> (x Lude..:? "Avg")
            Lude.<*> (x Lude..:? "Count")
            Lude.<*> (x Lude..:? "MetricName")
            Lude.<*> (x Lude..:? "StdDev")
            Lude.<*> (x Lude..:? "Min")
            Lude.<*> (x Lude..:? "Last")
            Lude.<*> (x Lude..:? "TimeStamp")
      )
