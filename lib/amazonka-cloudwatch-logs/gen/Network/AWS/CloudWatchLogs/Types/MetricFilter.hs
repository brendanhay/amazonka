{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.MetricFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.MetricFilter
  ( MetricFilter (..),

    -- * Smart constructor
    mkMetricFilter,

    -- * Lenses
    mfCreationTime,
    mfFilterName,
    mfLogGroupName,
    mfFilterPattern,
    mfMetricTransformations,
  )
where

import Network.AWS.CloudWatchLogs.Types.MetricTransformation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Metric filters express how CloudWatch Logs would extract metric observations from ingested log events and transform them into metric data in a CloudWatch metric.
--
-- /See:/ 'mkMetricFilter' smart constructor.
data MetricFilter = MetricFilter'
  { creationTime ::
      Lude.Maybe Lude.Natural,
    filterName :: Lude.Maybe Lude.Text,
    logGroupName :: Lude.Maybe Lude.Text,
    filterPattern :: Lude.Maybe Lude.Text,
    metricTransformations ::
      Lude.Maybe (Lude.NonEmpty MetricTransformation)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricFilter' with the minimum fields required to make a request.
--
-- * 'creationTime' - The creation time of the metric filter, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
-- * 'filterName' - The name of the metric filter.
-- * 'filterPattern' - Undocumented field.
-- * 'logGroupName' - The name of the log group.
-- * 'metricTransformations' - The metric transformations.
mkMetricFilter ::
  MetricFilter
mkMetricFilter =
  MetricFilter'
    { creationTime = Lude.Nothing,
      filterName = Lude.Nothing,
      logGroupName = Lude.Nothing,
      filterPattern = Lude.Nothing,
      metricTransformations = Lude.Nothing
    }

-- | The creation time of the metric filter, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfCreationTime :: Lens.Lens' MetricFilter (Lude.Maybe Lude.Natural)
mfCreationTime = Lens.lens (creationTime :: MetricFilter -> Lude.Maybe Lude.Natural) (\s a -> s {creationTime = a} :: MetricFilter)
{-# DEPRECATED mfCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The name of the metric filter.
--
-- /Note:/ Consider using 'filterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfFilterName :: Lens.Lens' MetricFilter (Lude.Maybe Lude.Text)
mfFilterName = Lens.lens (filterName :: MetricFilter -> Lude.Maybe Lude.Text) (\s a -> s {filterName = a} :: MetricFilter)
{-# DEPRECATED mfFilterName "Use generic-lens or generic-optics with 'filterName' instead." #-}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfLogGroupName :: Lens.Lens' MetricFilter (Lude.Maybe Lude.Text)
mfLogGroupName = Lens.lens (logGroupName :: MetricFilter -> Lude.Maybe Lude.Text) (\s a -> s {logGroupName = a} :: MetricFilter)
{-# DEPRECATED mfLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'filterPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfFilterPattern :: Lens.Lens' MetricFilter (Lude.Maybe Lude.Text)
mfFilterPattern = Lens.lens (filterPattern :: MetricFilter -> Lude.Maybe Lude.Text) (\s a -> s {filterPattern = a} :: MetricFilter)
{-# DEPRECATED mfFilterPattern "Use generic-lens or generic-optics with 'filterPattern' instead." #-}

-- | The metric transformations.
--
-- /Note:/ Consider using 'metricTransformations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfMetricTransformations :: Lens.Lens' MetricFilter (Lude.Maybe (Lude.NonEmpty MetricTransformation))
mfMetricTransformations = Lens.lens (metricTransformations :: MetricFilter -> Lude.Maybe (Lude.NonEmpty MetricTransformation)) (\s a -> s {metricTransformations = a} :: MetricFilter)
{-# DEPRECATED mfMetricTransformations "Use generic-lens or generic-optics with 'metricTransformations' instead." #-}

instance Lude.FromJSON MetricFilter where
  parseJSON =
    Lude.withObject
      "MetricFilter"
      ( \x ->
          MetricFilter'
            Lude.<$> (x Lude..:? "creationTime")
            Lude.<*> (x Lude..:? "filterName")
            Lude.<*> (x Lude..:? "logGroupName")
            Lude.<*> (x Lude..:? "filterPattern")
            Lude.<*> (x Lude..:? "metricTransformations")
      )
