{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.CloudWatchAlarmDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.CloudWatchAlarmDefinition
  ( CloudWatchAlarmDefinition (..),

    -- * Smart constructor
    mkCloudWatchAlarmDefinition,

    -- * Lenses
    cwadPeriod,
    cwadEvaluationPeriods,
    cwadMetricName,
    cwadNamespace,
    cwadComparisonOperator,
    cwadThreshold,
    cwadDimensions,
    cwadUnit,
    cwadStatistic,
  )
where

import Network.AWS.EMR.Types.ComparisonOperator
import Network.AWS.EMR.Types.MetricDimension
import Network.AWS.EMR.Types.Statistic
import Network.AWS.EMR.Types.Unit
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The definition of a CloudWatch metric alarm, which determines when an automatic scaling activity is triggered. When the defined alarm conditions are satisfied, scaling activity begins.
--
-- /See:/ 'mkCloudWatchAlarmDefinition' smart constructor.
data CloudWatchAlarmDefinition = CloudWatchAlarmDefinition'
  { -- | The period, in seconds, over which the statistic is applied. EMR CloudWatch metrics are emitted every five minutes (300 seconds), so if an EMR CloudWatch metric is specified, specify @300@ .
    period :: Lude.Int,
    -- | The number of periods, in five-minute increments, during which the alarm condition must exist before the alarm triggers automatic scaling activity. The default value is @1@ .
    evaluationPeriods :: Lude.Maybe Lude.Int,
    -- | The name of the CloudWatch metric that is watched to determine an alarm condition.
    metricName :: Lude.Text,
    -- | The namespace for the CloudWatch metric. The default is @AWS/ElasticMapReduce@ .
    namespace :: Lude.Maybe Lude.Text,
    -- | Determines how the metric specified by @MetricName@ is compared to the value specified by @Threshold@ .
    comparisonOperator :: ComparisonOperator,
    -- | The value against which the specified statistic is compared.
    threshold :: Lude.Double,
    -- | A CloudWatch metric dimension.
    dimensions :: Lude.Maybe [MetricDimension],
    -- | The unit of measure associated with the CloudWatch metric being watched. The value specified for @Unit@ must correspond to the units specified in the CloudWatch metric.
    unit :: Lude.Maybe Unit,
    -- | The statistic to apply to the metric associated with the alarm. The default is @AVERAGE@ .
    statistic :: Lude.Maybe Statistic
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudWatchAlarmDefinition' with the minimum fields required to make a request.
--
-- * 'period' - The period, in seconds, over which the statistic is applied. EMR CloudWatch metrics are emitted every five minutes (300 seconds), so if an EMR CloudWatch metric is specified, specify @300@ .
-- * 'evaluationPeriods' - The number of periods, in five-minute increments, during which the alarm condition must exist before the alarm triggers automatic scaling activity. The default value is @1@ .
-- * 'metricName' - The name of the CloudWatch metric that is watched to determine an alarm condition.
-- * 'namespace' - The namespace for the CloudWatch metric. The default is @AWS/ElasticMapReduce@ .
-- * 'comparisonOperator' - Determines how the metric specified by @MetricName@ is compared to the value specified by @Threshold@ .
-- * 'threshold' - The value against which the specified statistic is compared.
-- * 'dimensions' - A CloudWatch metric dimension.
-- * 'unit' - The unit of measure associated with the CloudWatch metric being watched. The value specified for @Unit@ must correspond to the units specified in the CloudWatch metric.
-- * 'statistic' - The statistic to apply to the metric associated with the alarm. The default is @AVERAGE@ .
mkCloudWatchAlarmDefinition ::
  -- | 'period'
  Lude.Int ->
  -- | 'metricName'
  Lude.Text ->
  -- | 'comparisonOperator'
  ComparisonOperator ->
  -- | 'threshold'
  Lude.Double ->
  CloudWatchAlarmDefinition
mkCloudWatchAlarmDefinition
  pPeriod_
  pMetricName_
  pComparisonOperator_
  pThreshold_ =
    CloudWatchAlarmDefinition'
      { period = pPeriod_,
        evaluationPeriods = Lude.Nothing,
        metricName = pMetricName_,
        namespace = Lude.Nothing,
        comparisonOperator = pComparisonOperator_,
        threshold = pThreshold_,
        dimensions = Lude.Nothing,
        unit = Lude.Nothing,
        statistic = Lude.Nothing
      }

-- | The period, in seconds, over which the statistic is applied. EMR CloudWatch metrics are emitted every five minutes (300 seconds), so if an EMR CloudWatch metric is specified, specify @300@ .
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwadPeriod :: Lens.Lens' CloudWatchAlarmDefinition Lude.Int
cwadPeriod = Lens.lens (period :: CloudWatchAlarmDefinition -> Lude.Int) (\s a -> s {period = a} :: CloudWatchAlarmDefinition)
{-# DEPRECATED cwadPeriod "Use generic-lens or generic-optics with 'period' instead." #-}

-- | The number of periods, in five-minute increments, during which the alarm condition must exist before the alarm triggers automatic scaling activity. The default value is @1@ .
--
-- /Note:/ Consider using 'evaluationPeriods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwadEvaluationPeriods :: Lens.Lens' CloudWatchAlarmDefinition (Lude.Maybe Lude.Int)
cwadEvaluationPeriods = Lens.lens (evaluationPeriods :: CloudWatchAlarmDefinition -> Lude.Maybe Lude.Int) (\s a -> s {evaluationPeriods = a} :: CloudWatchAlarmDefinition)
{-# DEPRECATED cwadEvaluationPeriods "Use generic-lens or generic-optics with 'evaluationPeriods' instead." #-}

-- | The name of the CloudWatch metric that is watched to determine an alarm condition.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwadMetricName :: Lens.Lens' CloudWatchAlarmDefinition Lude.Text
cwadMetricName = Lens.lens (metricName :: CloudWatchAlarmDefinition -> Lude.Text) (\s a -> s {metricName = a} :: CloudWatchAlarmDefinition)
{-# DEPRECATED cwadMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The namespace for the CloudWatch metric. The default is @AWS/ElasticMapReduce@ .
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwadNamespace :: Lens.Lens' CloudWatchAlarmDefinition (Lude.Maybe Lude.Text)
cwadNamespace = Lens.lens (namespace :: CloudWatchAlarmDefinition -> Lude.Maybe Lude.Text) (\s a -> s {namespace = a} :: CloudWatchAlarmDefinition)
{-# DEPRECATED cwadNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | Determines how the metric specified by @MetricName@ is compared to the value specified by @Threshold@ .
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwadComparisonOperator :: Lens.Lens' CloudWatchAlarmDefinition ComparisonOperator
cwadComparisonOperator = Lens.lens (comparisonOperator :: CloudWatchAlarmDefinition -> ComparisonOperator) (\s a -> s {comparisonOperator = a} :: CloudWatchAlarmDefinition)
{-# DEPRECATED cwadComparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead." #-}

-- | The value against which the specified statistic is compared.
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwadThreshold :: Lens.Lens' CloudWatchAlarmDefinition Lude.Double
cwadThreshold = Lens.lens (threshold :: CloudWatchAlarmDefinition -> Lude.Double) (\s a -> s {threshold = a} :: CloudWatchAlarmDefinition)
{-# DEPRECATED cwadThreshold "Use generic-lens or generic-optics with 'threshold' instead." #-}

-- | A CloudWatch metric dimension.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwadDimensions :: Lens.Lens' CloudWatchAlarmDefinition (Lude.Maybe [MetricDimension])
cwadDimensions = Lens.lens (dimensions :: CloudWatchAlarmDefinition -> Lude.Maybe [MetricDimension]) (\s a -> s {dimensions = a} :: CloudWatchAlarmDefinition)
{-# DEPRECATED cwadDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The unit of measure associated with the CloudWatch metric being watched. The value specified for @Unit@ must correspond to the units specified in the CloudWatch metric.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwadUnit :: Lens.Lens' CloudWatchAlarmDefinition (Lude.Maybe Unit)
cwadUnit = Lens.lens (unit :: CloudWatchAlarmDefinition -> Lude.Maybe Unit) (\s a -> s {unit = a} :: CloudWatchAlarmDefinition)
{-# DEPRECATED cwadUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

-- | The statistic to apply to the metric associated with the alarm. The default is @AVERAGE@ .
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwadStatistic :: Lens.Lens' CloudWatchAlarmDefinition (Lude.Maybe Statistic)
cwadStatistic = Lens.lens (statistic :: CloudWatchAlarmDefinition -> Lude.Maybe Statistic) (\s a -> s {statistic = a} :: CloudWatchAlarmDefinition)
{-# DEPRECATED cwadStatistic "Use generic-lens or generic-optics with 'statistic' instead." #-}

instance Lude.FromJSON CloudWatchAlarmDefinition where
  parseJSON =
    Lude.withObject
      "CloudWatchAlarmDefinition"
      ( \x ->
          CloudWatchAlarmDefinition'
            Lude.<$> (x Lude..: "Period")
            Lude.<*> (x Lude..:? "EvaluationPeriods")
            Lude.<*> (x Lude..: "MetricName")
            Lude.<*> (x Lude..:? "Namespace")
            Lude.<*> (x Lude..: "ComparisonOperator")
            Lude.<*> (x Lude..: "Threshold")
            Lude.<*> (x Lude..:? "Dimensions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Unit")
            Lude.<*> (x Lude..:? "Statistic")
      )

instance Lude.ToJSON CloudWatchAlarmDefinition where
  toJSON CloudWatchAlarmDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Period" Lude..= period),
            ("EvaluationPeriods" Lude..=) Lude.<$> evaluationPeriods,
            Lude.Just ("MetricName" Lude..= metricName),
            ("Namespace" Lude..=) Lude.<$> namespace,
            Lude.Just ("ComparisonOperator" Lude..= comparisonOperator),
            Lude.Just ("Threshold" Lude..= threshold),
            ("Dimensions" Lude..=) Lude.<$> dimensions,
            ("Unit" Lude..=) Lude.<$> unit,
            ("Statistic" Lude..=) Lude.<$> statistic
          ]
      )
