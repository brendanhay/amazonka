{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.CloudWatchAlarmConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.CloudWatchAlarmConfiguration
  ( CloudWatchAlarmConfiguration (..),

    -- * Smart constructor
    mkCloudWatchAlarmConfiguration,

    -- * Lenses
    cwacPeriod,
    cwacEvaluationPeriods,
    cwacMetricName,
    cwacNamespace,
    cwacComparisonOperator,
    cwacThreshold,
    cwacDimensions,
    cwacStatistic,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.ComparisonOperator
import Network.AWS.Route53.Types.Dimension
import Network.AWS.Route53.Types.Statistic

-- | A complex type that contains information about the CloudWatch alarm that Amazon Route 53 is monitoring for this health check.
--
-- /See:/ 'mkCloudWatchAlarmConfiguration' smart constructor.
data CloudWatchAlarmConfiguration = CloudWatchAlarmConfiguration'
  { -- | For the metric that the CloudWatch alarm is associated with, the duration of one evaluation period in seconds.
    period :: Lude.Natural,
    -- | For the metric that the CloudWatch alarm is associated with, the number of periods that the metric is compared to the threshold.
    evaluationPeriods :: Lude.Natural,
    -- | The name of the CloudWatch metric that the alarm is associated with.
    metricName :: Lude.Text,
    -- | The namespace of the metric that the alarm is associated with. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference> in the /Amazon CloudWatch User Guide/ .
    namespace :: Lude.Text,
    -- | For the metric that the CloudWatch alarm is associated with, the arithmetic operation that is used for the comparison.
    comparisonOperator :: ComparisonOperator,
    -- | For the metric that the CloudWatch alarm is associated with, the value the metric is compared with.
    threshold :: Lude.Double,
    -- | For the metric that the CloudWatch alarm is associated with, a complex type that contains information about the dimensions for the metric. For information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference> in the /Amazon CloudWatch User Guide/ .
    dimensions :: Lude.Maybe [Dimension],
    -- | For the metric that the CloudWatch alarm is associated with, the statistic that is applied to the metric.
    statistic :: Statistic
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudWatchAlarmConfiguration' with the minimum fields required to make a request.
--
-- * 'period' - For the metric that the CloudWatch alarm is associated with, the duration of one evaluation period in seconds.
-- * 'evaluationPeriods' - For the metric that the CloudWatch alarm is associated with, the number of periods that the metric is compared to the threshold.
-- * 'metricName' - The name of the CloudWatch metric that the alarm is associated with.
-- * 'namespace' - The namespace of the metric that the alarm is associated with. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference> in the /Amazon CloudWatch User Guide/ .
-- * 'comparisonOperator' - For the metric that the CloudWatch alarm is associated with, the arithmetic operation that is used for the comparison.
-- * 'threshold' - For the metric that the CloudWatch alarm is associated with, the value the metric is compared with.
-- * 'dimensions' - For the metric that the CloudWatch alarm is associated with, a complex type that contains information about the dimensions for the metric. For information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference> in the /Amazon CloudWatch User Guide/ .
-- * 'statistic' - For the metric that the CloudWatch alarm is associated with, the statistic that is applied to the metric.
mkCloudWatchAlarmConfiguration ::
  -- | 'period'
  Lude.Natural ->
  -- | 'evaluationPeriods'
  Lude.Natural ->
  -- | 'metricName'
  Lude.Text ->
  -- | 'namespace'
  Lude.Text ->
  -- | 'comparisonOperator'
  ComparisonOperator ->
  -- | 'threshold'
  Lude.Double ->
  -- | 'statistic'
  Statistic ->
  CloudWatchAlarmConfiguration
mkCloudWatchAlarmConfiguration
  pPeriod_
  pEvaluationPeriods_
  pMetricName_
  pNamespace_
  pComparisonOperator_
  pThreshold_
  pStatistic_ =
    CloudWatchAlarmConfiguration'
      { period = pPeriod_,
        evaluationPeriods = pEvaluationPeriods_,
        metricName = pMetricName_,
        namespace = pNamespace_,
        comparisonOperator = pComparisonOperator_,
        threshold = pThreshold_,
        dimensions = Lude.Nothing,
        statistic = pStatistic_
      }

-- | For the metric that the CloudWatch alarm is associated with, the duration of one evaluation period in seconds.
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwacPeriod :: Lens.Lens' CloudWatchAlarmConfiguration Lude.Natural
cwacPeriod = Lens.lens (period :: CloudWatchAlarmConfiguration -> Lude.Natural) (\s a -> s {period = a} :: CloudWatchAlarmConfiguration)
{-# DEPRECATED cwacPeriod "Use generic-lens or generic-optics with 'period' instead." #-}

-- | For the metric that the CloudWatch alarm is associated with, the number of periods that the metric is compared to the threshold.
--
-- /Note:/ Consider using 'evaluationPeriods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwacEvaluationPeriods :: Lens.Lens' CloudWatchAlarmConfiguration Lude.Natural
cwacEvaluationPeriods = Lens.lens (evaluationPeriods :: CloudWatchAlarmConfiguration -> Lude.Natural) (\s a -> s {evaluationPeriods = a} :: CloudWatchAlarmConfiguration)
{-# DEPRECATED cwacEvaluationPeriods "Use generic-lens or generic-optics with 'evaluationPeriods' instead." #-}

-- | The name of the CloudWatch metric that the alarm is associated with.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwacMetricName :: Lens.Lens' CloudWatchAlarmConfiguration Lude.Text
cwacMetricName = Lens.lens (metricName :: CloudWatchAlarmConfiguration -> Lude.Text) (\s a -> s {metricName = a} :: CloudWatchAlarmConfiguration)
{-# DEPRECATED cwacMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The namespace of the metric that the alarm is associated with. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference> in the /Amazon CloudWatch User Guide/ .
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwacNamespace :: Lens.Lens' CloudWatchAlarmConfiguration Lude.Text
cwacNamespace = Lens.lens (namespace :: CloudWatchAlarmConfiguration -> Lude.Text) (\s a -> s {namespace = a} :: CloudWatchAlarmConfiguration)
{-# DEPRECATED cwacNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | For the metric that the CloudWatch alarm is associated with, the arithmetic operation that is used for the comparison.
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwacComparisonOperator :: Lens.Lens' CloudWatchAlarmConfiguration ComparisonOperator
cwacComparisonOperator = Lens.lens (comparisonOperator :: CloudWatchAlarmConfiguration -> ComparisonOperator) (\s a -> s {comparisonOperator = a} :: CloudWatchAlarmConfiguration)
{-# DEPRECATED cwacComparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead." #-}

-- | For the metric that the CloudWatch alarm is associated with, the value the metric is compared with.
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwacThreshold :: Lens.Lens' CloudWatchAlarmConfiguration Lude.Double
cwacThreshold = Lens.lens (threshold :: CloudWatchAlarmConfiguration -> Lude.Double) (\s a -> s {threshold = a} :: CloudWatchAlarmConfiguration)
{-# DEPRECATED cwacThreshold "Use generic-lens or generic-optics with 'threshold' instead." #-}

-- | For the metric that the CloudWatch alarm is associated with, a complex type that contains information about the dimensions for the metric. For information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference> in the /Amazon CloudWatch User Guide/ .
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwacDimensions :: Lens.Lens' CloudWatchAlarmConfiguration (Lude.Maybe [Dimension])
cwacDimensions = Lens.lens (dimensions :: CloudWatchAlarmConfiguration -> Lude.Maybe [Dimension]) (\s a -> s {dimensions = a} :: CloudWatchAlarmConfiguration)
{-# DEPRECATED cwacDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | For the metric that the CloudWatch alarm is associated with, the statistic that is applied to the metric.
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwacStatistic :: Lens.Lens' CloudWatchAlarmConfiguration Statistic
cwacStatistic = Lens.lens (statistic :: CloudWatchAlarmConfiguration -> Statistic) (\s a -> s {statistic = a} :: CloudWatchAlarmConfiguration)
{-# DEPRECATED cwacStatistic "Use generic-lens or generic-optics with 'statistic' instead." #-}

instance Lude.FromXML CloudWatchAlarmConfiguration where
  parseXML x =
    CloudWatchAlarmConfiguration'
      Lude.<$> (x Lude..@ "Period")
      Lude.<*> (x Lude..@ "EvaluationPeriods")
      Lude.<*> (x Lude..@ "MetricName")
      Lude.<*> (x Lude..@ "Namespace")
      Lude.<*> (x Lude..@ "ComparisonOperator")
      Lude.<*> (x Lude..@ "Threshold")
      Lude.<*> ( x Lude..@? "Dimensions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Dimension")
               )
      Lude.<*> (x Lude..@ "Statistic")
