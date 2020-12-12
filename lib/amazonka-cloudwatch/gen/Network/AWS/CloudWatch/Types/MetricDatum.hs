{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.MetricDatum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.MetricDatum
  ( MetricDatum (..),

    -- * Smart constructor
    mkMetricDatum,

    -- * Lenses
    mdValues,
    mdCounts,
    mdValue,
    mdStorageResolution,
    mdDimensions,
    mdUnit,
    mdTimestamp,
    mdStatisticValues,
    mdMetricName,
  )
where

import Network.AWS.CloudWatch.Types.Dimension
import Network.AWS.CloudWatch.Types.StandardUnit
import Network.AWS.CloudWatch.Types.StatisticSet
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Encapsulates the information sent to either create a metric or add new values to be aggregated into an existing metric.
--
-- /See:/ 'mkMetricDatum' smart constructor.
data MetricDatum = MetricDatum'
  { values :: Lude.Maybe [Lude.Double],
    counts :: Lude.Maybe [Lude.Double],
    value :: Lude.Maybe Lude.Double,
    storageResolution :: Lude.Maybe Lude.Natural,
    dimensions :: Lude.Maybe [Dimension],
    unit :: Lude.Maybe StandardUnit,
    timestamp :: Lude.Maybe Lude.DateTime,
    statisticValues :: Lude.Maybe StatisticSet,
    metricName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricDatum' with the minimum fields required to make a request.
--
-- * 'counts' - Array of numbers that is used along with the @Values@ array. Each number in the @Count@ array is the number of times the corresponding value in the @Values@ array occurred during the period.
--
-- If you omit the @Counts@ array, the default of 1 is used as the value for each count. If you include a @Counts@ array, it must include the same amount of values as the @Values@ array.
-- * 'dimensions' - The dimensions associated with the metric.
-- * 'metricName' - The name of the metric.
-- * 'statisticValues' - The statistical values for the metric.
-- * 'storageResolution' - Valid values are 1 and 60. Setting this to 1 specifies this metric as a high-resolution metric, so that CloudWatch stores the metric with sub-minute resolution down to one second. Setting this to 60 specifies this metric as a regular-resolution metric, which CloudWatch stores at 1-minute resolution. Currently, high resolution is available only for custom metrics. For more information about high-resolution metrics, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html#high-resolution-metrics High-Resolution Metrics> in the /Amazon CloudWatch User Guide/ .
--
-- This field is optional, if you do not specify it the default of 60 is used.
-- * 'timestamp' - The time the metric data was received, expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC.
-- * 'unit' - When you are using a @Put@ operation, this defines what unit you want to use when storing the metric.
--
-- In a @Get@ operation, this displays the unit that is used for the metric.
-- * 'value' - The value for the metric.
--
-- Although the parameter accepts numbers of type Double, CloudWatch rejects values that are either too small or too large. Values must be in the range of -2^360 to 2^360. In addition, special values (for example, NaN, +Infinity, -Infinity) are not supported.
-- * 'values' - Array of numbers representing the values for the metric during the period. Each unique value is listed just once in this array, and the corresponding number in the @Counts@ array specifies the number of times that value occurred during the period. You can include up to 150 unique values in each @PutMetricData@ action that specifies a @Values@ array.
--
-- Although the @Values@ array accepts numbers of type @Double@ , CloudWatch rejects values that are either too small or too large. Values must be in the range of -2^360 to 2^360. In addition, special values (for example, NaN, +Infinity, -Infinity) are not supported.
mkMetricDatum ::
  -- | 'metricName'
  Lude.Text ->
  MetricDatum
mkMetricDatum pMetricName_ =
  MetricDatum'
    { values = Lude.Nothing,
      counts = Lude.Nothing,
      value = Lude.Nothing,
      storageResolution = Lude.Nothing,
      dimensions = Lude.Nothing,
      unit = Lude.Nothing,
      timestamp = Lude.Nothing,
      statisticValues = Lude.Nothing,
      metricName = pMetricName_
    }

-- | Array of numbers representing the values for the metric during the period. Each unique value is listed just once in this array, and the corresponding number in the @Counts@ array specifies the number of times that value occurred during the period. You can include up to 150 unique values in each @PutMetricData@ action that specifies a @Values@ array.
--
-- Although the @Values@ array accepts numbers of type @Double@ , CloudWatch rejects values that are either too small or too large. Values must be in the range of -2^360 to 2^360. In addition, special values (for example, NaN, +Infinity, -Infinity) are not supported.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdValues :: Lens.Lens' MetricDatum (Lude.Maybe [Lude.Double])
mdValues = Lens.lens (values :: MetricDatum -> Lude.Maybe [Lude.Double]) (\s a -> s {values = a} :: MetricDatum)
{-# DEPRECATED mdValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | Array of numbers that is used along with the @Values@ array. Each number in the @Count@ array is the number of times the corresponding value in the @Values@ array occurred during the period.
--
-- If you omit the @Counts@ array, the default of 1 is used as the value for each count. If you include a @Counts@ array, it must include the same amount of values as the @Values@ array.
--
-- /Note:/ Consider using 'counts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdCounts :: Lens.Lens' MetricDatum (Lude.Maybe [Lude.Double])
mdCounts = Lens.lens (counts :: MetricDatum -> Lude.Maybe [Lude.Double]) (\s a -> s {counts = a} :: MetricDatum)
{-# DEPRECATED mdCounts "Use generic-lens or generic-optics with 'counts' instead." #-}

-- | The value for the metric.
--
-- Although the parameter accepts numbers of type Double, CloudWatch rejects values that are either too small or too large. Values must be in the range of -2^360 to 2^360. In addition, special values (for example, NaN, +Infinity, -Infinity) are not supported.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdValue :: Lens.Lens' MetricDatum (Lude.Maybe Lude.Double)
mdValue = Lens.lens (value :: MetricDatum -> Lude.Maybe Lude.Double) (\s a -> s {value = a} :: MetricDatum)
{-# DEPRECATED mdValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | Valid values are 1 and 60. Setting this to 1 specifies this metric as a high-resolution metric, so that CloudWatch stores the metric with sub-minute resolution down to one second. Setting this to 60 specifies this metric as a regular-resolution metric, which CloudWatch stores at 1-minute resolution. Currently, high resolution is available only for custom metrics. For more information about high-resolution metrics, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html#high-resolution-metrics High-Resolution Metrics> in the /Amazon CloudWatch User Guide/ .
--
-- This field is optional, if you do not specify it the default of 60 is used.
--
-- /Note:/ Consider using 'storageResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdStorageResolution :: Lens.Lens' MetricDatum (Lude.Maybe Lude.Natural)
mdStorageResolution = Lens.lens (storageResolution :: MetricDatum -> Lude.Maybe Lude.Natural) (\s a -> s {storageResolution = a} :: MetricDatum)
{-# DEPRECATED mdStorageResolution "Use generic-lens or generic-optics with 'storageResolution' instead." #-}

-- | The dimensions associated with the metric.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdDimensions :: Lens.Lens' MetricDatum (Lude.Maybe [Dimension])
mdDimensions = Lens.lens (dimensions :: MetricDatum -> Lude.Maybe [Dimension]) (\s a -> s {dimensions = a} :: MetricDatum)
{-# DEPRECATED mdDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | When you are using a @Put@ operation, this defines what unit you want to use when storing the metric.
--
-- In a @Get@ operation, this displays the unit that is used for the metric.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdUnit :: Lens.Lens' MetricDatum (Lude.Maybe StandardUnit)
mdUnit = Lens.lens (unit :: MetricDatum -> Lude.Maybe StandardUnit) (\s a -> s {unit = a} :: MetricDatum)
{-# DEPRECATED mdUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

-- | The time the metric data was received, expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdTimestamp :: Lens.Lens' MetricDatum (Lude.Maybe Lude.DateTime)
mdTimestamp = Lens.lens (timestamp :: MetricDatum -> Lude.Maybe Lude.DateTime) (\s a -> s {timestamp = a} :: MetricDatum)
{-# DEPRECATED mdTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The statistical values for the metric.
--
-- /Note:/ Consider using 'statisticValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdStatisticValues :: Lens.Lens' MetricDatum (Lude.Maybe StatisticSet)
mdStatisticValues = Lens.lens (statisticValues :: MetricDatum -> Lude.Maybe StatisticSet) (\s a -> s {statisticValues = a} :: MetricDatum)
{-# DEPRECATED mdStatisticValues "Use generic-lens or generic-optics with 'statisticValues' instead." #-}

-- | The name of the metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdMetricName :: Lens.Lens' MetricDatum Lude.Text
mdMetricName = Lens.lens (metricName :: MetricDatum -> Lude.Text) (\s a -> s {metricName = a} :: MetricDatum)
{-# DEPRECATED mdMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

instance Lude.ToQuery MetricDatum where
  toQuery MetricDatum' {..} =
    Lude.mconcat
      [ "Values"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> values),
        "Counts"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> counts),
        "Value" Lude.=: value,
        "StorageResolution" Lude.=: storageResolution,
        "Dimensions"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> dimensions),
        "Unit" Lude.=: unit,
        "Timestamp" Lude.=: timestamp,
        "StatisticValues" Lude.=: statisticValues,
        "MetricName" Lude.=: metricName
      ]
