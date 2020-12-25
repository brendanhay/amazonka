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
    mdMetricName,
    mdCounts,
    mdDimensions,
    mdStatisticValues,
    mdStorageResolution,
    mdTimestamp,
    mdUnit,
    mdValue,
    mdValues,
  )
where

import qualified Network.AWS.CloudWatch.Types.Dimension as Types
import qualified Network.AWS.CloudWatch.Types.MetricName as Types
import qualified Network.AWS.CloudWatch.Types.StandardUnit as Types
import qualified Network.AWS.CloudWatch.Types.StatisticSet as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Encapsulates the information sent to either create a metric or add new values to be aggregated into an existing metric.
--
-- /See:/ 'mkMetricDatum' smart constructor.
data MetricDatum = MetricDatum'
  { -- | The name of the metric.
    metricName :: Types.MetricName,
    -- | Array of numbers that is used along with the @Values@ array. Each number in the @Count@ array is the number of times the corresponding value in the @Values@ array occurred during the period.
    --
    -- If you omit the @Counts@ array, the default of 1 is used as the value for each count. If you include a @Counts@ array, it must include the same amount of values as the @Values@ array.
    counts :: Core.Maybe [Core.Double],
    -- | The dimensions associated with the metric.
    dimensions :: Core.Maybe [Types.Dimension],
    -- | The statistical values for the metric.
    statisticValues :: Core.Maybe Types.StatisticSet,
    -- | Valid values are 1 and 60. Setting this to 1 specifies this metric as a high-resolution metric, so that CloudWatch stores the metric with sub-minute resolution down to one second. Setting this to 60 specifies this metric as a regular-resolution metric, which CloudWatch stores at 1-minute resolution. Currently, high resolution is available only for custom metrics. For more information about high-resolution metrics, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html#high-resolution-metrics High-Resolution Metrics> in the /Amazon CloudWatch User Guide/ .
    --
    -- This field is optional, if you do not specify it the default of 60 is used.
    storageResolution :: Core.Maybe Core.Natural,
    -- | The time the metric data was received, expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC.
    timestamp :: Core.Maybe Core.UTCTime,
    -- | When you are using a @Put@ operation, this defines what unit you want to use when storing the metric.
    --
    -- In a @Get@ operation, this displays the unit that is used for the metric.
    unit :: Core.Maybe Types.StandardUnit,
    -- | The value for the metric.
    --
    -- Although the parameter accepts numbers of type Double, CloudWatch rejects values that are either too small or too large. Values must be in the range of -2^360 to 2^360. In addition, special values (for example, NaN, +Infinity, -Infinity) are not supported.
    value :: Core.Maybe Core.Double,
    -- | Array of numbers representing the values for the metric during the period. Each unique value is listed just once in this array, and the corresponding number in the @Counts@ array specifies the number of times that value occurred during the period. You can include up to 150 unique values in each @PutMetricData@ action that specifies a @Values@ array.
    --
    -- Although the @Values@ array accepts numbers of type @Double@ , CloudWatch rejects values that are either too small or too large. Values must be in the range of -2^360 to 2^360. In addition, special values (for example, NaN, +Infinity, -Infinity) are not supported.
    values :: Core.Maybe [Core.Double]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'MetricDatum' value with any optional fields omitted.
mkMetricDatum ::
  -- | 'metricName'
  Types.MetricName ->
  MetricDatum
mkMetricDatum metricName =
  MetricDatum'
    { metricName,
      counts = Core.Nothing,
      dimensions = Core.Nothing,
      statisticValues = Core.Nothing,
      storageResolution = Core.Nothing,
      timestamp = Core.Nothing,
      unit = Core.Nothing,
      value = Core.Nothing,
      values = Core.Nothing
    }

-- | The name of the metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdMetricName :: Lens.Lens' MetricDatum Types.MetricName
mdMetricName = Lens.field @"metricName"
{-# DEPRECATED mdMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | Array of numbers that is used along with the @Values@ array. Each number in the @Count@ array is the number of times the corresponding value in the @Values@ array occurred during the period.
--
-- If you omit the @Counts@ array, the default of 1 is used as the value for each count. If you include a @Counts@ array, it must include the same amount of values as the @Values@ array.
--
-- /Note:/ Consider using 'counts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdCounts :: Lens.Lens' MetricDatum (Core.Maybe [Core.Double])
mdCounts = Lens.field @"counts"
{-# DEPRECATED mdCounts "Use generic-lens or generic-optics with 'counts' instead." #-}

-- | The dimensions associated with the metric.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdDimensions :: Lens.Lens' MetricDatum (Core.Maybe [Types.Dimension])
mdDimensions = Lens.field @"dimensions"
{-# DEPRECATED mdDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The statistical values for the metric.
--
-- /Note:/ Consider using 'statisticValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdStatisticValues :: Lens.Lens' MetricDatum (Core.Maybe Types.StatisticSet)
mdStatisticValues = Lens.field @"statisticValues"
{-# DEPRECATED mdStatisticValues "Use generic-lens or generic-optics with 'statisticValues' instead." #-}

-- | Valid values are 1 and 60. Setting this to 1 specifies this metric as a high-resolution metric, so that CloudWatch stores the metric with sub-minute resolution down to one second. Setting this to 60 specifies this metric as a regular-resolution metric, which CloudWatch stores at 1-minute resolution. Currently, high resolution is available only for custom metrics. For more information about high-resolution metrics, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html#high-resolution-metrics High-Resolution Metrics> in the /Amazon CloudWatch User Guide/ .
--
-- This field is optional, if you do not specify it the default of 60 is used.
--
-- /Note:/ Consider using 'storageResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdStorageResolution :: Lens.Lens' MetricDatum (Core.Maybe Core.Natural)
mdStorageResolution = Lens.field @"storageResolution"
{-# DEPRECATED mdStorageResolution "Use generic-lens or generic-optics with 'storageResolution' instead." #-}

-- | The time the metric data was received, expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdTimestamp :: Lens.Lens' MetricDatum (Core.Maybe Core.UTCTime)
mdTimestamp = Lens.field @"timestamp"
{-# DEPRECATED mdTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | When you are using a @Put@ operation, this defines what unit you want to use when storing the metric.
--
-- In a @Get@ operation, this displays the unit that is used for the metric.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdUnit :: Lens.Lens' MetricDatum (Core.Maybe Types.StandardUnit)
mdUnit = Lens.field @"unit"
{-# DEPRECATED mdUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

-- | The value for the metric.
--
-- Although the parameter accepts numbers of type Double, CloudWatch rejects values that are either too small or too large. Values must be in the range of -2^360 to 2^360. In addition, special values (for example, NaN, +Infinity, -Infinity) are not supported.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdValue :: Lens.Lens' MetricDatum (Core.Maybe Core.Double)
mdValue = Lens.field @"value"
{-# DEPRECATED mdValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | Array of numbers representing the values for the metric during the period. Each unique value is listed just once in this array, and the corresponding number in the @Counts@ array specifies the number of times that value occurred during the period. You can include up to 150 unique values in each @PutMetricData@ action that specifies a @Values@ array.
--
-- Although the @Values@ array accepts numbers of type @Double@ , CloudWatch rejects values that are either too small or too large. Values must be in the range of -2^360 to 2^360. In addition, special values (for example, NaN, +Infinity, -Infinity) are not supported.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdValues :: Lens.Lens' MetricDatum (Core.Maybe [Core.Double])
mdValues = Lens.field @"values"
{-# DEPRECATED mdValues "Use generic-lens or generic-optics with 'values' instead." #-}
