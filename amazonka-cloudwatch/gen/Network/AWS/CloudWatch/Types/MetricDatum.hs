{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.MetricDatum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.MetricDatum where

import Network.AWS.CloudWatch.Types.Dimension
import Network.AWS.CloudWatch.Types.StandardUnit
import Network.AWS.CloudWatch.Types.StatisticSet
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Encapsulates the information sent to either create a metric or add new
-- values to be aggregated into an existing metric.
--
-- /See:/ 'newMetricDatum' smart constructor.
data MetricDatum = MetricDatum'
  { -- | Valid values are 1 and 60. Setting this to 1 specifies this metric as a
    -- high-resolution metric, so that CloudWatch stores the metric with
    -- sub-minute resolution down to one second. Setting this to 60 specifies
    -- this metric as a regular-resolution metric, which CloudWatch stores at
    -- 1-minute resolution. Currently, high resolution is available only for
    -- custom metrics. For more information about high-resolution metrics, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html#high-resolution-metrics High-Resolution Metrics>
    -- in the /Amazon CloudWatch User Guide/.
    --
    -- This field is optional, if you do not specify it the default of 60 is
    -- used.
    storageResolution :: Core.Maybe Core.Natural,
    -- | When you are using a @Put@ operation, this defines what unit you want to
    -- use when storing the metric.
    --
    -- In a @Get@ operation, this displays the unit that is used for the
    -- metric.
    unit :: Core.Maybe StandardUnit,
    -- | Array of numbers representing the values for the metric during the
    -- period. Each unique value is listed just once in this array, and the
    -- corresponding number in the @Counts@ array specifies the number of times
    -- that value occurred during the period. You can include up to 150 unique
    -- values in each @PutMetricData@ action that specifies a @Values@ array.
    --
    -- Although the @Values@ array accepts numbers of type @Double@, CloudWatch
    -- rejects values that are either too small or too large. Values must be in
    -- the range of -2^360 to 2^360. In addition, special values (for example,
    -- NaN, +Infinity, -Infinity) are not supported.
    values :: Core.Maybe [Core.Double],
    -- | Array of numbers that is used along with the @Values@ array. Each number
    -- in the @Count@ array is the number of times the corresponding value in
    -- the @Values@ array occurred during the period.
    --
    -- If you omit the @Counts@ array, the default of 1 is used as the value
    -- for each count. If you include a @Counts@ array, it must include the
    -- same amount of values as the @Values@ array.
    counts :: Core.Maybe [Core.Double],
    -- | The time the metric data was received, expressed as the number of
    -- milliseconds since Jan 1, 1970 00:00:00 UTC.
    timestamp :: Core.Maybe Core.ISO8601,
    -- | The statistical values for the metric.
    statisticValues :: Core.Maybe StatisticSet,
    -- | The value for the metric.
    --
    -- Although the parameter accepts numbers of type Double, CloudWatch
    -- rejects values that are either too small or too large. Values must be in
    -- the range of -2^360 to 2^360. In addition, special values (for example,
    -- NaN, +Infinity, -Infinity) are not supported.
    value :: Core.Maybe Core.Double,
    -- | The dimensions associated with the metric.
    dimensions :: Core.Maybe [Dimension],
    -- | The name of the metric.
    metricName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MetricDatum' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storageResolution', 'metricDatum_storageResolution' - Valid values are 1 and 60. Setting this to 1 specifies this metric as a
-- high-resolution metric, so that CloudWatch stores the metric with
-- sub-minute resolution down to one second. Setting this to 60 specifies
-- this metric as a regular-resolution metric, which CloudWatch stores at
-- 1-minute resolution. Currently, high resolution is available only for
-- custom metrics. For more information about high-resolution metrics, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html#high-resolution-metrics High-Resolution Metrics>
-- in the /Amazon CloudWatch User Guide/.
--
-- This field is optional, if you do not specify it the default of 60 is
-- used.
--
-- 'unit', 'metricDatum_unit' - When you are using a @Put@ operation, this defines what unit you want to
-- use when storing the metric.
--
-- In a @Get@ operation, this displays the unit that is used for the
-- metric.
--
-- 'values', 'metricDatum_values' - Array of numbers representing the values for the metric during the
-- period. Each unique value is listed just once in this array, and the
-- corresponding number in the @Counts@ array specifies the number of times
-- that value occurred during the period. You can include up to 150 unique
-- values in each @PutMetricData@ action that specifies a @Values@ array.
--
-- Although the @Values@ array accepts numbers of type @Double@, CloudWatch
-- rejects values that are either too small or too large. Values must be in
-- the range of -2^360 to 2^360. In addition, special values (for example,
-- NaN, +Infinity, -Infinity) are not supported.
--
-- 'counts', 'metricDatum_counts' - Array of numbers that is used along with the @Values@ array. Each number
-- in the @Count@ array is the number of times the corresponding value in
-- the @Values@ array occurred during the period.
--
-- If you omit the @Counts@ array, the default of 1 is used as the value
-- for each count. If you include a @Counts@ array, it must include the
-- same amount of values as the @Values@ array.
--
-- 'timestamp', 'metricDatum_timestamp' - The time the metric data was received, expressed as the number of
-- milliseconds since Jan 1, 1970 00:00:00 UTC.
--
-- 'statisticValues', 'metricDatum_statisticValues' - The statistical values for the metric.
--
-- 'value', 'metricDatum_value' - The value for the metric.
--
-- Although the parameter accepts numbers of type Double, CloudWatch
-- rejects values that are either too small or too large. Values must be in
-- the range of -2^360 to 2^360. In addition, special values (for example,
-- NaN, +Infinity, -Infinity) are not supported.
--
-- 'dimensions', 'metricDatum_dimensions' - The dimensions associated with the metric.
--
-- 'metricName', 'metricDatum_metricName' - The name of the metric.
newMetricDatum ::
  -- | 'metricName'
  Core.Text ->
  MetricDatum
newMetricDatum pMetricName_ =
  MetricDatum'
    { storageResolution = Core.Nothing,
      unit = Core.Nothing,
      values = Core.Nothing,
      counts = Core.Nothing,
      timestamp = Core.Nothing,
      statisticValues = Core.Nothing,
      value = Core.Nothing,
      dimensions = Core.Nothing,
      metricName = pMetricName_
    }

-- | Valid values are 1 and 60. Setting this to 1 specifies this metric as a
-- high-resolution metric, so that CloudWatch stores the metric with
-- sub-minute resolution down to one second. Setting this to 60 specifies
-- this metric as a regular-resolution metric, which CloudWatch stores at
-- 1-minute resolution. Currently, high resolution is available only for
-- custom metrics. For more information about high-resolution metrics, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html#high-resolution-metrics High-Resolution Metrics>
-- in the /Amazon CloudWatch User Guide/.
--
-- This field is optional, if you do not specify it the default of 60 is
-- used.
metricDatum_storageResolution :: Lens.Lens' MetricDatum (Core.Maybe Core.Natural)
metricDatum_storageResolution = Lens.lens (\MetricDatum' {storageResolution} -> storageResolution) (\s@MetricDatum' {} a -> s {storageResolution = a} :: MetricDatum)

-- | When you are using a @Put@ operation, this defines what unit you want to
-- use when storing the metric.
--
-- In a @Get@ operation, this displays the unit that is used for the
-- metric.
metricDatum_unit :: Lens.Lens' MetricDatum (Core.Maybe StandardUnit)
metricDatum_unit = Lens.lens (\MetricDatum' {unit} -> unit) (\s@MetricDatum' {} a -> s {unit = a} :: MetricDatum)

-- | Array of numbers representing the values for the metric during the
-- period. Each unique value is listed just once in this array, and the
-- corresponding number in the @Counts@ array specifies the number of times
-- that value occurred during the period. You can include up to 150 unique
-- values in each @PutMetricData@ action that specifies a @Values@ array.
--
-- Although the @Values@ array accepts numbers of type @Double@, CloudWatch
-- rejects values that are either too small or too large. Values must be in
-- the range of -2^360 to 2^360. In addition, special values (for example,
-- NaN, +Infinity, -Infinity) are not supported.
metricDatum_values :: Lens.Lens' MetricDatum (Core.Maybe [Core.Double])
metricDatum_values = Lens.lens (\MetricDatum' {values} -> values) (\s@MetricDatum' {} a -> s {values = a} :: MetricDatum) Core.. Lens.mapping Lens._Coerce

-- | Array of numbers that is used along with the @Values@ array. Each number
-- in the @Count@ array is the number of times the corresponding value in
-- the @Values@ array occurred during the period.
--
-- If you omit the @Counts@ array, the default of 1 is used as the value
-- for each count. If you include a @Counts@ array, it must include the
-- same amount of values as the @Values@ array.
metricDatum_counts :: Lens.Lens' MetricDatum (Core.Maybe [Core.Double])
metricDatum_counts = Lens.lens (\MetricDatum' {counts} -> counts) (\s@MetricDatum' {} a -> s {counts = a} :: MetricDatum) Core.. Lens.mapping Lens._Coerce

-- | The time the metric data was received, expressed as the number of
-- milliseconds since Jan 1, 1970 00:00:00 UTC.
metricDatum_timestamp :: Lens.Lens' MetricDatum (Core.Maybe Core.UTCTime)
metricDatum_timestamp = Lens.lens (\MetricDatum' {timestamp} -> timestamp) (\s@MetricDatum' {} a -> s {timestamp = a} :: MetricDatum) Core.. Lens.mapping Core._Time

-- | The statistical values for the metric.
metricDatum_statisticValues :: Lens.Lens' MetricDatum (Core.Maybe StatisticSet)
metricDatum_statisticValues = Lens.lens (\MetricDatum' {statisticValues} -> statisticValues) (\s@MetricDatum' {} a -> s {statisticValues = a} :: MetricDatum)

-- | The value for the metric.
--
-- Although the parameter accepts numbers of type Double, CloudWatch
-- rejects values that are either too small or too large. Values must be in
-- the range of -2^360 to 2^360. In addition, special values (for example,
-- NaN, +Infinity, -Infinity) are not supported.
metricDatum_value :: Lens.Lens' MetricDatum (Core.Maybe Core.Double)
metricDatum_value = Lens.lens (\MetricDatum' {value} -> value) (\s@MetricDatum' {} a -> s {value = a} :: MetricDatum)

-- | The dimensions associated with the metric.
metricDatum_dimensions :: Lens.Lens' MetricDatum (Core.Maybe [Dimension])
metricDatum_dimensions = Lens.lens (\MetricDatum' {dimensions} -> dimensions) (\s@MetricDatum' {} a -> s {dimensions = a} :: MetricDatum) Core.. Lens.mapping Lens._Coerce

-- | The name of the metric.
metricDatum_metricName :: Lens.Lens' MetricDatum Core.Text
metricDatum_metricName = Lens.lens (\MetricDatum' {metricName} -> metricName) (\s@MetricDatum' {} a -> s {metricName = a} :: MetricDatum)

instance Core.Hashable MetricDatum

instance Core.NFData MetricDatum

instance Core.ToQuery MetricDatum where
  toQuery MetricDatum' {..} =
    Core.mconcat
      [ "StorageResolution" Core.=: storageResolution,
        "Unit" Core.=: unit,
        "Values"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> values),
        "Counts"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> counts),
        "Timestamp" Core.=: timestamp,
        "StatisticValues" Core.=: statisticValues,
        "Value" Core.=: value,
        "Dimensions"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> dimensions),
        "MetricName" Core.=: metricName
      ]
