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
-- Module      : Amazonka.CloudWatch.Types.MetricDatum
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.MetricDatum where

import Amazonka.CloudWatch.Types.Dimension
import Amazonka.CloudWatch.Types.StandardUnit
import Amazonka.CloudWatch.Types.StatisticSet
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Encapsulates the information sent to either create a metric or add new
-- values to be aggregated into an existing metric.
--
-- /See:/ 'newMetricDatum' smart constructor.
data MetricDatum = MetricDatum'
  { -- | Array of numbers that is used along with the @Values@ array. Each number
    -- in the @Count@ array is the number of times the corresponding value in
    -- the @Values@ array occurred during the period.
    --
    -- If you omit the @Counts@ array, the default of 1 is used as the value
    -- for each count. If you include a @Counts@ array, it must include the
    -- same amount of values as the @Values@ array.
    counts :: Prelude.Maybe [Prelude.Double],
    -- | The dimensions associated with the metric.
    dimensions :: Prelude.Maybe [Dimension],
    -- | The statistical values for the metric.
    statisticValues :: Prelude.Maybe StatisticSet,
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
    storageResolution :: Prelude.Maybe Prelude.Natural,
    -- | The time the metric data was received, expressed as the number of
    -- milliseconds since Jan 1, 1970 00:00:00 UTC.
    timestamp :: Prelude.Maybe Data.ISO8601,
    -- | When you are using a @Put@ operation, this defines what unit you want to
    -- use when storing the metric.
    --
    -- In a @Get@ operation, this displays the unit that is used for the
    -- metric.
    unit :: Prelude.Maybe StandardUnit,
    -- | The value for the metric.
    --
    -- Although the parameter accepts numbers of type Double, CloudWatch
    -- rejects values that are either too small or too large. Values must be in
    -- the range of -2^360 to 2^360. In addition, special values (for example,
    -- NaN, +Infinity, -Infinity) are not supported.
    value :: Prelude.Maybe Prelude.Double,
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
    values :: Prelude.Maybe [Prelude.Double],
    -- | The name of the metric.
    metricName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricDatum' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'counts', 'metricDatum_counts' - Array of numbers that is used along with the @Values@ array. Each number
-- in the @Count@ array is the number of times the corresponding value in
-- the @Values@ array occurred during the period.
--
-- If you omit the @Counts@ array, the default of 1 is used as the value
-- for each count. If you include a @Counts@ array, it must include the
-- same amount of values as the @Values@ array.
--
-- 'dimensions', 'metricDatum_dimensions' - The dimensions associated with the metric.
--
-- 'statisticValues', 'metricDatum_statisticValues' - The statistical values for the metric.
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
-- 'timestamp', 'metricDatum_timestamp' - The time the metric data was received, expressed as the number of
-- milliseconds since Jan 1, 1970 00:00:00 UTC.
--
-- 'unit', 'metricDatum_unit' - When you are using a @Put@ operation, this defines what unit you want to
-- use when storing the metric.
--
-- In a @Get@ operation, this displays the unit that is used for the
-- metric.
--
-- 'value', 'metricDatum_value' - The value for the metric.
--
-- Although the parameter accepts numbers of type Double, CloudWatch
-- rejects values that are either too small or too large. Values must be in
-- the range of -2^360 to 2^360. In addition, special values (for example,
-- NaN, +Infinity, -Infinity) are not supported.
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
-- 'metricName', 'metricDatum_metricName' - The name of the metric.
newMetricDatum ::
  -- | 'metricName'
  Prelude.Text ->
  MetricDatum
newMetricDatum pMetricName_ =
  MetricDatum'
    { counts = Prelude.Nothing,
      dimensions = Prelude.Nothing,
      statisticValues = Prelude.Nothing,
      storageResolution = Prelude.Nothing,
      timestamp = Prelude.Nothing,
      unit = Prelude.Nothing,
      value = Prelude.Nothing,
      values = Prelude.Nothing,
      metricName = pMetricName_
    }

-- | Array of numbers that is used along with the @Values@ array. Each number
-- in the @Count@ array is the number of times the corresponding value in
-- the @Values@ array occurred during the period.
--
-- If you omit the @Counts@ array, the default of 1 is used as the value
-- for each count. If you include a @Counts@ array, it must include the
-- same amount of values as the @Values@ array.
metricDatum_counts :: Lens.Lens' MetricDatum (Prelude.Maybe [Prelude.Double])
metricDatum_counts = Lens.lens (\MetricDatum' {counts} -> counts) (\s@MetricDatum' {} a -> s {counts = a} :: MetricDatum) Prelude.. Lens.mapping Lens.coerced

-- | The dimensions associated with the metric.
metricDatum_dimensions :: Lens.Lens' MetricDatum (Prelude.Maybe [Dimension])
metricDatum_dimensions = Lens.lens (\MetricDatum' {dimensions} -> dimensions) (\s@MetricDatum' {} a -> s {dimensions = a} :: MetricDatum) Prelude.. Lens.mapping Lens.coerced

-- | The statistical values for the metric.
metricDatum_statisticValues :: Lens.Lens' MetricDatum (Prelude.Maybe StatisticSet)
metricDatum_statisticValues = Lens.lens (\MetricDatum' {statisticValues} -> statisticValues) (\s@MetricDatum' {} a -> s {statisticValues = a} :: MetricDatum)

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
metricDatum_storageResolution :: Lens.Lens' MetricDatum (Prelude.Maybe Prelude.Natural)
metricDatum_storageResolution = Lens.lens (\MetricDatum' {storageResolution} -> storageResolution) (\s@MetricDatum' {} a -> s {storageResolution = a} :: MetricDatum)

-- | The time the metric data was received, expressed as the number of
-- milliseconds since Jan 1, 1970 00:00:00 UTC.
metricDatum_timestamp :: Lens.Lens' MetricDatum (Prelude.Maybe Prelude.UTCTime)
metricDatum_timestamp = Lens.lens (\MetricDatum' {timestamp} -> timestamp) (\s@MetricDatum' {} a -> s {timestamp = a} :: MetricDatum) Prelude.. Lens.mapping Data._Time

-- | When you are using a @Put@ operation, this defines what unit you want to
-- use when storing the metric.
--
-- In a @Get@ operation, this displays the unit that is used for the
-- metric.
metricDatum_unit :: Lens.Lens' MetricDatum (Prelude.Maybe StandardUnit)
metricDatum_unit = Lens.lens (\MetricDatum' {unit} -> unit) (\s@MetricDatum' {} a -> s {unit = a} :: MetricDatum)

-- | The value for the metric.
--
-- Although the parameter accepts numbers of type Double, CloudWatch
-- rejects values that are either too small or too large. Values must be in
-- the range of -2^360 to 2^360. In addition, special values (for example,
-- NaN, +Infinity, -Infinity) are not supported.
metricDatum_value :: Lens.Lens' MetricDatum (Prelude.Maybe Prelude.Double)
metricDatum_value = Lens.lens (\MetricDatum' {value} -> value) (\s@MetricDatum' {} a -> s {value = a} :: MetricDatum)

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
metricDatum_values :: Lens.Lens' MetricDatum (Prelude.Maybe [Prelude.Double])
metricDatum_values = Lens.lens (\MetricDatum' {values} -> values) (\s@MetricDatum' {} a -> s {values = a} :: MetricDatum) Prelude.. Lens.mapping Lens.coerced

-- | The name of the metric.
metricDatum_metricName :: Lens.Lens' MetricDatum Prelude.Text
metricDatum_metricName = Lens.lens (\MetricDatum' {metricName} -> metricName) (\s@MetricDatum' {} a -> s {metricName = a} :: MetricDatum)

instance Prelude.Hashable MetricDatum where
  hashWithSalt _salt MetricDatum' {..} =
    _salt `Prelude.hashWithSalt` counts
      `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` statisticValues
      `Prelude.hashWithSalt` storageResolution
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` metricName

instance Prelude.NFData MetricDatum where
  rnf MetricDatum' {..} =
    Prelude.rnf counts
      `Prelude.seq` Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf statisticValues
      `Prelude.seq` Prelude.rnf storageResolution
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf values
      `Prelude.seq` Prelude.rnf metricName

instance Data.ToQuery MetricDatum where
  toQuery MetricDatum' {..} =
    Prelude.mconcat
      [ "Counts"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> counts),
        "Dimensions"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> dimensions),
        "StatisticValues" Data.=: statisticValues,
        "StorageResolution" Data.=: storageResolution,
        "Timestamp" Data.=: timestamp,
        "Unit" Data.=: unit,
        "Value" Data.=: value,
        "Values"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> values),
        "MetricName" Data.=: metricName
      ]
