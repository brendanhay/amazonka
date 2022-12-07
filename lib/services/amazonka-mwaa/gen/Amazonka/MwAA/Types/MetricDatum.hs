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
-- Module      : Amazonka.MwAA.Types.MetricDatum
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MwAA.Types.MetricDatum where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MwAA.Types.Dimension
import Amazonka.MwAA.Types.StatisticSet
import Amazonka.MwAA.Types.Unit
import qualified Amazonka.Prelude as Prelude

-- | __Internal only__. Collects Apache Airflow metrics. To learn more about
-- the metrics published to Amazon CloudWatch, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/cw-metrics.html Amazon MWAA performance metrics in Amazon CloudWatch>.
--
-- /See:/ 'newMetricDatum' smart constructor.
data MetricDatum = MetricDatum'
  { -- | __Internal only__. The statistical values for the metric.
    statisticValues :: Prelude.Maybe StatisticSet,
    -- | __Internal only__. The dimensions associated with the metric.
    dimensions :: Prelude.Maybe [Dimension],
    -- | __Internal only__. The unit used to store the metric.
    unit :: Prelude.Maybe Unit,
    -- | __Internal only__. The value for the metric.
    value :: Prelude.Maybe Prelude.Double,
    -- | __Internal only__. The name of the metric.
    metricName :: Prelude.Text,
    -- | __Internal only__. The time the metric data was received.
    timestamp :: Data.POSIX
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
-- 'statisticValues', 'metricDatum_statisticValues' - __Internal only__. The statistical values for the metric.
--
-- 'dimensions', 'metricDatum_dimensions' - __Internal only__. The dimensions associated with the metric.
--
-- 'unit', 'metricDatum_unit' - __Internal only__. The unit used to store the metric.
--
-- 'value', 'metricDatum_value' - __Internal only__. The value for the metric.
--
-- 'metricName', 'metricDatum_metricName' - __Internal only__. The name of the metric.
--
-- 'timestamp', 'metricDatum_timestamp' - __Internal only__. The time the metric data was received.
newMetricDatum ::
  -- | 'metricName'
  Prelude.Text ->
  -- | 'timestamp'
  Prelude.UTCTime ->
  MetricDatum
newMetricDatum pMetricName_ pTimestamp_ =
  MetricDatum'
    { statisticValues = Prelude.Nothing,
      dimensions = Prelude.Nothing,
      unit = Prelude.Nothing,
      value = Prelude.Nothing,
      metricName = pMetricName_,
      timestamp = Data._Time Lens.# pTimestamp_
    }

-- | __Internal only__. The statistical values for the metric.
metricDatum_statisticValues :: Lens.Lens' MetricDatum (Prelude.Maybe StatisticSet)
metricDatum_statisticValues = Lens.lens (\MetricDatum' {statisticValues} -> statisticValues) (\s@MetricDatum' {} a -> s {statisticValues = a} :: MetricDatum)

-- | __Internal only__. The dimensions associated with the metric.
metricDatum_dimensions :: Lens.Lens' MetricDatum (Prelude.Maybe [Dimension])
metricDatum_dimensions = Lens.lens (\MetricDatum' {dimensions} -> dimensions) (\s@MetricDatum' {} a -> s {dimensions = a} :: MetricDatum) Prelude.. Lens.mapping Lens.coerced

-- | __Internal only__. The unit used to store the metric.
metricDatum_unit :: Lens.Lens' MetricDatum (Prelude.Maybe Unit)
metricDatum_unit = Lens.lens (\MetricDatum' {unit} -> unit) (\s@MetricDatum' {} a -> s {unit = a} :: MetricDatum)

-- | __Internal only__. The value for the metric.
metricDatum_value :: Lens.Lens' MetricDatum (Prelude.Maybe Prelude.Double)
metricDatum_value = Lens.lens (\MetricDatum' {value} -> value) (\s@MetricDatum' {} a -> s {value = a} :: MetricDatum)

-- | __Internal only__. The name of the metric.
metricDatum_metricName :: Lens.Lens' MetricDatum Prelude.Text
metricDatum_metricName = Lens.lens (\MetricDatum' {metricName} -> metricName) (\s@MetricDatum' {} a -> s {metricName = a} :: MetricDatum)

-- | __Internal only__. The time the metric data was received.
metricDatum_timestamp :: Lens.Lens' MetricDatum Prelude.UTCTime
metricDatum_timestamp = Lens.lens (\MetricDatum' {timestamp} -> timestamp) (\s@MetricDatum' {} a -> s {timestamp = a} :: MetricDatum) Prelude.. Data._Time

instance Prelude.Hashable MetricDatum where
  hashWithSalt _salt MetricDatum' {..} =
    _salt `Prelude.hashWithSalt` statisticValues
      `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData MetricDatum where
  rnf MetricDatum' {..} =
    Prelude.rnf statisticValues
      `Prelude.seq` Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf timestamp

instance Data.ToJSON MetricDatum where
  toJSON MetricDatum' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StatisticValues" Data..=)
              Prelude.<$> statisticValues,
            ("Dimensions" Data..=) Prelude.<$> dimensions,
            ("Unit" Data..=) Prelude.<$> unit,
            ("Value" Data..=) Prelude.<$> value,
            Prelude.Just ("MetricName" Data..= metricName),
            Prelude.Just ("Timestamp" Data..= timestamp)
          ]
      )
