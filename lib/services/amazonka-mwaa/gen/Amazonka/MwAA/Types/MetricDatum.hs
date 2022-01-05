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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MwAA.Types.MetricDatum where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MwAA.Types.Dimension
import Amazonka.MwAA.Types.StatisticSet
import Amazonka.MwAA.Types.Unit
import qualified Amazonka.Prelude as Prelude

-- | Internal only API.
--
-- /See:/ 'newMetricDatum' smart constructor.
data MetricDatum = MetricDatum'
  { -- | Internal only API.
    value :: Prelude.Maybe Prelude.Double,
    -- | Internal only API.
    dimensions :: Prelude.Maybe [Dimension],
    unit :: Prelude.Maybe Unit,
    -- | Internal only API.
    statisticValues :: Prelude.Maybe StatisticSet,
    -- | Internal only API.
    metricName :: Prelude.Text,
    -- | Internal only API.
    timestamp :: Core.POSIX
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
-- 'value', 'metricDatum_value' - Internal only API.
--
-- 'dimensions', 'metricDatum_dimensions' - Internal only API.
--
-- 'unit', 'metricDatum_unit' - Undocumented member.
--
-- 'statisticValues', 'metricDatum_statisticValues' - Internal only API.
--
-- 'metricName', 'metricDatum_metricName' - Internal only API.
--
-- 'timestamp', 'metricDatum_timestamp' - Internal only API.
newMetricDatum ::
  -- | 'metricName'
  Prelude.Text ->
  -- | 'timestamp'
  Prelude.UTCTime ->
  MetricDatum
newMetricDatum pMetricName_ pTimestamp_ =
  MetricDatum'
    { value = Prelude.Nothing,
      dimensions = Prelude.Nothing,
      unit = Prelude.Nothing,
      statisticValues = Prelude.Nothing,
      metricName = pMetricName_,
      timestamp = Core._Time Lens.# pTimestamp_
    }

-- | Internal only API.
metricDatum_value :: Lens.Lens' MetricDatum (Prelude.Maybe Prelude.Double)
metricDatum_value = Lens.lens (\MetricDatum' {value} -> value) (\s@MetricDatum' {} a -> s {value = a} :: MetricDatum)

-- | Internal only API.
metricDatum_dimensions :: Lens.Lens' MetricDatum (Prelude.Maybe [Dimension])
metricDatum_dimensions = Lens.lens (\MetricDatum' {dimensions} -> dimensions) (\s@MetricDatum' {} a -> s {dimensions = a} :: MetricDatum) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
metricDatum_unit :: Lens.Lens' MetricDatum (Prelude.Maybe Unit)
metricDatum_unit = Lens.lens (\MetricDatum' {unit} -> unit) (\s@MetricDatum' {} a -> s {unit = a} :: MetricDatum)

-- | Internal only API.
metricDatum_statisticValues :: Lens.Lens' MetricDatum (Prelude.Maybe StatisticSet)
metricDatum_statisticValues = Lens.lens (\MetricDatum' {statisticValues} -> statisticValues) (\s@MetricDatum' {} a -> s {statisticValues = a} :: MetricDatum)

-- | Internal only API.
metricDatum_metricName :: Lens.Lens' MetricDatum Prelude.Text
metricDatum_metricName = Lens.lens (\MetricDatum' {metricName} -> metricName) (\s@MetricDatum' {} a -> s {metricName = a} :: MetricDatum)

-- | Internal only API.
metricDatum_timestamp :: Lens.Lens' MetricDatum Prelude.UTCTime
metricDatum_timestamp = Lens.lens (\MetricDatum' {timestamp} -> timestamp) (\s@MetricDatum' {} a -> s {timestamp = a} :: MetricDatum) Prelude.. Core._Time

instance Prelude.Hashable MetricDatum where
  hashWithSalt _salt MetricDatum' {..} =
    _salt `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` statisticValues
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData MetricDatum where
  rnf MetricDatum' {..} =
    Prelude.rnf value
      `Prelude.seq` Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf statisticValues
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf timestamp

instance Core.ToJSON MetricDatum where
  toJSON MetricDatum' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Value" Core..=) Prelude.<$> value,
            ("Dimensions" Core..=) Prelude.<$> dimensions,
            ("Unit" Core..=) Prelude.<$> unit,
            ("StatisticValues" Core..=)
              Prelude.<$> statisticValues,
            Prelude.Just ("MetricName" Core..= metricName),
            Prelude.Just ("Timestamp" Core..= timestamp)
          ]
      )
