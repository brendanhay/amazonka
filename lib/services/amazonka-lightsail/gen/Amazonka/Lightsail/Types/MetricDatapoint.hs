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
-- Module      : Amazonka.Lightsail.Types.MetricDatapoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.MetricDatapoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.MetricUnit
import qualified Amazonka.Prelude as Prelude

-- | Describes the metric data point.
--
-- /See:/ 'newMetricDatapoint' smart constructor.
data MetricDatapoint = MetricDatapoint'
  { -- | The average.
    average :: Prelude.Maybe Prelude.Double,
    -- | The maximum.
    maximum :: Prelude.Maybe Prelude.Double,
    -- | The minimum.
    minimum :: Prelude.Maybe Prelude.Double,
    -- | The sample count.
    sampleCount :: Prelude.Maybe Prelude.Double,
    -- | The sum.
    sum :: Prelude.Maybe Prelude.Double,
    -- | The timestamp (e.g., @1479816991.349@).
    timestamp :: Prelude.Maybe Data.POSIX,
    -- | The unit.
    unit :: Prelude.Maybe MetricUnit
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricDatapoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'average', 'metricDatapoint_average' - The average.
--
-- 'maximum', 'metricDatapoint_maximum' - The maximum.
--
-- 'minimum', 'metricDatapoint_minimum' - The minimum.
--
-- 'sampleCount', 'metricDatapoint_sampleCount' - The sample count.
--
-- 'sum', 'metricDatapoint_sum' - The sum.
--
-- 'timestamp', 'metricDatapoint_timestamp' - The timestamp (e.g., @1479816991.349@).
--
-- 'unit', 'metricDatapoint_unit' - The unit.
newMetricDatapoint ::
  MetricDatapoint
newMetricDatapoint =
  MetricDatapoint'
    { average = Prelude.Nothing,
      maximum = Prelude.Nothing,
      minimum = Prelude.Nothing,
      sampleCount = Prelude.Nothing,
      sum = Prelude.Nothing,
      timestamp = Prelude.Nothing,
      unit = Prelude.Nothing
    }

-- | The average.
metricDatapoint_average :: Lens.Lens' MetricDatapoint (Prelude.Maybe Prelude.Double)
metricDatapoint_average = Lens.lens (\MetricDatapoint' {average} -> average) (\s@MetricDatapoint' {} a -> s {average = a} :: MetricDatapoint)

-- | The maximum.
metricDatapoint_maximum :: Lens.Lens' MetricDatapoint (Prelude.Maybe Prelude.Double)
metricDatapoint_maximum = Lens.lens (\MetricDatapoint' {maximum} -> maximum) (\s@MetricDatapoint' {} a -> s {maximum = a} :: MetricDatapoint)

-- | The minimum.
metricDatapoint_minimum :: Lens.Lens' MetricDatapoint (Prelude.Maybe Prelude.Double)
metricDatapoint_minimum = Lens.lens (\MetricDatapoint' {minimum} -> minimum) (\s@MetricDatapoint' {} a -> s {minimum = a} :: MetricDatapoint)

-- | The sample count.
metricDatapoint_sampleCount :: Lens.Lens' MetricDatapoint (Prelude.Maybe Prelude.Double)
metricDatapoint_sampleCount = Lens.lens (\MetricDatapoint' {sampleCount} -> sampleCount) (\s@MetricDatapoint' {} a -> s {sampleCount = a} :: MetricDatapoint)

-- | The sum.
metricDatapoint_sum :: Lens.Lens' MetricDatapoint (Prelude.Maybe Prelude.Double)
metricDatapoint_sum = Lens.lens (\MetricDatapoint' {sum} -> sum) (\s@MetricDatapoint' {} a -> s {sum = a} :: MetricDatapoint)

-- | The timestamp (e.g., @1479816991.349@).
metricDatapoint_timestamp :: Lens.Lens' MetricDatapoint (Prelude.Maybe Prelude.UTCTime)
metricDatapoint_timestamp = Lens.lens (\MetricDatapoint' {timestamp} -> timestamp) (\s@MetricDatapoint' {} a -> s {timestamp = a} :: MetricDatapoint) Prelude.. Lens.mapping Data._Time

-- | The unit.
metricDatapoint_unit :: Lens.Lens' MetricDatapoint (Prelude.Maybe MetricUnit)
metricDatapoint_unit = Lens.lens (\MetricDatapoint' {unit} -> unit) (\s@MetricDatapoint' {} a -> s {unit = a} :: MetricDatapoint)

instance Data.FromJSON MetricDatapoint where
  parseJSON =
    Data.withObject
      "MetricDatapoint"
      ( \x ->
          MetricDatapoint'
            Prelude.<$> (x Data..:? "average")
            Prelude.<*> (x Data..:? "maximum")
            Prelude.<*> (x Data..:? "minimum")
            Prelude.<*> (x Data..:? "sampleCount")
            Prelude.<*> (x Data..:? "sum")
            Prelude.<*> (x Data..:? "timestamp")
            Prelude.<*> (x Data..:? "unit")
      )

instance Prelude.Hashable MetricDatapoint where
  hashWithSalt _salt MetricDatapoint' {..} =
    _salt
      `Prelude.hashWithSalt` average
      `Prelude.hashWithSalt` maximum
      `Prelude.hashWithSalt` minimum
      `Prelude.hashWithSalt` sampleCount
      `Prelude.hashWithSalt` sum
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` unit

instance Prelude.NFData MetricDatapoint where
  rnf MetricDatapoint' {..} =
    Prelude.rnf average
      `Prelude.seq` Prelude.rnf maximum
      `Prelude.seq` Prelude.rnf minimum
      `Prelude.seq` Prelude.rnf sampleCount
      `Prelude.seq` Prelude.rnf sum
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf unit
