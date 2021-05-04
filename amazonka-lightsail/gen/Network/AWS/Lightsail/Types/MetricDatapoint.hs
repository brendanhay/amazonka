{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Lightsail.Types.MetricDatapoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.MetricDatapoint where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.MetricUnit
import qualified Network.AWS.Prelude as Prelude

-- | Describes the metric data point.
--
-- /See:/ 'newMetricDatapoint' smart constructor.
data MetricDatapoint = MetricDatapoint'
  { -- | The minimum.
    minimum :: Prelude.Maybe Prelude.Double,
    -- | The unit.
    unit :: Prelude.Maybe MetricUnit,
    -- | The sum.
    sum :: Prelude.Maybe Prelude.Double,
    -- | The sample count.
    sampleCount :: Prelude.Maybe Prelude.Double,
    -- | The timestamp (e.g., @1479816991.349@).
    timestamp :: Prelude.Maybe Prelude.POSIX,
    -- | The average.
    average :: Prelude.Maybe Prelude.Double,
    -- | The maximum.
    maximum :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MetricDatapoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minimum', 'metricDatapoint_minimum' - The minimum.
--
-- 'unit', 'metricDatapoint_unit' - The unit.
--
-- 'sum', 'metricDatapoint_sum' - The sum.
--
-- 'sampleCount', 'metricDatapoint_sampleCount' - The sample count.
--
-- 'timestamp', 'metricDatapoint_timestamp' - The timestamp (e.g., @1479816991.349@).
--
-- 'average', 'metricDatapoint_average' - The average.
--
-- 'maximum', 'metricDatapoint_maximum' - The maximum.
newMetricDatapoint ::
  MetricDatapoint
newMetricDatapoint =
  MetricDatapoint'
    { minimum = Prelude.Nothing,
      unit = Prelude.Nothing,
      sum = Prelude.Nothing,
      sampleCount = Prelude.Nothing,
      timestamp = Prelude.Nothing,
      average = Prelude.Nothing,
      maximum = Prelude.Nothing
    }

-- | The minimum.
metricDatapoint_minimum :: Lens.Lens' MetricDatapoint (Prelude.Maybe Prelude.Double)
metricDatapoint_minimum = Lens.lens (\MetricDatapoint' {minimum} -> minimum) (\s@MetricDatapoint' {} a -> s {minimum = a} :: MetricDatapoint)

-- | The unit.
metricDatapoint_unit :: Lens.Lens' MetricDatapoint (Prelude.Maybe MetricUnit)
metricDatapoint_unit = Lens.lens (\MetricDatapoint' {unit} -> unit) (\s@MetricDatapoint' {} a -> s {unit = a} :: MetricDatapoint)

-- | The sum.
metricDatapoint_sum :: Lens.Lens' MetricDatapoint (Prelude.Maybe Prelude.Double)
metricDatapoint_sum = Lens.lens (\MetricDatapoint' {sum} -> sum) (\s@MetricDatapoint' {} a -> s {sum = a} :: MetricDatapoint)

-- | The sample count.
metricDatapoint_sampleCount :: Lens.Lens' MetricDatapoint (Prelude.Maybe Prelude.Double)
metricDatapoint_sampleCount = Lens.lens (\MetricDatapoint' {sampleCount} -> sampleCount) (\s@MetricDatapoint' {} a -> s {sampleCount = a} :: MetricDatapoint)

-- | The timestamp (e.g., @1479816991.349@).
metricDatapoint_timestamp :: Lens.Lens' MetricDatapoint (Prelude.Maybe Prelude.UTCTime)
metricDatapoint_timestamp = Lens.lens (\MetricDatapoint' {timestamp} -> timestamp) (\s@MetricDatapoint' {} a -> s {timestamp = a} :: MetricDatapoint) Prelude.. Lens.mapping Prelude._Time

-- | The average.
metricDatapoint_average :: Lens.Lens' MetricDatapoint (Prelude.Maybe Prelude.Double)
metricDatapoint_average = Lens.lens (\MetricDatapoint' {average} -> average) (\s@MetricDatapoint' {} a -> s {average = a} :: MetricDatapoint)

-- | The maximum.
metricDatapoint_maximum :: Lens.Lens' MetricDatapoint (Prelude.Maybe Prelude.Double)
metricDatapoint_maximum = Lens.lens (\MetricDatapoint' {maximum} -> maximum) (\s@MetricDatapoint' {} a -> s {maximum = a} :: MetricDatapoint)

instance Prelude.FromJSON MetricDatapoint where
  parseJSON =
    Prelude.withObject
      "MetricDatapoint"
      ( \x ->
          MetricDatapoint'
            Prelude.<$> (x Prelude..:? "minimum")
            Prelude.<*> (x Prelude..:? "unit")
            Prelude.<*> (x Prelude..:? "sum")
            Prelude.<*> (x Prelude..:? "sampleCount")
            Prelude.<*> (x Prelude..:? "timestamp")
            Prelude.<*> (x Prelude..:? "average")
            Prelude.<*> (x Prelude..:? "maximum")
      )

instance Prelude.Hashable MetricDatapoint

instance Prelude.NFData MetricDatapoint
