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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.MetricUnit

-- | Describes the metric data point.
--
-- /See:/ 'newMetricDatapoint' smart constructor.
data MetricDatapoint = MetricDatapoint'
  { -- | The minimum.
    minimum :: Core.Maybe Core.Double,
    -- | The unit.
    unit :: Core.Maybe MetricUnit,
    -- | The sum.
    sum :: Core.Maybe Core.Double,
    -- | The sample count.
    sampleCount :: Core.Maybe Core.Double,
    -- | The timestamp (e.g., @1479816991.349@).
    timestamp :: Core.Maybe Core.POSIX,
    -- | The average.
    average :: Core.Maybe Core.Double,
    -- | The maximum.
    maximum :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { minimum = Core.Nothing,
      unit = Core.Nothing,
      sum = Core.Nothing,
      sampleCount = Core.Nothing,
      timestamp = Core.Nothing,
      average = Core.Nothing,
      maximum = Core.Nothing
    }

-- | The minimum.
metricDatapoint_minimum :: Lens.Lens' MetricDatapoint (Core.Maybe Core.Double)
metricDatapoint_minimum = Lens.lens (\MetricDatapoint' {minimum} -> minimum) (\s@MetricDatapoint' {} a -> s {minimum = a} :: MetricDatapoint)

-- | The unit.
metricDatapoint_unit :: Lens.Lens' MetricDatapoint (Core.Maybe MetricUnit)
metricDatapoint_unit = Lens.lens (\MetricDatapoint' {unit} -> unit) (\s@MetricDatapoint' {} a -> s {unit = a} :: MetricDatapoint)

-- | The sum.
metricDatapoint_sum :: Lens.Lens' MetricDatapoint (Core.Maybe Core.Double)
metricDatapoint_sum = Lens.lens (\MetricDatapoint' {sum} -> sum) (\s@MetricDatapoint' {} a -> s {sum = a} :: MetricDatapoint)

-- | The sample count.
metricDatapoint_sampleCount :: Lens.Lens' MetricDatapoint (Core.Maybe Core.Double)
metricDatapoint_sampleCount = Lens.lens (\MetricDatapoint' {sampleCount} -> sampleCount) (\s@MetricDatapoint' {} a -> s {sampleCount = a} :: MetricDatapoint)

-- | The timestamp (e.g., @1479816991.349@).
metricDatapoint_timestamp :: Lens.Lens' MetricDatapoint (Core.Maybe Core.UTCTime)
metricDatapoint_timestamp = Lens.lens (\MetricDatapoint' {timestamp} -> timestamp) (\s@MetricDatapoint' {} a -> s {timestamp = a} :: MetricDatapoint) Core.. Lens.mapping Core._Time

-- | The average.
metricDatapoint_average :: Lens.Lens' MetricDatapoint (Core.Maybe Core.Double)
metricDatapoint_average = Lens.lens (\MetricDatapoint' {average} -> average) (\s@MetricDatapoint' {} a -> s {average = a} :: MetricDatapoint)

-- | The maximum.
metricDatapoint_maximum :: Lens.Lens' MetricDatapoint (Core.Maybe Core.Double)
metricDatapoint_maximum = Lens.lens (\MetricDatapoint' {maximum} -> maximum) (\s@MetricDatapoint' {} a -> s {maximum = a} :: MetricDatapoint)

instance Core.FromJSON MetricDatapoint where
  parseJSON =
    Core.withObject
      "MetricDatapoint"
      ( \x ->
          MetricDatapoint'
            Core.<$> (x Core..:? "minimum")
            Core.<*> (x Core..:? "unit")
            Core.<*> (x Core..:? "sum")
            Core.<*> (x Core..:? "sampleCount")
            Core.<*> (x Core..:? "timestamp")
            Core.<*> (x Core..:? "average")
            Core.<*> (x Core..:? "maximum")
      )

instance Core.Hashable MetricDatapoint

instance Core.NFData MetricDatapoint
