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
-- Module      : Network.AWS.Connect.Types.HistoricalMetric
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HistoricalMetric where

import Network.AWS.Connect.Types.HistoricalMetricName
import Network.AWS.Connect.Types.Statistic
import Network.AWS.Connect.Types.Threshold
import Network.AWS.Connect.Types.Unit
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about a historical metric. For a description of
-- each metric, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/historical-metrics-definitions.html Historical Metrics Definitions>
-- in the /Amazon Connect Administrator Guide/.
--
-- /See:/ 'newHistoricalMetric' smart constructor.
data HistoricalMetric = HistoricalMetric'
  { -- | The threshold for the metric, used with service level metrics.
    threshold :: Core.Maybe Threshold,
    -- | The unit for the metric.
    unit :: Core.Maybe Unit,
    -- | The name of the metric.
    name :: Core.Maybe HistoricalMetricName,
    -- | The statistic for the metric.
    statistic :: Core.Maybe Statistic
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HistoricalMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'threshold', 'historicalMetric_threshold' - The threshold for the metric, used with service level metrics.
--
-- 'unit', 'historicalMetric_unit' - The unit for the metric.
--
-- 'name', 'historicalMetric_name' - The name of the metric.
--
-- 'statistic', 'historicalMetric_statistic' - The statistic for the metric.
newHistoricalMetric ::
  HistoricalMetric
newHistoricalMetric =
  HistoricalMetric'
    { threshold = Core.Nothing,
      unit = Core.Nothing,
      name = Core.Nothing,
      statistic = Core.Nothing
    }

-- | The threshold for the metric, used with service level metrics.
historicalMetric_threshold :: Lens.Lens' HistoricalMetric (Core.Maybe Threshold)
historicalMetric_threshold = Lens.lens (\HistoricalMetric' {threshold} -> threshold) (\s@HistoricalMetric' {} a -> s {threshold = a} :: HistoricalMetric)

-- | The unit for the metric.
historicalMetric_unit :: Lens.Lens' HistoricalMetric (Core.Maybe Unit)
historicalMetric_unit = Lens.lens (\HistoricalMetric' {unit} -> unit) (\s@HistoricalMetric' {} a -> s {unit = a} :: HistoricalMetric)

-- | The name of the metric.
historicalMetric_name :: Lens.Lens' HistoricalMetric (Core.Maybe HistoricalMetricName)
historicalMetric_name = Lens.lens (\HistoricalMetric' {name} -> name) (\s@HistoricalMetric' {} a -> s {name = a} :: HistoricalMetric)

-- | The statistic for the metric.
historicalMetric_statistic :: Lens.Lens' HistoricalMetric (Core.Maybe Statistic)
historicalMetric_statistic = Lens.lens (\HistoricalMetric' {statistic} -> statistic) (\s@HistoricalMetric' {} a -> s {statistic = a} :: HistoricalMetric)

instance Core.FromJSON HistoricalMetric where
  parseJSON =
    Core.withObject
      "HistoricalMetric"
      ( \x ->
          HistoricalMetric'
            Core.<$> (x Core..:? "Threshold")
            Core.<*> (x Core..:? "Unit")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Statistic")
      )

instance Core.Hashable HistoricalMetric

instance Core.NFData HistoricalMetric

instance Core.ToJSON HistoricalMetric where
  toJSON HistoricalMetric' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Threshold" Core..=) Core.<$> threshold,
            ("Unit" Core..=) Core.<$> unit,
            ("Name" Core..=) Core.<$> name,
            ("Statistic" Core..=) Core.<$> statistic
          ]
      )
