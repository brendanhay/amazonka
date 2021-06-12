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
-- Module      : Network.AWS.Connect.Types.HistoricalMetricData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HistoricalMetricData where

import Network.AWS.Connect.Types.HistoricalMetric
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the data for a historical metric.
--
-- /See:/ 'newHistoricalMetricData' smart constructor.
data HistoricalMetricData = HistoricalMetricData'
  { -- | Information about the metric.
    metric :: Core.Maybe HistoricalMetric,
    -- | The value of the metric.
    value :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HistoricalMetricData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metric', 'historicalMetricData_metric' - Information about the metric.
--
-- 'value', 'historicalMetricData_value' - The value of the metric.
newHistoricalMetricData ::
  HistoricalMetricData
newHistoricalMetricData =
  HistoricalMetricData'
    { metric = Core.Nothing,
      value = Core.Nothing
    }

-- | Information about the metric.
historicalMetricData_metric :: Lens.Lens' HistoricalMetricData (Core.Maybe HistoricalMetric)
historicalMetricData_metric = Lens.lens (\HistoricalMetricData' {metric} -> metric) (\s@HistoricalMetricData' {} a -> s {metric = a} :: HistoricalMetricData)

-- | The value of the metric.
historicalMetricData_value :: Lens.Lens' HistoricalMetricData (Core.Maybe Core.Double)
historicalMetricData_value = Lens.lens (\HistoricalMetricData' {value} -> value) (\s@HistoricalMetricData' {} a -> s {value = a} :: HistoricalMetricData)

instance Core.FromJSON HistoricalMetricData where
  parseJSON =
    Core.withObject
      "HistoricalMetricData"
      ( \x ->
          HistoricalMetricData'
            Core.<$> (x Core..:? "Metric") Core.<*> (x Core..:? "Value")
      )

instance Core.Hashable HistoricalMetricData

instance Core.NFData HistoricalMetricData
