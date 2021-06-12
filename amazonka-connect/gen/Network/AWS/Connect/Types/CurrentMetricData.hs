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
-- Module      : Network.AWS.Connect.Types.CurrentMetricData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.CurrentMetricData where

import Network.AWS.Connect.Types.CurrentMetric
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the data for a real-time metric.
--
-- /See:/ 'newCurrentMetricData' smart constructor.
data CurrentMetricData = CurrentMetricData'
  { -- | Information about the metric.
    metric :: Core.Maybe CurrentMetric,
    -- | The value of the metric.
    value :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CurrentMetricData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metric', 'currentMetricData_metric' - Information about the metric.
--
-- 'value', 'currentMetricData_value' - The value of the metric.
newCurrentMetricData ::
  CurrentMetricData
newCurrentMetricData =
  CurrentMetricData'
    { metric = Core.Nothing,
      value = Core.Nothing
    }

-- | Information about the metric.
currentMetricData_metric :: Lens.Lens' CurrentMetricData (Core.Maybe CurrentMetric)
currentMetricData_metric = Lens.lens (\CurrentMetricData' {metric} -> metric) (\s@CurrentMetricData' {} a -> s {metric = a} :: CurrentMetricData)

-- | The value of the metric.
currentMetricData_value :: Lens.Lens' CurrentMetricData (Core.Maybe Core.Double)
currentMetricData_value = Lens.lens (\CurrentMetricData' {value} -> value) (\s@CurrentMetricData' {} a -> s {value = a} :: CurrentMetricData)

instance Core.FromJSON CurrentMetricData where
  parseJSON =
    Core.withObject
      "CurrentMetricData"
      ( \x ->
          CurrentMetricData'
            Core.<$> (x Core..:? "Metric") Core.<*> (x Core..:? "Value")
      )

instance Core.Hashable CurrentMetricData

instance Core.NFData CurrentMetricData
