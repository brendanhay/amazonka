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
-- Module      : Amazonka.Connect.Types.CurrentMetricData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.CurrentMetricData where

import Amazonka.Connect.Types.CurrentMetric
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the data for a real-time metric.
--
-- /See:/ 'newCurrentMetricData' smart constructor.
data CurrentMetricData = CurrentMetricData'
  { -- | The value of the metric.
    value :: Prelude.Maybe Prelude.Double,
    -- | Information about the metric.
    metric :: Prelude.Maybe CurrentMetric
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CurrentMetricData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'currentMetricData_value' - The value of the metric.
--
-- 'metric', 'currentMetricData_metric' - Information about the metric.
newCurrentMetricData ::
  CurrentMetricData
newCurrentMetricData =
  CurrentMetricData'
    { value = Prelude.Nothing,
      metric = Prelude.Nothing
    }

-- | The value of the metric.
currentMetricData_value :: Lens.Lens' CurrentMetricData (Prelude.Maybe Prelude.Double)
currentMetricData_value = Lens.lens (\CurrentMetricData' {value} -> value) (\s@CurrentMetricData' {} a -> s {value = a} :: CurrentMetricData)

-- | Information about the metric.
currentMetricData_metric :: Lens.Lens' CurrentMetricData (Prelude.Maybe CurrentMetric)
currentMetricData_metric = Lens.lens (\CurrentMetricData' {metric} -> metric) (\s@CurrentMetricData' {} a -> s {metric = a} :: CurrentMetricData)

instance Core.FromJSON CurrentMetricData where
  parseJSON =
    Core.withObject
      "CurrentMetricData"
      ( \x ->
          CurrentMetricData'
            Prelude.<$> (x Core..:? "Value")
            Prelude.<*> (x Core..:? "Metric")
      )

instance Prelude.Hashable CurrentMetricData

instance Prelude.NFData CurrentMetricData
