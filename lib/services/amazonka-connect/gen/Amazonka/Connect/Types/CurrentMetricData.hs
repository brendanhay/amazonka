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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.CurrentMetricData where

import Amazonka.Connect.Types.CurrentMetric
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the data for a real-time metric.
--
-- /See:/ 'newCurrentMetricData' smart constructor.
data CurrentMetricData = CurrentMetricData'
  { -- | Information about the metric.
    metric :: Prelude.Maybe CurrentMetric,
    -- | The value of the metric.
    value :: Prelude.Maybe Prelude.Double
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
-- 'metric', 'currentMetricData_metric' - Information about the metric.
--
-- 'value', 'currentMetricData_value' - The value of the metric.
newCurrentMetricData ::
  CurrentMetricData
newCurrentMetricData =
  CurrentMetricData'
    { metric = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Information about the metric.
currentMetricData_metric :: Lens.Lens' CurrentMetricData (Prelude.Maybe CurrentMetric)
currentMetricData_metric = Lens.lens (\CurrentMetricData' {metric} -> metric) (\s@CurrentMetricData' {} a -> s {metric = a} :: CurrentMetricData)

-- | The value of the metric.
currentMetricData_value :: Lens.Lens' CurrentMetricData (Prelude.Maybe Prelude.Double)
currentMetricData_value = Lens.lens (\CurrentMetricData' {value} -> value) (\s@CurrentMetricData' {} a -> s {value = a} :: CurrentMetricData)

instance Data.FromJSON CurrentMetricData where
  parseJSON =
    Data.withObject
      "CurrentMetricData"
      ( \x ->
          CurrentMetricData'
            Prelude.<$> (x Data..:? "Metric")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable CurrentMetricData where
  hashWithSalt _salt CurrentMetricData' {..} =
    _salt
      `Prelude.hashWithSalt` metric
      `Prelude.hashWithSalt` value

instance Prelude.NFData CurrentMetricData where
  rnf CurrentMetricData' {..} =
    Prelude.rnf metric `Prelude.seq` Prelude.rnf value
