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
-- Module      : Amazonka.ComputeOptimizer.Types.LambdaFunctionMemoryProjectedMetric
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.LambdaFunctionMemoryProjectedMetric where

import Amazonka.ComputeOptimizer.Types.LambdaFunctionMemoryMetricName
import Amazonka.ComputeOptimizer.Types.LambdaFunctionMemoryMetricStatistic
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a projected utilization metric of an Lambda function
-- recommendation option.
--
-- /See:/ 'newLambdaFunctionMemoryProjectedMetric' smart constructor.
data LambdaFunctionMemoryProjectedMetric = LambdaFunctionMemoryProjectedMetric'
  { -- | The name of the projected utilization metric.
    name :: Prelude.Maybe LambdaFunctionMemoryMetricName,
    -- | The statistic of the projected utilization metric.
    statistic :: Prelude.Maybe LambdaFunctionMemoryMetricStatistic,
    -- | The values of the projected utilization metrics.
    value :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionMemoryProjectedMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'lambdaFunctionMemoryProjectedMetric_name' - The name of the projected utilization metric.
--
-- 'statistic', 'lambdaFunctionMemoryProjectedMetric_statistic' - The statistic of the projected utilization metric.
--
-- 'value', 'lambdaFunctionMemoryProjectedMetric_value' - The values of the projected utilization metrics.
newLambdaFunctionMemoryProjectedMetric ::
  LambdaFunctionMemoryProjectedMetric
newLambdaFunctionMemoryProjectedMetric =
  LambdaFunctionMemoryProjectedMetric'
    { name =
        Prelude.Nothing,
      statistic = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the projected utilization metric.
lambdaFunctionMemoryProjectedMetric_name :: Lens.Lens' LambdaFunctionMemoryProjectedMetric (Prelude.Maybe LambdaFunctionMemoryMetricName)
lambdaFunctionMemoryProjectedMetric_name = Lens.lens (\LambdaFunctionMemoryProjectedMetric' {name} -> name) (\s@LambdaFunctionMemoryProjectedMetric' {} a -> s {name = a} :: LambdaFunctionMemoryProjectedMetric)

-- | The statistic of the projected utilization metric.
lambdaFunctionMemoryProjectedMetric_statistic :: Lens.Lens' LambdaFunctionMemoryProjectedMetric (Prelude.Maybe LambdaFunctionMemoryMetricStatistic)
lambdaFunctionMemoryProjectedMetric_statistic = Lens.lens (\LambdaFunctionMemoryProjectedMetric' {statistic} -> statistic) (\s@LambdaFunctionMemoryProjectedMetric' {} a -> s {statistic = a} :: LambdaFunctionMemoryProjectedMetric)

-- | The values of the projected utilization metrics.
lambdaFunctionMemoryProjectedMetric_value :: Lens.Lens' LambdaFunctionMemoryProjectedMetric (Prelude.Maybe Prelude.Double)
lambdaFunctionMemoryProjectedMetric_value = Lens.lens (\LambdaFunctionMemoryProjectedMetric' {value} -> value) (\s@LambdaFunctionMemoryProjectedMetric' {} a -> s {value = a} :: LambdaFunctionMemoryProjectedMetric)

instance
  Core.FromJSON
    LambdaFunctionMemoryProjectedMetric
  where
  parseJSON =
    Core.withObject
      "LambdaFunctionMemoryProjectedMetric"
      ( \x ->
          LambdaFunctionMemoryProjectedMetric'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "statistic")
            Prelude.<*> (x Core..:? "value")
      )

instance
  Prelude.Hashable
    LambdaFunctionMemoryProjectedMetric
  where
  hashWithSalt
    _salt
    LambdaFunctionMemoryProjectedMetric' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` statistic
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    LambdaFunctionMemoryProjectedMetric
  where
  rnf LambdaFunctionMemoryProjectedMetric' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf statistic
      `Prelude.seq` Prelude.rnf value
