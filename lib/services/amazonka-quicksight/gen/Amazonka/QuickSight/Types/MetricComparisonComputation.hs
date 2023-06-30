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
-- Module      : Amazonka.QuickSight.Types.MetricComparisonComputation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.MetricComparisonComputation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField

-- | The metric comparison computation configuration.
--
-- /See:/ 'newMetricComparisonComputation' smart constructor.
data MetricComparisonComputation = MetricComparisonComputation'
  { -- | The name of a computation.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID for a computation.
    computationId :: Prelude.Text,
    -- | The time field that is used in a computation.
    time :: DimensionField,
    -- | The field that is used in a metric comparison from value setup.
    fromValue :: MeasureField,
    -- | The field that is used in a metric comparison to value setup.
    targetValue :: MeasureField
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricComparisonComputation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'metricComparisonComputation_name' - The name of a computation.
--
-- 'computationId', 'metricComparisonComputation_computationId' - The ID for a computation.
--
-- 'time', 'metricComparisonComputation_time' - The time field that is used in a computation.
--
-- 'fromValue', 'metricComparisonComputation_fromValue' - The field that is used in a metric comparison from value setup.
--
-- 'targetValue', 'metricComparisonComputation_targetValue' - The field that is used in a metric comparison to value setup.
newMetricComparisonComputation ::
  -- | 'computationId'
  Prelude.Text ->
  -- | 'time'
  DimensionField ->
  -- | 'fromValue'
  MeasureField ->
  -- | 'targetValue'
  MeasureField ->
  MetricComparisonComputation
newMetricComparisonComputation
  pComputationId_
  pTime_
  pFromValue_
  pTargetValue_ =
    MetricComparisonComputation'
      { name =
          Prelude.Nothing,
        computationId = pComputationId_,
        time = pTime_,
        fromValue = pFromValue_,
        targetValue = pTargetValue_
      }

-- | The name of a computation.
metricComparisonComputation_name :: Lens.Lens' MetricComparisonComputation (Prelude.Maybe Prelude.Text)
metricComparisonComputation_name = Lens.lens (\MetricComparisonComputation' {name} -> name) (\s@MetricComparisonComputation' {} a -> s {name = a} :: MetricComparisonComputation)

-- | The ID for a computation.
metricComparisonComputation_computationId :: Lens.Lens' MetricComparisonComputation Prelude.Text
metricComparisonComputation_computationId = Lens.lens (\MetricComparisonComputation' {computationId} -> computationId) (\s@MetricComparisonComputation' {} a -> s {computationId = a} :: MetricComparisonComputation)

-- | The time field that is used in a computation.
metricComparisonComputation_time :: Lens.Lens' MetricComparisonComputation DimensionField
metricComparisonComputation_time = Lens.lens (\MetricComparisonComputation' {time} -> time) (\s@MetricComparisonComputation' {} a -> s {time = a} :: MetricComparisonComputation)

-- | The field that is used in a metric comparison from value setup.
metricComparisonComputation_fromValue :: Lens.Lens' MetricComparisonComputation MeasureField
metricComparisonComputation_fromValue = Lens.lens (\MetricComparisonComputation' {fromValue} -> fromValue) (\s@MetricComparisonComputation' {} a -> s {fromValue = a} :: MetricComparisonComputation)

-- | The field that is used in a metric comparison to value setup.
metricComparisonComputation_targetValue :: Lens.Lens' MetricComparisonComputation MeasureField
metricComparisonComputation_targetValue = Lens.lens (\MetricComparisonComputation' {targetValue} -> targetValue) (\s@MetricComparisonComputation' {} a -> s {targetValue = a} :: MetricComparisonComputation)

instance Data.FromJSON MetricComparisonComputation where
  parseJSON =
    Data.withObject
      "MetricComparisonComputation"
      ( \x ->
          MetricComparisonComputation'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..: "ComputationId")
            Prelude.<*> (x Data..: "Time")
            Prelude.<*> (x Data..: "FromValue")
            Prelude.<*> (x Data..: "TargetValue")
      )

instance Prelude.Hashable MetricComparisonComputation where
  hashWithSalt _salt MetricComparisonComputation' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` computationId
      `Prelude.hashWithSalt` time
      `Prelude.hashWithSalt` fromValue
      `Prelude.hashWithSalt` targetValue

instance Prelude.NFData MetricComparisonComputation where
  rnf MetricComparisonComputation' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf computationId
      `Prelude.seq` Prelude.rnf time
      `Prelude.seq` Prelude.rnf fromValue
      `Prelude.seq` Prelude.rnf targetValue

instance Data.ToJSON MetricComparisonComputation where
  toJSON MetricComparisonComputation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            Prelude.Just ("ComputationId" Data..= computationId),
            Prelude.Just ("Time" Data..= time),
            Prelude.Just ("FromValue" Data..= fromValue),
            Prelude.Just ("TargetValue" Data..= targetValue)
          ]
      )
