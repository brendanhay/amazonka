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
-- Module      : Amazonka.QuickSight.Types.PeriodToDateComputation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PeriodToDateComputation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField
import Amazonka.QuickSight.Types.TimeGranularity

-- | The period to date computation configuration.
--
-- /See:/ 'newPeriodToDateComputation' smart constructor.
data PeriodToDateComputation = PeriodToDateComputation'
  { -- | The name of a computation.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time granularity setup of period to date computation. Choose from
    -- the following options:
    --
    -- -   YEAR: Year to date.
    --
    -- -   MONTH: Month to date.
    periodTimeGranularity :: Prelude.Maybe TimeGranularity,
    -- | The value field that is used in a computation.
    value :: Prelude.Maybe MeasureField,
    -- | The ID for a computation.
    computationId :: Prelude.Text,
    -- | The time field that is used in a computation.
    time :: DimensionField
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PeriodToDateComputation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'periodToDateComputation_name' - The name of a computation.
--
-- 'periodTimeGranularity', 'periodToDateComputation_periodTimeGranularity' - The time granularity setup of period to date computation. Choose from
-- the following options:
--
-- -   YEAR: Year to date.
--
-- -   MONTH: Month to date.
--
-- 'value', 'periodToDateComputation_value' - The value field that is used in a computation.
--
-- 'computationId', 'periodToDateComputation_computationId' - The ID for a computation.
--
-- 'time', 'periodToDateComputation_time' - The time field that is used in a computation.
newPeriodToDateComputation ::
  -- | 'computationId'
  Prelude.Text ->
  -- | 'time'
  DimensionField ->
  PeriodToDateComputation
newPeriodToDateComputation pComputationId_ pTime_ =
  PeriodToDateComputation'
    { name = Prelude.Nothing,
      periodTimeGranularity = Prelude.Nothing,
      value = Prelude.Nothing,
      computationId = pComputationId_,
      time = pTime_
    }

-- | The name of a computation.
periodToDateComputation_name :: Lens.Lens' PeriodToDateComputation (Prelude.Maybe Prelude.Text)
periodToDateComputation_name = Lens.lens (\PeriodToDateComputation' {name} -> name) (\s@PeriodToDateComputation' {} a -> s {name = a} :: PeriodToDateComputation)

-- | The time granularity setup of period to date computation. Choose from
-- the following options:
--
-- -   YEAR: Year to date.
--
-- -   MONTH: Month to date.
periodToDateComputation_periodTimeGranularity :: Lens.Lens' PeriodToDateComputation (Prelude.Maybe TimeGranularity)
periodToDateComputation_periodTimeGranularity = Lens.lens (\PeriodToDateComputation' {periodTimeGranularity} -> periodTimeGranularity) (\s@PeriodToDateComputation' {} a -> s {periodTimeGranularity = a} :: PeriodToDateComputation)

-- | The value field that is used in a computation.
periodToDateComputation_value :: Lens.Lens' PeriodToDateComputation (Prelude.Maybe MeasureField)
periodToDateComputation_value = Lens.lens (\PeriodToDateComputation' {value} -> value) (\s@PeriodToDateComputation' {} a -> s {value = a} :: PeriodToDateComputation)

-- | The ID for a computation.
periodToDateComputation_computationId :: Lens.Lens' PeriodToDateComputation Prelude.Text
periodToDateComputation_computationId = Lens.lens (\PeriodToDateComputation' {computationId} -> computationId) (\s@PeriodToDateComputation' {} a -> s {computationId = a} :: PeriodToDateComputation)

-- | The time field that is used in a computation.
periodToDateComputation_time :: Lens.Lens' PeriodToDateComputation DimensionField
periodToDateComputation_time = Lens.lens (\PeriodToDateComputation' {time} -> time) (\s@PeriodToDateComputation' {} a -> s {time = a} :: PeriodToDateComputation)

instance Data.FromJSON PeriodToDateComputation where
  parseJSON =
    Data.withObject
      "PeriodToDateComputation"
      ( \x ->
          PeriodToDateComputation'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "PeriodTimeGranularity")
            Prelude.<*> (x Data..:? "Value")
            Prelude.<*> (x Data..: "ComputationId")
            Prelude.<*> (x Data..: "Time")
      )

instance Prelude.Hashable PeriodToDateComputation where
  hashWithSalt _salt PeriodToDateComputation' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` periodTimeGranularity
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` computationId
      `Prelude.hashWithSalt` time

instance Prelude.NFData PeriodToDateComputation where
  rnf PeriodToDateComputation' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf periodTimeGranularity
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf computationId
      `Prelude.seq` Prelude.rnf time

instance Data.ToJSON PeriodToDateComputation where
  toJSON PeriodToDateComputation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("PeriodTimeGranularity" Data..=)
              Prelude.<$> periodTimeGranularity,
            ("Value" Data..=) Prelude.<$> value,
            Prelude.Just ("ComputationId" Data..= computationId),
            Prelude.Just ("Time" Data..= time)
          ]
      )
