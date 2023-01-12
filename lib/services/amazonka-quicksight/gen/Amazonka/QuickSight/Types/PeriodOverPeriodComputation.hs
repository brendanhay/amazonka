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
-- Module      : Amazonka.QuickSight.Types.PeriodOverPeriodComputation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PeriodOverPeriodComputation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField

-- | The period over period computation configuration.
--
-- /See:/ 'newPeriodOverPeriodComputation' smart constructor.
data PeriodOverPeriodComputation = PeriodOverPeriodComputation'
  { -- | The name of a computation.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value field that is used in a computation.
    value :: Prelude.Maybe MeasureField,
    -- | The ID for a computation.
    computationId :: Prelude.Text,
    -- | The time field that is used in a computation.
    time :: DimensionField
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PeriodOverPeriodComputation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'periodOverPeriodComputation_name' - The name of a computation.
--
-- 'value', 'periodOverPeriodComputation_value' - The value field that is used in a computation.
--
-- 'computationId', 'periodOverPeriodComputation_computationId' - The ID for a computation.
--
-- 'time', 'periodOverPeriodComputation_time' - The time field that is used in a computation.
newPeriodOverPeriodComputation ::
  -- | 'computationId'
  Prelude.Text ->
  -- | 'time'
  DimensionField ->
  PeriodOverPeriodComputation
newPeriodOverPeriodComputation pComputationId_ pTime_ =
  PeriodOverPeriodComputation'
    { name =
        Prelude.Nothing,
      value = Prelude.Nothing,
      computationId = pComputationId_,
      time = pTime_
    }

-- | The name of a computation.
periodOverPeriodComputation_name :: Lens.Lens' PeriodOverPeriodComputation (Prelude.Maybe Prelude.Text)
periodOverPeriodComputation_name = Lens.lens (\PeriodOverPeriodComputation' {name} -> name) (\s@PeriodOverPeriodComputation' {} a -> s {name = a} :: PeriodOverPeriodComputation)

-- | The value field that is used in a computation.
periodOverPeriodComputation_value :: Lens.Lens' PeriodOverPeriodComputation (Prelude.Maybe MeasureField)
periodOverPeriodComputation_value = Lens.lens (\PeriodOverPeriodComputation' {value} -> value) (\s@PeriodOverPeriodComputation' {} a -> s {value = a} :: PeriodOverPeriodComputation)

-- | The ID for a computation.
periodOverPeriodComputation_computationId :: Lens.Lens' PeriodOverPeriodComputation Prelude.Text
periodOverPeriodComputation_computationId = Lens.lens (\PeriodOverPeriodComputation' {computationId} -> computationId) (\s@PeriodOverPeriodComputation' {} a -> s {computationId = a} :: PeriodOverPeriodComputation)

-- | The time field that is used in a computation.
periodOverPeriodComputation_time :: Lens.Lens' PeriodOverPeriodComputation DimensionField
periodOverPeriodComputation_time = Lens.lens (\PeriodOverPeriodComputation' {time} -> time) (\s@PeriodOverPeriodComputation' {} a -> s {time = a} :: PeriodOverPeriodComputation)

instance Data.FromJSON PeriodOverPeriodComputation where
  parseJSON =
    Data.withObject
      "PeriodOverPeriodComputation"
      ( \x ->
          PeriodOverPeriodComputation'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Value")
            Prelude.<*> (x Data..: "ComputationId")
            Prelude.<*> (x Data..: "Time")
      )

instance Prelude.Hashable PeriodOverPeriodComputation where
  hashWithSalt _salt PeriodOverPeriodComputation' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` computationId
      `Prelude.hashWithSalt` time

instance Prelude.NFData PeriodOverPeriodComputation where
  rnf PeriodOverPeriodComputation' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf computationId
      `Prelude.seq` Prelude.rnf time

instance Data.ToJSON PeriodOverPeriodComputation where
  toJSON PeriodOverPeriodComputation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Value" Data..=) Prelude.<$> value,
            Prelude.Just ("ComputationId" Data..= computationId),
            Prelude.Just ("Time" Data..= time)
          ]
      )
