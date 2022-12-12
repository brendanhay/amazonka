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
-- Module      : Amazonka.QuickSight.Types.GrowthRateComputation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GrowthRateComputation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField

-- | The growth rate computation configuration.
--
-- /See:/ 'newGrowthRateComputation' smart constructor.
data GrowthRateComputation = GrowthRateComputation'
  { -- | The name of a computation.
    name :: Prelude.Maybe Prelude.Text,
    -- | The period size setup of a growth rate computation.
    periodSize :: Prelude.Maybe Prelude.Natural,
    -- | The value field that is used in a computation.
    value :: Prelude.Maybe MeasureField,
    -- | The ID for a computation.
    computationId :: Prelude.Text,
    -- | The time field that is used in a computation.
    time :: DimensionField
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GrowthRateComputation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'growthRateComputation_name' - The name of a computation.
--
-- 'periodSize', 'growthRateComputation_periodSize' - The period size setup of a growth rate computation.
--
-- 'value', 'growthRateComputation_value' - The value field that is used in a computation.
--
-- 'computationId', 'growthRateComputation_computationId' - The ID for a computation.
--
-- 'time', 'growthRateComputation_time' - The time field that is used in a computation.
newGrowthRateComputation ::
  -- | 'computationId'
  Prelude.Text ->
  -- | 'time'
  DimensionField ->
  GrowthRateComputation
newGrowthRateComputation pComputationId_ pTime_ =
  GrowthRateComputation'
    { name = Prelude.Nothing,
      periodSize = Prelude.Nothing,
      value = Prelude.Nothing,
      computationId = pComputationId_,
      time = pTime_
    }

-- | The name of a computation.
growthRateComputation_name :: Lens.Lens' GrowthRateComputation (Prelude.Maybe Prelude.Text)
growthRateComputation_name = Lens.lens (\GrowthRateComputation' {name} -> name) (\s@GrowthRateComputation' {} a -> s {name = a} :: GrowthRateComputation)

-- | The period size setup of a growth rate computation.
growthRateComputation_periodSize :: Lens.Lens' GrowthRateComputation (Prelude.Maybe Prelude.Natural)
growthRateComputation_periodSize = Lens.lens (\GrowthRateComputation' {periodSize} -> periodSize) (\s@GrowthRateComputation' {} a -> s {periodSize = a} :: GrowthRateComputation)

-- | The value field that is used in a computation.
growthRateComputation_value :: Lens.Lens' GrowthRateComputation (Prelude.Maybe MeasureField)
growthRateComputation_value = Lens.lens (\GrowthRateComputation' {value} -> value) (\s@GrowthRateComputation' {} a -> s {value = a} :: GrowthRateComputation)

-- | The ID for a computation.
growthRateComputation_computationId :: Lens.Lens' GrowthRateComputation Prelude.Text
growthRateComputation_computationId = Lens.lens (\GrowthRateComputation' {computationId} -> computationId) (\s@GrowthRateComputation' {} a -> s {computationId = a} :: GrowthRateComputation)

-- | The time field that is used in a computation.
growthRateComputation_time :: Lens.Lens' GrowthRateComputation DimensionField
growthRateComputation_time = Lens.lens (\GrowthRateComputation' {time} -> time) (\s@GrowthRateComputation' {} a -> s {time = a} :: GrowthRateComputation)

instance Data.FromJSON GrowthRateComputation where
  parseJSON =
    Data.withObject
      "GrowthRateComputation"
      ( \x ->
          GrowthRateComputation'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "PeriodSize")
            Prelude.<*> (x Data..:? "Value")
            Prelude.<*> (x Data..: "ComputationId")
            Prelude.<*> (x Data..: "Time")
      )

instance Prelude.Hashable GrowthRateComputation where
  hashWithSalt _salt GrowthRateComputation' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` periodSize
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` computationId
      `Prelude.hashWithSalt` time

instance Prelude.NFData GrowthRateComputation where
  rnf GrowthRateComputation' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf periodSize
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf computationId
      `Prelude.seq` Prelude.rnf time

instance Data.ToJSON GrowthRateComputation where
  toJSON GrowthRateComputation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("PeriodSize" Data..=) Prelude.<$> periodSize,
            ("Value" Data..=) Prelude.<$> value,
            Prelude.Just ("ComputationId" Data..= computationId),
            Prelude.Just ("Time" Data..= time)
          ]
      )
