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
-- Module      : Amazonka.QuickSight.Types.MaximumMinimumComputation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.MaximumMinimumComputation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MaximumMinimumComputationType
import Amazonka.QuickSight.Types.MeasureField

-- | The maximum and minimum computation configuration.
--
-- /See:/ 'newMaximumMinimumComputation' smart constructor.
data MaximumMinimumComputation = MaximumMinimumComputation'
  { -- | The name of a computation.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value field that is used in a computation.
    value :: Prelude.Maybe MeasureField,
    -- | The ID for a computation.
    computationId :: Prelude.Text,
    -- | The time field that is used in a computation.
    time :: DimensionField,
    -- | The type of computation. Choose one of the following options:
    --
    -- -   MAXIMUM: A maximum computation.
    --
    -- -   MINIMUM: A minimum computation.
    type' :: MaximumMinimumComputationType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaximumMinimumComputation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'maximumMinimumComputation_name' - The name of a computation.
--
-- 'value', 'maximumMinimumComputation_value' - The value field that is used in a computation.
--
-- 'computationId', 'maximumMinimumComputation_computationId' - The ID for a computation.
--
-- 'time', 'maximumMinimumComputation_time' - The time field that is used in a computation.
--
-- 'type'', 'maximumMinimumComputation_type' - The type of computation. Choose one of the following options:
--
-- -   MAXIMUM: A maximum computation.
--
-- -   MINIMUM: A minimum computation.
newMaximumMinimumComputation ::
  -- | 'computationId'
  Prelude.Text ->
  -- | 'time'
  DimensionField ->
  -- | 'type''
  MaximumMinimumComputationType ->
  MaximumMinimumComputation
newMaximumMinimumComputation
  pComputationId_
  pTime_
  pType_ =
    MaximumMinimumComputation'
      { name = Prelude.Nothing,
        value = Prelude.Nothing,
        computationId = pComputationId_,
        time = pTime_,
        type' = pType_
      }

-- | The name of a computation.
maximumMinimumComputation_name :: Lens.Lens' MaximumMinimumComputation (Prelude.Maybe Prelude.Text)
maximumMinimumComputation_name = Lens.lens (\MaximumMinimumComputation' {name} -> name) (\s@MaximumMinimumComputation' {} a -> s {name = a} :: MaximumMinimumComputation)

-- | The value field that is used in a computation.
maximumMinimumComputation_value :: Lens.Lens' MaximumMinimumComputation (Prelude.Maybe MeasureField)
maximumMinimumComputation_value = Lens.lens (\MaximumMinimumComputation' {value} -> value) (\s@MaximumMinimumComputation' {} a -> s {value = a} :: MaximumMinimumComputation)

-- | The ID for a computation.
maximumMinimumComputation_computationId :: Lens.Lens' MaximumMinimumComputation Prelude.Text
maximumMinimumComputation_computationId = Lens.lens (\MaximumMinimumComputation' {computationId} -> computationId) (\s@MaximumMinimumComputation' {} a -> s {computationId = a} :: MaximumMinimumComputation)

-- | The time field that is used in a computation.
maximumMinimumComputation_time :: Lens.Lens' MaximumMinimumComputation DimensionField
maximumMinimumComputation_time = Lens.lens (\MaximumMinimumComputation' {time} -> time) (\s@MaximumMinimumComputation' {} a -> s {time = a} :: MaximumMinimumComputation)

-- | The type of computation. Choose one of the following options:
--
-- -   MAXIMUM: A maximum computation.
--
-- -   MINIMUM: A minimum computation.
maximumMinimumComputation_type :: Lens.Lens' MaximumMinimumComputation MaximumMinimumComputationType
maximumMinimumComputation_type = Lens.lens (\MaximumMinimumComputation' {type'} -> type') (\s@MaximumMinimumComputation' {} a -> s {type' = a} :: MaximumMinimumComputation)

instance Data.FromJSON MaximumMinimumComputation where
  parseJSON =
    Data.withObject
      "MaximumMinimumComputation"
      ( \x ->
          MaximumMinimumComputation'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Value")
            Prelude.<*> (x Data..: "ComputationId")
            Prelude.<*> (x Data..: "Time")
            Prelude.<*> (x Data..: "Type")
      )

instance Prelude.Hashable MaximumMinimumComputation where
  hashWithSalt _salt MaximumMinimumComputation' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` computationId
      `Prelude.hashWithSalt` time
      `Prelude.hashWithSalt` type'

instance Prelude.NFData MaximumMinimumComputation where
  rnf MaximumMinimumComputation' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf value `Prelude.seq`
        Prelude.rnf computationId `Prelude.seq`
          Prelude.rnf time `Prelude.seq`
            Prelude.rnf type'

instance Data.ToJSON MaximumMinimumComputation where
  toJSON MaximumMinimumComputation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Value" Data..=) Prelude.<$> value,
            Prelude.Just ("ComputationId" Data..= computationId),
            Prelude.Just ("Time" Data..= time),
            Prelude.Just ("Type" Data..= type')
          ]
      )
