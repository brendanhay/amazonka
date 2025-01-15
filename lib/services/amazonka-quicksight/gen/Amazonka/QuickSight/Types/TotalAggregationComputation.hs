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
-- Module      : Amazonka.QuickSight.Types.TotalAggregationComputation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TotalAggregationComputation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.MeasureField

-- | The total aggregation computation configuration.
--
-- /See:/ 'newTotalAggregationComputation' smart constructor.
data TotalAggregationComputation = TotalAggregationComputation'
  { -- | The name of a computation.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID for a computation.
    computationId :: Prelude.Text,
    -- | The value field that is used in a computation.
    value :: MeasureField
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TotalAggregationComputation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'totalAggregationComputation_name' - The name of a computation.
--
-- 'computationId', 'totalAggregationComputation_computationId' - The ID for a computation.
--
-- 'value', 'totalAggregationComputation_value' - The value field that is used in a computation.
newTotalAggregationComputation ::
  -- | 'computationId'
  Prelude.Text ->
  -- | 'value'
  MeasureField ->
  TotalAggregationComputation
newTotalAggregationComputation
  pComputationId_
  pValue_ =
    TotalAggregationComputation'
      { name =
          Prelude.Nothing,
        computationId = pComputationId_,
        value = pValue_
      }

-- | The name of a computation.
totalAggregationComputation_name :: Lens.Lens' TotalAggregationComputation (Prelude.Maybe Prelude.Text)
totalAggregationComputation_name = Lens.lens (\TotalAggregationComputation' {name} -> name) (\s@TotalAggregationComputation' {} a -> s {name = a} :: TotalAggregationComputation)

-- | The ID for a computation.
totalAggregationComputation_computationId :: Lens.Lens' TotalAggregationComputation Prelude.Text
totalAggregationComputation_computationId = Lens.lens (\TotalAggregationComputation' {computationId} -> computationId) (\s@TotalAggregationComputation' {} a -> s {computationId = a} :: TotalAggregationComputation)

-- | The value field that is used in a computation.
totalAggregationComputation_value :: Lens.Lens' TotalAggregationComputation MeasureField
totalAggregationComputation_value = Lens.lens (\TotalAggregationComputation' {value} -> value) (\s@TotalAggregationComputation' {} a -> s {value = a} :: TotalAggregationComputation)

instance Data.FromJSON TotalAggregationComputation where
  parseJSON =
    Data.withObject
      "TotalAggregationComputation"
      ( \x ->
          TotalAggregationComputation'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..: "ComputationId")
            Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable TotalAggregationComputation where
  hashWithSalt _salt TotalAggregationComputation' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` computationId
      `Prelude.hashWithSalt` value

instance Prelude.NFData TotalAggregationComputation where
  rnf TotalAggregationComputation' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf computationId `Prelude.seq`
        Prelude.rnf value

instance Data.ToJSON TotalAggregationComputation where
  toJSON TotalAggregationComputation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            Prelude.Just ("ComputationId" Data..= computationId),
            Prelude.Just ("Value" Data..= value)
          ]
      )
