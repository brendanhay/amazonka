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
-- Module      : Amazonka.QuickSight.Types.TopBottomRankedComputation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopBottomRankedComputation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField
import Amazonka.QuickSight.Types.TopBottomComputationType

-- | The top ranked and bottom ranked computation configuration.
--
-- /See:/ 'newTopBottomRankedComputation' smart constructor.
data TopBottomRankedComputation = TopBottomRankedComputation'
  { -- | The name of a computation.
    name :: Prelude.Maybe Prelude.Text,
    -- | The result size of a top and bottom ranked computation.
    resultSize :: Prelude.Maybe Prelude.Natural,
    -- | The value field that is used in a computation.
    value :: Prelude.Maybe MeasureField,
    -- | The ID for a computation.
    computationId :: Prelude.Text,
    -- | The category field that is used in a computation.
    category :: DimensionField,
    -- | The computation type. Choose one of the following options:
    --
    -- -   TOP: A top ranked computation.
    --
    -- -   BOTTOM: A bottom ranked computation.
    type' :: TopBottomComputationType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopBottomRankedComputation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'topBottomRankedComputation_name' - The name of a computation.
--
-- 'resultSize', 'topBottomRankedComputation_resultSize' - The result size of a top and bottom ranked computation.
--
-- 'value', 'topBottomRankedComputation_value' - The value field that is used in a computation.
--
-- 'computationId', 'topBottomRankedComputation_computationId' - The ID for a computation.
--
-- 'category', 'topBottomRankedComputation_category' - The category field that is used in a computation.
--
-- 'type'', 'topBottomRankedComputation_type' - The computation type. Choose one of the following options:
--
-- -   TOP: A top ranked computation.
--
-- -   BOTTOM: A bottom ranked computation.
newTopBottomRankedComputation ::
  -- | 'computationId'
  Prelude.Text ->
  -- | 'category'
  DimensionField ->
  -- | 'type''
  TopBottomComputationType ->
  TopBottomRankedComputation
newTopBottomRankedComputation
  pComputationId_
  pCategory_
  pType_ =
    TopBottomRankedComputation'
      { name = Prelude.Nothing,
        resultSize = Prelude.Nothing,
        value = Prelude.Nothing,
        computationId = pComputationId_,
        category = pCategory_,
        type' = pType_
      }

-- | The name of a computation.
topBottomRankedComputation_name :: Lens.Lens' TopBottomRankedComputation (Prelude.Maybe Prelude.Text)
topBottomRankedComputation_name = Lens.lens (\TopBottomRankedComputation' {name} -> name) (\s@TopBottomRankedComputation' {} a -> s {name = a} :: TopBottomRankedComputation)

-- | The result size of a top and bottom ranked computation.
topBottomRankedComputation_resultSize :: Lens.Lens' TopBottomRankedComputation (Prelude.Maybe Prelude.Natural)
topBottomRankedComputation_resultSize = Lens.lens (\TopBottomRankedComputation' {resultSize} -> resultSize) (\s@TopBottomRankedComputation' {} a -> s {resultSize = a} :: TopBottomRankedComputation)

-- | The value field that is used in a computation.
topBottomRankedComputation_value :: Lens.Lens' TopBottomRankedComputation (Prelude.Maybe MeasureField)
topBottomRankedComputation_value = Lens.lens (\TopBottomRankedComputation' {value} -> value) (\s@TopBottomRankedComputation' {} a -> s {value = a} :: TopBottomRankedComputation)

-- | The ID for a computation.
topBottomRankedComputation_computationId :: Lens.Lens' TopBottomRankedComputation Prelude.Text
topBottomRankedComputation_computationId = Lens.lens (\TopBottomRankedComputation' {computationId} -> computationId) (\s@TopBottomRankedComputation' {} a -> s {computationId = a} :: TopBottomRankedComputation)

-- | The category field that is used in a computation.
topBottomRankedComputation_category :: Lens.Lens' TopBottomRankedComputation DimensionField
topBottomRankedComputation_category = Lens.lens (\TopBottomRankedComputation' {category} -> category) (\s@TopBottomRankedComputation' {} a -> s {category = a} :: TopBottomRankedComputation)

-- | The computation type. Choose one of the following options:
--
-- -   TOP: A top ranked computation.
--
-- -   BOTTOM: A bottom ranked computation.
topBottomRankedComputation_type :: Lens.Lens' TopBottomRankedComputation TopBottomComputationType
topBottomRankedComputation_type = Lens.lens (\TopBottomRankedComputation' {type'} -> type') (\s@TopBottomRankedComputation' {} a -> s {type' = a} :: TopBottomRankedComputation)

instance Data.FromJSON TopBottomRankedComputation where
  parseJSON =
    Data.withObject
      "TopBottomRankedComputation"
      ( \x ->
          TopBottomRankedComputation'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ResultSize")
            Prelude.<*> (x Data..:? "Value")
            Prelude.<*> (x Data..: "ComputationId")
            Prelude.<*> (x Data..: "Category")
            Prelude.<*> (x Data..: "Type")
      )

instance Prelude.Hashable TopBottomRankedComputation where
  hashWithSalt _salt TopBottomRankedComputation' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resultSize
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` computationId
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` type'

instance Prelude.NFData TopBottomRankedComputation where
  rnf TopBottomRankedComputation' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf resultSize
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf computationId
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON TopBottomRankedComputation where
  toJSON TopBottomRankedComputation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("ResultSize" Data..=) Prelude.<$> resultSize,
            ("Value" Data..=) Prelude.<$> value,
            Prelude.Just ("ComputationId" Data..= computationId),
            Prelude.Just ("Category" Data..= category),
            Prelude.Just ("Type" Data..= type')
          ]
      )
