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
-- Module      : Amazonka.QuickSight.Types.TopBottomMoversComputation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopBottomMoversComputation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField
import Amazonka.QuickSight.Types.TopBottomComputationType
import Amazonka.QuickSight.Types.TopBottomSortOrder

-- | The top movers and bottom movers computation setup.
--
-- /See:/ 'newTopBottomMoversComputation' smart constructor.
data TopBottomMoversComputation = TopBottomMoversComputation'
  { -- | The mover size setup of the top and bottom movers computation.
    moverSize :: Prelude.Maybe Prelude.Natural,
    -- | The name of a computation.
    name :: Prelude.Maybe Prelude.Text,
    -- | The sort order setup of the top and bottom movers computation.
    sortOrder :: Prelude.Maybe TopBottomSortOrder,
    -- | The value field that is used in a computation.
    value :: Prelude.Maybe MeasureField,
    -- | The ID for a computation.
    computationId :: Prelude.Text,
    -- | The time field that is used in a computation.
    time :: DimensionField,
    -- | The category field that is used in a computation.
    category :: DimensionField,
    -- | The computation type. Choose from the following options:
    --
    -- -   TOP: Top movers computation.
    --
    -- -   BOTTOM: Bottom movers computation.
    type' :: TopBottomComputationType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopBottomMoversComputation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'moverSize', 'topBottomMoversComputation_moverSize' - The mover size setup of the top and bottom movers computation.
--
-- 'name', 'topBottomMoversComputation_name' - The name of a computation.
--
-- 'sortOrder', 'topBottomMoversComputation_sortOrder' - The sort order setup of the top and bottom movers computation.
--
-- 'value', 'topBottomMoversComputation_value' - The value field that is used in a computation.
--
-- 'computationId', 'topBottomMoversComputation_computationId' - The ID for a computation.
--
-- 'time', 'topBottomMoversComputation_time' - The time field that is used in a computation.
--
-- 'category', 'topBottomMoversComputation_category' - The category field that is used in a computation.
--
-- 'type'', 'topBottomMoversComputation_type' - The computation type. Choose from the following options:
--
-- -   TOP: Top movers computation.
--
-- -   BOTTOM: Bottom movers computation.
newTopBottomMoversComputation ::
  -- | 'computationId'
  Prelude.Text ->
  -- | 'time'
  DimensionField ->
  -- | 'category'
  DimensionField ->
  -- | 'type''
  TopBottomComputationType ->
  TopBottomMoversComputation
newTopBottomMoversComputation
  pComputationId_
  pTime_
  pCategory_
  pType_ =
    TopBottomMoversComputation'
      { moverSize =
          Prelude.Nothing,
        name = Prelude.Nothing,
        sortOrder = Prelude.Nothing,
        value = Prelude.Nothing,
        computationId = pComputationId_,
        time = pTime_,
        category = pCategory_,
        type' = pType_
      }

-- | The mover size setup of the top and bottom movers computation.
topBottomMoversComputation_moverSize :: Lens.Lens' TopBottomMoversComputation (Prelude.Maybe Prelude.Natural)
topBottomMoversComputation_moverSize = Lens.lens (\TopBottomMoversComputation' {moverSize} -> moverSize) (\s@TopBottomMoversComputation' {} a -> s {moverSize = a} :: TopBottomMoversComputation)

-- | The name of a computation.
topBottomMoversComputation_name :: Lens.Lens' TopBottomMoversComputation (Prelude.Maybe Prelude.Text)
topBottomMoversComputation_name = Lens.lens (\TopBottomMoversComputation' {name} -> name) (\s@TopBottomMoversComputation' {} a -> s {name = a} :: TopBottomMoversComputation)

-- | The sort order setup of the top and bottom movers computation.
topBottomMoversComputation_sortOrder :: Lens.Lens' TopBottomMoversComputation (Prelude.Maybe TopBottomSortOrder)
topBottomMoversComputation_sortOrder = Lens.lens (\TopBottomMoversComputation' {sortOrder} -> sortOrder) (\s@TopBottomMoversComputation' {} a -> s {sortOrder = a} :: TopBottomMoversComputation)

-- | The value field that is used in a computation.
topBottomMoversComputation_value :: Lens.Lens' TopBottomMoversComputation (Prelude.Maybe MeasureField)
topBottomMoversComputation_value = Lens.lens (\TopBottomMoversComputation' {value} -> value) (\s@TopBottomMoversComputation' {} a -> s {value = a} :: TopBottomMoversComputation)

-- | The ID for a computation.
topBottomMoversComputation_computationId :: Lens.Lens' TopBottomMoversComputation Prelude.Text
topBottomMoversComputation_computationId = Lens.lens (\TopBottomMoversComputation' {computationId} -> computationId) (\s@TopBottomMoversComputation' {} a -> s {computationId = a} :: TopBottomMoversComputation)

-- | The time field that is used in a computation.
topBottomMoversComputation_time :: Lens.Lens' TopBottomMoversComputation DimensionField
topBottomMoversComputation_time = Lens.lens (\TopBottomMoversComputation' {time} -> time) (\s@TopBottomMoversComputation' {} a -> s {time = a} :: TopBottomMoversComputation)

-- | The category field that is used in a computation.
topBottomMoversComputation_category :: Lens.Lens' TopBottomMoversComputation DimensionField
topBottomMoversComputation_category = Lens.lens (\TopBottomMoversComputation' {category} -> category) (\s@TopBottomMoversComputation' {} a -> s {category = a} :: TopBottomMoversComputation)

-- | The computation type. Choose from the following options:
--
-- -   TOP: Top movers computation.
--
-- -   BOTTOM: Bottom movers computation.
topBottomMoversComputation_type :: Lens.Lens' TopBottomMoversComputation TopBottomComputationType
topBottomMoversComputation_type = Lens.lens (\TopBottomMoversComputation' {type'} -> type') (\s@TopBottomMoversComputation' {} a -> s {type' = a} :: TopBottomMoversComputation)

instance Data.FromJSON TopBottomMoversComputation where
  parseJSON =
    Data.withObject
      "TopBottomMoversComputation"
      ( \x ->
          TopBottomMoversComputation'
            Prelude.<$> (x Data..:? "MoverSize")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "SortOrder")
            Prelude.<*> (x Data..:? "Value")
            Prelude.<*> (x Data..: "ComputationId")
            Prelude.<*> (x Data..: "Time")
            Prelude.<*> (x Data..: "Category")
            Prelude.<*> (x Data..: "Type")
      )

instance Prelude.Hashable TopBottomMoversComputation where
  hashWithSalt _salt TopBottomMoversComputation' {..} =
    _salt `Prelude.hashWithSalt` moverSize
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` computationId
      `Prelude.hashWithSalt` time
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` type'

instance Prelude.NFData TopBottomMoversComputation where
  rnf TopBottomMoversComputation' {..} =
    Prelude.rnf moverSize
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf computationId
      `Prelude.seq` Prelude.rnf time
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON TopBottomMoversComputation where
  toJSON TopBottomMoversComputation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MoverSize" Data..=) Prelude.<$> moverSize,
            ("Name" Data..=) Prelude.<$> name,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("Value" Data..=) Prelude.<$> value,
            Prelude.Just ("ComputationId" Data..= computationId),
            Prelude.Just ("Time" Data..= time),
            Prelude.Just ("Category" Data..= category),
            Prelude.Just ("Type" Data..= type')
          ]
      )
