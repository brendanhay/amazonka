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
-- Module      : Amazonka.LexV2Models.Types.SlotTypeFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SlotTypeFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types.SlotTypeFilterName
import Amazonka.LexV2Models.Types.SlotTypeFilterOperator
import qualified Amazonka.Prelude as Prelude

-- | Filters the response from the @ListSlotTypes@ operation.
--
-- /See:/ 'newSlotTypeFilter' smart constructor.
data SlotTypeFilter = SlotTypeFilter'
  { -- | The name of the field to use for filtering.
    name :: SlotTypeFilterName,
    -- | The value to use to filter the response.
    values :: Prelude.NonEmpty Prelude.Text,
    -- | The operator to use for the filter. Specify @EQ@ when the
    -- @ListSlotTypes@ operation should return only aliases that equal the
    -- specified value. Specify @CO@ when the @ListSlotTypes@ operation should
    -- return aliases that contain the specified value.
    operator :: SlotTypeFilterOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlotTypeFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'slotTypeFilter_name' - The name of the field to use for filtering.
--
-- 'values', 'slotTypeFilter_values' - The value to use to filter the response.
--
-- 'operator', 'slotTypeFilter_operator' - The operator to use for the filter. Specify @EQ@ when the
-- @ListSlotTypes@ operation should return only aliases that equal the
-- specified value. Specify @CO@ when the @ListSlotTypes@ operation should
-- return aliases that contain the specified value.
newSlotTypeFilter ::
  -- | 'name'
  SlotTypeFilterName ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'operator'
  SlotTypeFilterOperator ->
  SlotTypeFilter
newSlotTypeFilter pName_ pValues_ pOperator_ =
  SlotTypeFilter'
    { name = pName_,
      values = Lens.coerced Lens.# pValues_,
      operator = pOperator_
    }

-- | The name of the field to use for filtering.
slotTypeFilter_name :: Lens.Lens' SlotTypeFilter SlotTypeFilterName
slotTypeFilter_name = Lens.lens (\SlotTypeFilter' {name} -> name) (\s@SlotTypeFilter' {} a -> s {name = a} :: SlotTypeFilter)

-- | The value to use to filter the response.
slotTypeFilter_values :: Lens.Lens' SlotTypeFilter (Prelude.NonEmpty Prelude.Text)
slotTypeFilter_values = Lens.lens (\SlotTypeFilter' {values} -> values) (\s@SlotTypeFilter' {} a -> s {values = a} :: SlotTypeFilter) Prelude.. Lens.coerced

-- | The operator to use for the filter. Specify @EQ@ when the
-- @ListSlotTypes@ operation should return only aliases that equal the
-- specified value. Specify @CO@ when the @ListSlotTypes@ operation should
-- return aliases that contain the specified value.
slotTypeFilter_operator :: Lens.Lens' SlotTypeFilter SlotTypeFilterOperator
slotTypeFilter_operator = Lens.lens (\SlotTypeFilter' {operator} -> operator) (\s@SlotTypeFilter' {} a -> s {operator = a} :: SlotTypeFilter)

instance Prelude.Hashable SlotTypeFilter where
  hashWithSalt _salt SlotTypeFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` operator

instance Prelude.NFData SlotTypeFilter where
  rnf SlotTypeFilter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf values
      `Prelude.seq` Prelude.rnf operator

instance Core.ToJSON SlotTypeFilter where
  toJSON SlotTypeFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Core..= name),
            Prelude.Just ("values" Core..= values),
            Prelude.Just ("operator" Core..= operator)
          ]
      )
