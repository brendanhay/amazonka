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
-- Module      : Amazonka.LexV2Models.Types.SlotFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SlotFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.SlotFilterName
import Amazonka.LexV2Models.Types.SlotFilterOperator
import qualified Amazonka.Prelude as Prelude

-- | Filters the response from the @ListSlots@ operation.
--
-- /See:/ 'newSlotFilter' smart constructor.
data SlotFilter = SlotFilter'
  { -- | The name of the field to use for filtering.
    name :: SlotFilterName,
    -- | The value to use to filter the response.
    values :: Prelude.NonEmpty Prelude.Text,
    -- | The operator to use for the filter. Specify @EQ@ when the @ListSlots@
    -- operation should return only aliases that equal the specified value.
    -- Specify @CO@ when the @ListSlots@ operation should return aliases that
    -- contain the specified value.
    operator :: SlotFilterOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlotFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'slotFilter_name' - The name of the field to use for filtering.
--
-- 'values', 'slotFilter_values' - The value to use to filter the response.
--
-- 'operator', 'slotFilter_operator' - The operator to use for the filter. Specify @EQ@ when the @ListSlots@
-- operation should return only aliases that equal the specified value.
-- Specify @CO@ when the @ListSlots@ operation should return aliases that
-- contain the specified value.
newSlotFilter ::
  -- | 'name'
  SlotFilterName ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'operator'
  SlotFilterOperator ->
  SlotFilter
newSlotFilter pName_ pValues_ pOperator_ =
  SlotFilter'
    { name = pName_,
      values = Lens.coerced Lens.# pValues_,
      operator = pOperator_
    }

-- | The name of the field to use for filtering.
slotFilter_name :: Lens.Lens' SlotFilter SlotFilterName
slotFilter_name = Lens.lens (\SlotFilter' {name} -> name) (\s@SlotFilter' {} a -> s {name = a} :: SlotFilter)

-- | The value to use to filter the response.
slotFilter_values :: Lens.Lens' SlotFilter (Prelude.NonEmpty Prelude.Text)
slotFilter_values = Lens.lens (\SlotFilter' {values} -> values) (\s@SlotFilter' {} a -> s {values = a} :: SlotFilter) Prelude.. Lens.coerced

-- | The operator to use for the filter. Specify @EQ@ when the @ListSlots@
-- operation should return only aliases that equal the specified value.
-- Specify @CO@ when the @ListSlots@ operation should return aliases that
-- contain the specified value.
slotFilter_operator :: Lens.Lens' SlotFilter SlotFilterOperator
slotFilter_operator = Lens.lens (\SlotFilter' {operator} -> operator) (\s@SlotFilter' {} a -> s {operator = a} :: SlotFilter)

instance Prelude.Hashable SlotFilter where
  hashWithSalt _salt SlotFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` operator

instance Prelude.NFData SlotFilter where
  rnf SlotFilter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf values
      `Prelude.seq` Prelude.rnf operator

instance Data.ToJSON SlotFilter where
  toJSON SlotFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("values" Data..= values),
            Prelude.Just ("operator" Data..= operator)
          ]
      )
