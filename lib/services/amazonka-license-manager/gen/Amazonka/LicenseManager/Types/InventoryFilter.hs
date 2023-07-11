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
-- Module      : Amazonka.LicenseManager.Types.InventoryFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.InventoryFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types.InventoryFilterCondition
import qualified Amazonka.Prelude as Prelude

-- | An inventory filter.
--
-- /See:/ 'newInventoryFilter' smart constructor.
data InventoryFilter = InventoryFilter'
  { -- | Value of the filter.
    value :: Prelude.Maybe Prelude.Text,
    -- | Name of the filter.
    name :: Prelude.Text,
    -- | Condition of the filter.
    condition :: InventoryFilterCondition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InventoryFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'inventoryFilter_value' - Value of the filter.
--
-- 'name', 'inventoryFilter_name' - Name of the filter.
--
-- 'condition', 'inventoryFilter_condition' - Condition of the filter.
newInventoryFilter ::
  -- | 'name'
  Prelude.Text ->
  -- | 'condition'
  InventoryFilterCondition ->
  InventoryFilter
newInventoryFilter pName_ pCondition_ =
  InventoryFilter'
    { value = Prelude.Nothing,
      name = pName_,
      condition = pCondition_
    }

-- | Value of the filter.
inventoryFilter_value :: Lens.Lens' InventoryFilter (Prelude.Maybe Prelude.Text)
inventoryFilter_value = Lens.lens (\InventoryFilter' {value} -> value) (\s@InventoryFilter' {} a -> s {value = a} :: InventoryFilter)

-- | Name of the filter.
inventoryFilter_name :: Lens.Lens' InventoryFilter Prelude.Text
inventoryFilter_name = Lens.lens (\InventoryFilter' {name} -> name) (\s@InventoryFilter' {} a -> s {name = a} :: InventoryFilter)

-- | Condition of the filter.
inventoryFilter_condition :: Lens.Lens' InventoryFilter InventoryFilterCondition
inventoryFilter_condition = Lens.lens (\InventoryFilter' {condition} -> condition) (\s@InventoryFilter' {} a -> s {condition = a} :: InventoryFilter)

instance Prelude.Hashable InventoryFilter where
  hashWithSalt _salt InventoryFilter' {..} =
    _salt
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` condition

instance Prelude.NFData InventoryFilter where
  rnf InventoryFilter' {..} =
    Prelude.rnf value
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf condition

instance Data.ToJSON InventoryFilter where
  toJSON InventoryFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Value" Data..=) Prelude.<$> value,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Condition" Data..= condition)
          ]
      )
