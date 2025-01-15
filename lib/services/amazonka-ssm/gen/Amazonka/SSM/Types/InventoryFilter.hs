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
-- Module      : Amazonka.SSM.Types.InventoryFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InventoryFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.InventoryQueryOperatorType

-- | One or more filters. Use a filter to return a more specific list of
-- results.
--
-- /See:/ 'newInventoryFilter' smart constructor.
data InventoryFilter = InventoryFilter'
  { -- | The type of filter.
    --
    -- The @Exists@ filter must be used with aggregators. For more information,
    -- see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-aggregate.html Aggregating inventory data>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    type' :: Prelude.Maybe InventoryQueryOperatorType,
    -- | The name of the filter key.
    key :: Prelude.Text,
    -- | Inventory filter values. Example: inventory filter where managed node
    -- IDs are specified as values
    -- @Key=AWS:InstanceInformation.InstanceId,Values= i-a12b3c4d5e6g, i-1a2b3c4d5e6,Type=Equal@.
    values :: Prelude.NonEmpty Prelude.Text
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
-- 'type'', 'inventoryFilter_type' - The type of filter.
--
-- The @Exists@ filter must be used with aggregators. For more information,
-- see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-aggregate.html Aggregating inventory data>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'key', 'inventoryFilter_key' - The name of the filter key.
--
-- 'values', 'inventoryFilter_values' - Inventory filter values. Example: inventory filter where managed node
-- IDs are specified as values
-- @Key=AWS:InstanceInformation.InstanceId,Values= i-a12b3c4d5e6g, i-1a2b3c4d5e6,Type=Equal@.
newInventoryFilter ::
  -- | 'key'
  Prelude.Text ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  InventoryFilter
newInventoryFilter pKey_ pValues_ =
  InventoryFilter'
    { type' = Prelude.Nothing,
      key = pKey_,
      values = Lens.coerced Lens.# pValues_
    }

-- | The type of filter.
--
-- The @Exists@ filter must be used with aggregators. For more information,
-- see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-aggregate.html Aggregating inventory data>
-- in the /Amazon Web Services Systems Manager User Guide/.
inventoryFilter_type :: Lens.Lens' InventoryFilter (Prelude.Maybe InventoryQueryOperatorType)
inventoryFilter_type = Lens.lens (\InventoryFilter' {type'} -> type') (\s@InventoryFilter' {} a -> s {type' = a} :: InventoryFilter)

-- | The name of the filter key.
inventoryFilter_key :: Lens.Lens' InventoryFilter Prelude.Text
inventoryFilter_key = Lens.lens (\InventoryFilter' {key} -> key) (\s@InventoryFilter' {} a -> s {key = a} :: InventoryFilter)

-- | Inventory filter values. Example: inventory filter where managed node
-- IDs are specified as values
-- @Key=AWS:InstanceInformation.InstanceId,Values= i-a12b3c4d5e6g, i-1a2b3c4d5e6,Type=Equal@.
inventoryFilter_values :: Lens.Lens' InventoryFilter (Prelude.NonEmpty Prelude.Text)
inventoryFilter_values = Lens.lens (\InventoryFilter' {values} -> values) (\s@InventoryFilter' {} a -> s {values = a} :: InventoryFilter) Prelude.. Lens.coerced

instance Prelude.Hashable InventoryFilter where
  hashWithSalt _salt InventoryFilter' {..} =
    _salt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values

instance Prelude.NFData InventoryFilter where
  rnf InventoryFilter' {..} =
    Prelude.rnf type' `Prelude.seq`
      Prelude.rnf key `Prelude.seq`
        Prelude.rnf values

instance Data.ToJSON InventoryFilter where
  toJSON InventoryFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Type" Data..=) Prelude.<$> type',
            Prelude.Just ("Key" Data..= key),
            Prelude.Just ("Values" Data..= values)
          ]
      )
