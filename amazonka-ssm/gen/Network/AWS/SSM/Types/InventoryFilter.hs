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
-- Module      : Network.AWS.SSM.Types.InventoryFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.InventoryQueryOperatorType

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
    -- in the /AWS Systems Manager User Guide/.
    type' :: Core.Maybe InventoryQueryOperatorType,
    -- | The name of the filter key.
    key :: Core.Text,
    -- | Inventory filter values. Example: inventory filter where instance IDs
    -- are specified as values Key=AWS:InstanceInformation.InstanceId,Values=
    -- i-a12b3c4d5e6g, i-1a2b3c4d5e6,Type=Equal
    values :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- in the /AWS Systems Manager User Guide/.
--
-- 'key', 'inventoryFilter_key' - The name of the filter key.
--
-- 'values', 'inventoryFilter_values' - Inventory filter values. Example: inventory filter where instance IDs
-- are specified as values Key=AWS:InstanceInformation.InstanceId,Values=
-- i-a12b3c4d5e6g, i-1a2b3c4d5e6,Type=Equal
newInventoryFilter ::
  -- | 'key'
  Core.Text ->
  -- | 'values'
  Core.NonEmpty Core.Text ->
  InventoryFilter
newInventoryFilter pKey_ pValues_ =
  InventoryFilter'
    { type' = Core.Nothing,
      key = pKey_,
      values = Lens._Coerce Lens.# pValues_
    }

-- | The type of filter.
--
-- The @Exists@ filter must be used with aggregators. For more information,
-- see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-aggregate.html Aggregating inventory data>
-- in the /AWS Systems Manager User Guide/.
inventoryFilter_type :: Lens.Lens' InventoryFilter (Core.Maybe InventoryQueryOperatorType)
inventoryFilter_type = Lens.lens (\InventoryFilter' {type'} -> type') (\s@InventoryFilter' {} a -> s {type' = a} :: InventoryFilter)

-- | The name of the filter key.
inventoryFilter_key :: Lens.Lens' InventoryFilter Core.Text
inventoryFilter_key = Lens.lens (\InventoryFilter' {key} -> key) (\s@InventoryFilter' {} a -> s {key = a} :: InventoryFilter)

-- | Inventory filter values. Example: inventory filter where instance IDs
-- are specified as values Key=AWS:InstanceInformation.InstanceId,Values=
-- i-a12b3c4d5e6g, i-1a2b3c4d5e6,Type=Equal
inventoryFilter_values :: Lens.Lens' InventoryFilter (Core.NonEmpty Core.Text)
inventoryFilter_values = Lens.lens (\InventoryFilter' {values} -> values) (\s@InventoryFilter' {} a -> s {values = a} :: InventoryFilter) Core.. Lens._Coerce

instance Core.Hashable InventoryFilter

instance Core.NFData InventoryFilter

instance Core.ToJSON InventoryFilter where
  toJSON InventoryFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Type" Core..=) Core.<$> type',
            Core.Just ("Key" Core..= key),
            Core.Just ("Values" Core..= values)
          ]
      )
