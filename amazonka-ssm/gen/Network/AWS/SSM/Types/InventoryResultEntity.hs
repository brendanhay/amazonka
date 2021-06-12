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
-- Module      : Network.AWS.SSM.Types.InventoryResultEntity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryResultEntity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.InventoryResultItem

-- | Inventory query results.
--
-- /See:/ 'newInventoryResultEntity' smart constructor.
data InventoryResultEntity = InventoryResultEntity'
  { -- | The data section in the inventory result entity JSON.
    data' :: Core.Maybe (Core.HashMap Core.Text InventoryResultItem),
    -- | ID of the inventory result entity. For example, for managed instance
    -- inventory the result will be the managed instance ID. For EC2 instance
    -- inventory, the result will be the instance ID.
    id :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InventoryResultEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'inventoryResultEntity_data' - The data section in the inventory result entity JSON.
--
-- 'id', 'inventoryResultEntity_id' - ID of the inventory result entity. For example, for managed instance
-- inventory the result will be the managed instance ID. For EC2 instance
-- inventory, the result will be the instance ID.
newInventoryResultEntity ::
  InventoryResultEntity
newInventoryResultEntity =
  InventoryResultEntity'
    { data' = Core.Nothing,
      id = Core.Nothing
    }

-- | The data section in the inventory result entity JSON.
inventoryResultEntity_data :: Lens.Lens' InventoryResultEntity (Core.Maybe (Core.HashMap Core.Text InventoryResultItem))
inventoryResultEntity_data = Lens.lens (\InventoryResultEntity' {data'} -> data') (\s@InventoryResultEntity' {} a -> s {data' = a} :: InventoryResultEntity) Core.. Lens.mapping Lens._Coerce

-- | ID of the inventory result entity. For example, for managed instance
-- inventory the result will be the managed instance ID. For EC2 instance
-- inventory, the result will be the instance ID.
inventoryResultEntity_id :: Lens.Lens' InventoryResultEntity (Core.Maybe Core.Text)
inventoryResultEntity_id = Lens.lens (\InventoryResultEntity' {id} -> id) (\s@InventoryResultEntity' {} a -> s {id = a} :: InventoryResultEntity)

instance Core.FromJSON InventoryResultEntity where
  parseJSON =
    Core.withObject
      "InventoryResultEntity"
      ( \x ->
          InventoryResultEntity'
            Core.<$> (x Core..:? "Data" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Id")
      )

instance Core.Hashable InventoryResultEntity

instance Core.NFData InventoryResultEntity
