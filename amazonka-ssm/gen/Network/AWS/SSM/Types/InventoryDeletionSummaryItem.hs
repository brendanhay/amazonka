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
-- Module      : Network.AWS.SSM.Types.InventoryDeletionSummaryItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryDeletionSummaryItem where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Either a count, remaining count, or a version number in a delete
-- inventory summary.
--
-- /See:/ 'newInventoryDeletionSummaryItem' smart constructor.
data InventoryDeletionSummaryItem = InventoryDeletionSummaryItem'
  { -- | The remaining number of items to delete.
    remainingCount :: Core.Maybe Core.Int,
    -- | The inventory type version.
    version :: Core.Maybe Core.Text,
    -- | A count of the number of deleted items.
    count :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InventoryDeletionSummaryItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remainingCount', 'inventoryDeletionSummaryItem_remainingCount' - The remaining number of items to delete.
--
-- 'version', 'inventoryDeletionSummaryItem_version' - The inventory type version.
--
-- 'count', 'inventoryDeletionSummaryItem_count' - A count of the number of deleted items.
newInventoryDeletionSummaryItem ::
  InventoryDeletionSummaryItem
newInventoryDeletionSummaryItem =
  InventoryDeletionSummaryItem'
    { remainingCount =
        Core.Nothing,
      version = Core.Nothing,
      count = Core.Nothing
    }

-- | The remaining number of items to delete.
inventoryDeletionSummaryItem_remainingCount :: Lens.Lens' InventoryDeletionSummaryItem (Core.Maybe Core.Int)
inventoryDeletionSummaryItem_remainingCount = Lens.lens (\InventoryDeletionSummaryItem' {remainingCount} -> remainingCount) (\s@InventoryDeletionSummaryItem' {} a -> s {remainingCount = a} :: InventoryDeletionSummaryItem)

-- | The inventory type version.
inventoryDeletionSummaryItem_version :: Lens.Lens' InventoryDeletionSummaryItem (Core.Maybe Core.Text)
inventoryDeletionSummaryItem_version = Lens.lens (\InventoryDeletionSummaryItem' {version} -> version) (\s@InventoryDeletionSummaryItem' {} a -> s {version = a} :: InventoryDeletionSummaryItem)

-- | A count of the number of deleted items.
inventoryDeletionSummaryItem_count :: Lens.Lens' InventoryDeletionSummaryItem (Core.Maybe Core.Int)
inventoryDeletionSummaryItem_count = Lens.lens (\InventoryDeletionSummaryItem' {count} -> count) (\s@InventoryDeletionSummaryItem' {} a -> s {count = a} :: InventoryDeletionSummaryItem)

instance Core.FromJSON InventoryDeletionSummaryItem where
  parseJSON =
    Core.withObject
      "InventoryDeletionSummaryItem"
      ( \x ->
          InventoryDeletionSummaryItem'
            Core.<$> (x Core..:? "RemainingCount")
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "Count")
      )

instance Core.Hashable InventoryDeletionSummaryItem

instance Core.NFData InventoryDeletionSummaryItem
