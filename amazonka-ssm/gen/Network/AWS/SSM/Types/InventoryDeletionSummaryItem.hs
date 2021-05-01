{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Either a count, remaining count, or a version number in a delete
-- inventory summary.
--
-- /See:/ 'newInventoryDeletionSummaryItem' smart constructor.
data InventoryDeletionSummaryItem = InventoryDeletionSummaryItem'
  { -- | The remaining number of items to delete.
    remainingCount :: Prelude.Maybe Prelude.Int,
    -- | The inventory type version.
    version :: Prelude.Maybe Prelude.Text,
    -- | A count of the number of deleted items.
    count :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      version = Prelude.Nothing,
      count = Prelude.Nothing
    }

-- | The remaining number of items to delete.
inventoryDeletionSummaryItem_remainingCount :: Lens.Lens' InventoryDeletionSummaryItem (Prelude.Maybe Prelude.Int)
inventoryDeletionSummaryItem_remainingCount = Lens.lens (\InventoryDeletionSummaryItem' {remainingCount} -> remainingCount) (\s@InventoryDeletionSummaryItem' {} a -> s {remainingCount = a} :: InventoryDeletionSummaryItem)

-- | The inventory type version.
inventoryDeletionSummaryItem_version :: Lens.Lens' InventoryDeletionSummaryItem (Prelude.Maybe Prelude.Text)
inventoryDeletionSummaryItem_version = Lens.lens (\InventoryDeletionSummaryItem' {version} -> version) (\s@InventoryDeletionSummaryItem' {} a -> s {version = a} :: InventoryDeletionSummaryItem)

-- | A count of the number of deleted items.
inventoryDeletionSummaryItem_count :: Lens.Lens' InventoryDeletionSummaryItem (Prelude.Maybe Prelude.Int)
inventoryDeletionSummaryItem_count = Lens.lens (\InventoryDeletionSummaryItem' {count} -> count) (\s@InventoryDeletionSummaryItem' {} a -> s {count = a} :: InventoryDeletionSummaryItem)

instance
  Prelude.FromJSON
    InventoryDeletionSummaryItem
  where
  parseJSON =
    Prelude.withObject
      "InventoryDeletionSummaryItem"
      ( \x ->
          InventoryDeletionSummaryItem'
            Prelude.<$> (x Prelude..:? "RemainingCount")
            Prelude.<*> (x Prelude..:? "Version")
            Prelude.<*> (x Prelude..:? "Count")
      )

instance
  Prelude.Hashable
    InventoryDeletionSummaryItem

instance Prelude.NFData InventoryDeletionSummaryItem
