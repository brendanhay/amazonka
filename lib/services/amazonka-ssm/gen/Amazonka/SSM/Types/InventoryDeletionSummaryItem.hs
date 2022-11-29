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
-- Module      : Amazonka.SSM.Types.InventoryDeletionSummaryItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InventoryDeletionSummaryItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Either a count, remaining count, or a version number in a delete
-- inventory summary.
--
-- /See:/ 'newInventoryDeletionSummaryItem' smart constructor.
data InventoryDeletionSummaryItem = InventoryDeletionSummaryItem'
  { -- | The remaining number of items to delete.
    remainingCount :: Prelude.Maybe Prelude.Int,
    -- | A count of the number of deleted items.
    count :: Prelude.Maybe Prelude.Int,
    -- | The inventory type version.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'count', 'inventoryDeletionSummaryItem_count' - A count of the number of deleted items.
--
-- 'version', 'inventoryDeletionSummaryItem_version' - The inventory type version.
newInventoryDeletionSummaryItem ::
  InventoryDeletionSummaryItem
newInventoryDeletionSummaryItem =
  InventoryDeletionSummaryItem'
    { remainingCount =
        Prelude.Nothing,
      count = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The remaining number of items to delete.
inventoryDeletionSummaryItem_remainingCount :: Lens.Lens' InventoryDeletionSummaryItem (Prelude.Maybe Prelude.Int)
inventoryDeletionSummaryItem_remainingCount = Lens.lens (\InventoryDeletionSummaryItem' {remainingCount} -> remainingCount) (\s@InventoryDeletionSummaryItem' {} a -> s {remainingCount = a} :: InventoryDeletionSummaryItem)

-- | A count of the number of deleted items.
inventoryDeletionSummaryItem_count :: Lens.Lens' InventoryDeletionSummaryItem (Prelude.Maybe Prelude.Int)
inventoryDeletionSummaryItem_count = Lens.lens (\InventoryDeletionSummaryItem' {count} -> count) (\s@InventoryDeletionSummaryItem' {} a -> s {count = a} :: InventoryDeletionSummaryItem)

-- | The inventory type version.
inventoryDeletionSummaryItem_version :: Lens.Lens' InventoryDeletionSummaryItem (Prelude.Maybe Prelude.Text)
inventoryDeletionSummaryItem_version = Lens.lens (\InventoryDeletionSummaryItem' {version} -> version) (\s@InventoryDeletionSummaryItem' {} a -> s {version = a} :: InventoryDeletionSummaryItem)

instance Core.FromJSON InventoryDeletionSummaryItem where
  parseJSON =
    Core.withObject
      "InventoryDeletionSummaryItem"
      ( \x ->
          InventoryDeletionSummaryItem'
            Prelude.<$> (x Core..:? "RemainingCount")
            Prelude.<*> (x Core..:? "Count")
            Prelude.<*> (x Core..:? "Version")
      )

instance
  Prelude.Hashable
    InventoryDeletionSummaryItem
  where
  hashWithSalt _salt InventoryDeletionSummaryItem' {..} =
    _salt `Prelude.hashWithSalt` remainingCount
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` version

instance Prelude.NFData InventoryDeletionSummaryItem where
  rnf InventoryDeletionSummaryItem' {..} =
    Prelude.rnf remainingCount
      `Prelude.seq` Prelude.rnf count
      `Prelude.seq` Prelude.rnf version
