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
-- Module      : Amazonka.SSM.Types.InventoryDeletionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InventoryDeletionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.InventoryDeletionSummaryItem

-- | Information about the delete operation.
--
-- /See:/ 'newInventoryDeletionSummary' smart constructor.
data InventoryDeletionSummary = InventoryDeletionSummary'
  { -- | Remaining number of items to delete.
    remainingCount :: Prelude.Maybe Prelude.Int,
    -- | A list of counts and versions for deleted items.
    summaryItems :: Prelude.Maybe [InventoryDeletionSummaryItem],
    -- | The total number of items to delete. This count doesn\'t change during
    -- the delete operation.
    totalCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InventoryDeletionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remainingCount', 'inventoryDeletionSummary_remainingCount' - Remaining number of items to delete.
--
-- 'summaryItems', 'inventoryDeletionSummary_summaryItems' - A list of counts and versions for deleted items.
--
-- 'totalCount', 'inventoryDeletionSummary_totalCount' - The total number of items to delete. This count doesn\'t change during
-- the delete operation.
newInventoryDeletionSummary ::
  InventoryDeletionSummary
newInventoryDeletionSummary =
  InventoryDeletionSummary'
    { remainingCount =
        Prelude.Nothing,
      summaryItems = Prelude.Nothing,
      totalCount = Prelude.Nothing
    }

-- | Remaining number of items to delete.
inventoryDeletionSummary_remainingCount :: Lens.Lens' InventoryDeletionSummary (Prelude.Maybe Prelude.Int)
inventoryDeletionSummary_remainingCount = Lens.lens (\InventoryDeletionSummary' {remainingCount} -> remainingCount) (\s@InventoryDeletionSummary' {} a -> s {remainingCount = a} :: InventoryDeletionSummary)

-- | A list of counts and versions for deleted items.
inventoryDeletionSummary_summaryItems :: Lens.Lens' InventoryDeletionSummary (Prelude.Maybe [InventoryDeletionSummaryItem])
inventoryDeletionSummary_summaryItems = Lens.lens (\InventoryDeletionSummary' {summaryItems} -> summaryItems) (\s@InventoryDeletionSummary' {} a -> s {summaryItems = a} :: InventoryDeletionSummary) Prelude.. Lens.mapping Lens.coerced

-- | The total number of items to delete. This count doesn\'t change during
-- the delete operation.
inventoryDeletionSummary_totalCount :: Lens.Lens' InventoryDeletionSummary (Prelude.Maybe Prelude.Int)
inventoryDeletionSummary_totalCount = Lens.lens (\InventoryDeletionSummary' {totalCount} -> totalCount) (\s@InventoryDeletionSummary' {} a -> s {totalCount = a} :: InventoryDeletionSummary)

instance Data.FromJSON InventoryDeletionSummary where
  parseJSON =
    Data.withObject
      "InventoryDeletionSummary"
      ( \x ->
          InventoryDeletionSummary'
            Prelude.<$> (x Data..:? "RemainingCount")
            Prelude.<*> (x Data..:? "SummaryItems" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TotalCount")
      )

instance Prelude.Hashable InventoryDeletionSummary where
  hashWithSalt _salt InventoryDeletionSummary' {..} =
    _salt
      `Prelude.hashWithSalt` remainingCount
      `Prelude.hashWithSalt` summaryItems
      `Prelude.hashWithSalt` totalCount

instance Prelude.NFData InventoryDeletionSummary where
  rnf InventoryDeletionSummary' {..} =
    Prelude.rnf remainingCount
      `Prelude.seq` Prelude.rnf summaryItems
      `Prelude.seq` Prelude.rnf totalCount
