{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryDeletionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryDeletionSummary
  ( InventoryDeletionSummary (..),

    -- * Smart constructor
    mkInventoryDeletionSummary,

    -- * Lenses
    idsRemainingCount,
    idsSummaryItems,
    idsTotalCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.InventoryDeletionSummaryItem as Types

-- | Information about the delete operation.
--
-- /See:/ 'mkInventoryDeletionSummary' smart constructor.
data InventoryDeletionSummary = InventoryDeletionSummary'
  { -- | Remaining number of items to delete.
    remainingCount :: Core.Maybe Core.Int,
    -- | A list of counts and versions for deleted items.
    summaryItems :: Core.Maybe [Types.InventoryDeletionSummaryItem],
    -- | The total number of items to delete. This count does not change during the delete operation.
    totalCount :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InventoryDeletionSummary' value with any optional fields omitted.
mkInventoryDeletionSummary ::
  InventoryDeletionSummary
mkInventoryDeletionSummary =
  InventoryDeletionSummary'
    { remainingCount = Core.Nothing,
      summaryItems = Core.Nothing,
      totalCount = Core.Nothing
    }

-- | Remaining number of items to delete.
--
-- /Note:/ Consider using 'remainingCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsRemainingCount :: Lens.Lens' InventoryDeletionSummary (Core.Maybe Core.Int)
idsRemainingCount = Lens.field @"remainingCount"
{-# DEPRECATED idsRemainingCount "Use generic-lens or generic-optics with 'remainingCount' instead." #-}

-- | A list of counts and versions for deleted items.
--
-- /Note:/ Consider using 'summaryItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsSummaryItems :: Lens.Lens' InventoryDeletionSummary (Core.Maybe [Types.InventoryDeletionSummaryItem])
idsSummaryItems = Lens.field @"summaryItems"
{-# DEPRECATED idsSummaryItems "Use generic-lens or generic-optics with 'summaryItems' instead." #-}

-- | The total number of items to delete. This count does not change during the delete operation.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsTotalCount :: Lens.Lens' InventoryDeletionSummary (Core.Maybe Core.Int)
idsTotalCount = Lens.field @"totalCount"
{-# DEPRECATED idsTotalCount "Use generic-lens or generic-optics with 'totalCount' instead." #-}

instance Core.FromJSON InventoryDeletionSummary where
  parseJSON =
    Core.withObject "InventoryDeletionSummary" Core.$
      \x ->
        InventoryDeletionSummary'
          Core.<$> (x Core..:? "RemainingCount")
          Core.<*> (x Core..:? "SummaryItems")
          Core.<*> (x Core..:? "TotalCount")
