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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.InventoryDeletionSummaryItem

-- | Information about the delete operation.
--
-- /See:/ 'mkInventoryDeletionSummary' smart constructor.
data InventoryDeletionSummary = InventoryDeletionSummary'
  { remainingCount ::
      Lude.Maybe Lude.Int,
    summaryItems ::
      Lude.Maybe [InventoryDeletionSummaryItem],
    totalCount :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InventoryDeletionSummary' with the minimum fields required to make a request.
--
-- * 'remainingCount' - Remaining number of items to delete.
-- * 'summaryItems' - A list of counts and versions for deleted items.
-- * 'totalCount' - The total number of items to delete. This count does not change during the delete operation.
mkInventoryDeletionSummary ::
  InventoryDeletionSummary
mkInventoryDeletionSummary =
  InventoryDeletionSummary'
    { remainingCount = Lude.Nothing,
      summaryItems = Lude.Nothing,
      totalCount = Lude.Nothing
    }

-- | Remaining number of items to delete.
--
-- /Note:/ Consider using 'remainingCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsRemainingCount :: Lens.Lens' InventoryDeletionSummary (Lude.Maybe Lude.Int)
idsRemainingCount = Lens.lens (remainingCount :: InventoryDeletionSummary -> Lude.Maybe Lude.Int) (\s a -> s {remainingCount = a} :: InventoryDeletionSummary)
{-# DEPRECATED idsRemainingCount "Use generic-lens or generic-optics with 'remainingCount' instead." #-}

-- | A list of counts and versions for deleted items.
--
-- /Note:/ Consider using 'summaryItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsSummaryItems :: Lens.Lens' InventoryDeletionSummary (Lude.Maybe [InventoryDeletionSummaryItem])
idsSummaryItems = Lens.lens (summaryItems :: InventoryDeletionSummary -> Lude.Maybe [InventoryDeletionSummaryItem]) (\s a -> s {summaryItems = a} :: InventoryDeletionSummary)
{-# DEPRECATED idsSummaryItems "Use generic-lens or generic-optics with 'summaryItems' instead." #-}

-- | The total number of items to delete. This count does not change during the delete operation.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsTotalCount :: Lens.Lens' InventoryDeletionSummary (Lude.Maybe Lude.Int)
idsTotalCount = Lens.lens (totalCount :: InventoryDeletionSummary -> Lude.Maybe Lude.Int) (\s a -> s {totalCount = a} :: InventoryDeletionSummary)
{-# DEPRECATED idsTotalCount "Use generic-lens or generic-optics with 'totalCount' instead." #-}

instance Lude.FromJSON InventoryDeletionSummary where
  parseJSON =
    Lude.withObject
      "InventoryDeletionSummary"
      ( \x ->
          InventoryDeletionSummary'
            Lude.<$> (x Lude..:? "RemainingCount")
            Lude.<*> (x Lude..:? "SummaryItems" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "TotalCount")
      )
