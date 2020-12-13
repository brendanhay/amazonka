{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryDeletionSummaryItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryDeletionSummaryItem
  ( InventoryDeletionSummaryItem (..),

    -- * Smart constructor
    mkInventoryDeletionSummaryItem,

    -- * Lenses
    idsiRemainingCount,
    idsiCount,
    idsiVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Either a count, remaining count, or a version number in a delete inventory summary.
--
-- /See:/ 'mkInventoryDeletionSummaryItem' smart constructor.
data InventoryDeletionSummaryItem = InventoryDeletionSummaryItem'
  { -- | The remaining number of items to delete.
    remainingCount :: Lude.Maybe Lude.Int,
    -- | A count of the number of deleted items.
    count :: Lude.Maybe Lude.Int,
    -- | The inventory type version.
    version :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InventoryDeletionSummaryItem' with the minimum fields required to make a request.
--
-- * 'remainingCount' - The remaining number of items to delete.
-- * 'count' - A count of the number of deleted items.
-- * 'version' - The inventory type version.
mkInventoryDeletionSummaryItem ::
  InventoryDeletionSummaryItem
mkInventoryDeletionSummaryItem =
  InventoryDeletionSummaryItem'
    { remainingCount = Lude.Nothing,
      count = Lude.Nothing,
      version = Lude.Nothing
    }

-- | The remaining number of items to delete.
--
-- /Note:/ Consider using 'remainingCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsiRemainingCount :: Lens.Lens' InventoryDeletionSummaryItem (Lude.Maybe Lude.Int)
idsiRemainingCount = Lens.lens (remainingCount :: InventoryDeletionSummaryItem -> Lude.Maybe Lude.Int) (\s a -> s {remainingCount = a} :: InventoryDeletionSummaryItem)
{-# DEPRECATED idsiRemainingCount "Use generic-lens or generic-optics with 'remainingCount' instead." #-}

-- | A count of the number of deleted items.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsiCount :: Lens.Lens' InventoryDeletionSummaryItem (Lude.Maybe Lude.Int)
idsiCount = Lens.lens (count :: InventoryDeletionSummaryItem -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: InventoryDeletionSummaryItem)
{-# DEPRECATED idsiCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The inventory type version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsiVersion :: Lens.Lens' InventoryDeletionSummaryItem (Lude.Maybe Lude.Text)
idsiVersion = Lens.lens (version :: InventoryDeletionSummaryItem -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: InventoryDeletionSummaryItem)
{-# DEPRECATED idsiVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.FromJSON InventoryDeletionSummaryItem where
  parseJSON =
    Lude.withObject
      "InventoryDeletionSummaryItem"
      ( \x ->
          InventoryDeletionSummaryItem'
            Lude.<$> (x Lude..:? "RemainingCount")
            Lude.<*> (x Lude..:? "Count")
            Lude.<*> (x Lude..:? "Version")
      )
