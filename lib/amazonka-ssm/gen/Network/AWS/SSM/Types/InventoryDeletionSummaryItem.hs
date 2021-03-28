{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryDeletionSummaryItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.InventoryDeletionSummaryItem
  ( InventoryDeletionSummaryItem (..)
  -- * Smart constructor
  , mkInventoryDeletionSummaryItem
  -- * Lenses
  , idsiCount
  , idsiRemainingCount
  , idsiVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.InventoryItemSchemaVersion as Types

-- | Either a count, remaining count, or a version number in a delete inventory summary.
--
-- /See:/ 'mkInventoryDeletionSummaryItem' smart constructor.
data InventoryDeletionSummaryItem = InventoryDeletionSummaryItem'
  { count :: Core.Maybe Core.Int
    -- ^ A count of the number of deleted items.
  , remainingCount :: Core.Maybe Core.Int
    -- ^ The remaining number of items to delete.
  , version :: Core.Maybe Types.InventoryItemSchemaVersion
    -- ^ The inventory type version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InventoryDeletionSummaryItem' value with any optional fields omitted.
mkInventoryDeletionSummaryItem
    :: InventoryDeletionSummaryItem
mkInventoryDeletionSummaryItem
  = InventoryDeletionSummaryItem'{count = Core.Nothing,
                                  remainingCount = Core.Nothing, version = Core.Nothing}

-- | A count of the number of deleted items.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsiCount :: Lens.Lens' InventoryDeletionSummaryItem (Core.Maybe Core.Int)
idsiCount = Lens.field @"count"
{-# INLINEABLE idsiCount #-}
{-# DEPRECATED count "Use generic-lens or generic-optics with 'count' instead"  #-}

-- | The remaining number of items to delete.
--
-- /Note:/ Consider using 'remainingCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsiRemainingCount :: Lens.Lens' InventoryDeletionSummaryItem (Core.Maybe Core.Int)
idsiRemainingCount = Lens.field @"remainingCount"
{-# INLINEABLE idsiRemainingCount #-}
{-# DEPRECATED remainingCount "Use generic-lens or generic-optics with 'remainingCount' instead"  #-}

-- | The inventory type version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsiVersion :: Lens.Lens' InventoryDeletionSummaryItem (Core.Maybe Types.InventoryItemSchemaVersion)
idsiVersion = Lens.field @"version"
{-# INLINEABLE idsiVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON InventoryDeletionSummaryItem where
        parseJSON
          = Core.withObject "InventoryDeletionSummaryItem" Core.$
              \ x ->
                InventoryDeletionSummaryItem' Core.<$>
                  (x Core..:? "Count") Core.<*> x Core..:? "RemainingCount" Core.<*>
                    x Core..:? "Version"
