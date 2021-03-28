{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryResultEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.InventoryResultEntity
  ( InventoryResultEntity (..)
  -- * Smart constructor
  , mkInventoryResultEntity
  -- * Lenses
  , ireData
  , ireId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.Id as Types
import qualified Network.AWS.SSM.Types.InventoryResultItem as Types
import qualified Network.AWS.SSM.Types.InventoryResultItemKey as Types

-- | Inventory query results.
--
-- /See:/ 'mkInventoryResultEntity' smart constructor.
data InventoryResultEntity = InventoryResultEntity'
  { data' :: Core.Maybe (Core.HashMap Types.InventoryResultItemKey Types.InventoryResultItem)
    -- ^ The data section in the inventory result entity JSON.
  , id :: Core.Maybe Types.Id
    -- ^ ID of the inventory result entity. For example, for managed instance inventory the result will be the managed instance ID. For EC2 instance inventory, the result will be the instance ID. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InventoryResultEntity' value with any optional fields omitted.
mkInventoryResultEntity
    :: InventoryResultEntity
mkInventoryResultEntity
  = InventoryResultEntity'{data' = Core.Nothing, id = Core.Nothing}

-- | The data section in the inventory result entity JSON.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ireData :: Lens.Lens' InventoryResultEntity (Core.Maybe (Core.HashMap Types.InventoryResultItemKey Types.InventoryResultItem))
ireData = Lens.field @"data'"
{-# INLINEABLE ireData #-}
{-# DEPRECATED data' "Use generic-lens or generic-optics with 'data'' instead"  #-}

-- | ID of the inventory result entity. For example, for managed instance inventory the result will be the managed instance ID. For EC2 instance inventory, the result will be the instance ID. 
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ireId :: Lens.Lens' InventoryResultEntity (Core.Maybe Types.Id)
ireId = Lens.field @"id"
{-# INLINEABLE ireId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.FromJSON InventoryResultEntity where
        parseJSON
          = Core.withObject "InventoryResultEntity" Core.$
              \ x ->
                InventoryResultEntity' Core.<$>
                  (x Core..:? "Data") Core.<*> x Core..:? "Id"
