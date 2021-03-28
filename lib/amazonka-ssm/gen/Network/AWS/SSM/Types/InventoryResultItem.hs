{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryResultItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.InventoryResultItem
  ( InventoryResultItem (..)
  -- * Smart constructor
  , mkInventoryResultItem
  -- * Lenses
  , iriTypeName
  , iriSchemaVersion
  , iriContent
  , iriCaptureTime
  , iriContentHash
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AttributeName as Types
import qualified Network.AWS.SSM.Types.AttributeValue as Types
import qualified Network.AWS.SSM.Types.InventoryItemCaptureTime as Types
import qualified Network.AWS.SSM.Types.InventoryItemContentHash as Types
import qualified Network.AWS.SSM.Types.InventoryItemSchemaVersion as Types
import qualified Network.AWS.SSM.Types.InventoryItemTypeName as Types

-- | The inventory result item.
--
-- /See:/ 'mkInventoryResultItem' smart constructor.
data InventoryResultItem = InventoryResultItem'
  { typeName :: Types.InventoryItemTypeName
    -- ^ The name of the inventory result item type.
  , schemaVersion :: Types.InventoryItemSchemaVersion
    -- ^ The schema version for the inventory result item/
  , content :: [Core.HashMap Types.AttributeName Types.AttributeValue]
    -- ^ Contains all the inventory data of the item type. Results include attribute names and values. 
  , captureTime :: Core.Maybe Types.InventoryItemCaptureTime
    -- ^ The time inventory item data was captured.
  , contentHash :: Core.Maybe Types.InventoryItemContentHash
    -- ^ MD5 hash of the inventory item type contents. The content hash is used to determine whether to update inventory information. The PutInventory API does not update the inventory item type contents if the MD5 hash has not changed since last update. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InventoryResultItem' value with any optional fields omitted.
mkInventoryResultItem
    :: Types.InventoryItemTypeName -- ^ 'typeName'
    -> Types.InventoryItemSchemaVersion -- ^ 'schemaVersion'
    -> InventoryResultItem
mkInventoryResultItem typeName schemaVersion
  = InventoryResultItem'{typeName, schemaVersion,
                         content = Core.mempty, captureTime = Core.Nothing,
                         contentHash = Core.Nothing}

-- | The name of the inventory result item type.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriTypeName :: Lens.Lens' InventoryResultItem Types.InventoryItemTypeName
iriTypeName = Lens.field @"typeName"
{-# INLINEABLE iriTypeName #-}
{-# DEPRECATED typeName "Use generic-lens or generic-optics with 'typeName' instead"  #-}

-- | The schema version for the inventory result item/
--
-- /Note:/ Consider using 'schemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriSchemaVersion :: Lens.Lens' InventoryResultItem Types.InventoryItemSchemaVersion
iriSchemaVersion = Lens.field @"schemaVersion"
{-# INLINEABLE iriSchemaVersion #-}
{-# DEPRECATED schemaVersion "Use generic-lens or generic-optics with 'schemaVersion' instead"  #-}

-- | Contains all the inventory data of the item type. Results include attribute names and values. 
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriContent :: Lens.Lens' InventoryResultItem [Core.HashMap Types.AttributeName Types.AttributeValue]
iriContent = Lens.field @"content"
{-# INLINEABLE iriContent #-}
{-# DEPRECATED content "Use generic-lens or generic-optics with 'content' instead"  #-}

-- | The time inventory item data was captured.
--
-- /Note:/ Consider using 'captureTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriCaptureTime :: Lens.Lens' InventoryResultItem (Core.Maybe Types.InventoryItemCaptureTime)
iriCaptureTime = Lens.field @"captureTime"
{-# INLINEABLE iriCaptureTime #-}
{-# DEPRECATED captureTime "Use generic-lens or generic-optics with 'captureTime' instead"  #-}

-- | MD5 hash of the inventory item type contents. The content hash is used to determine whether to update inventory information. The PutInventory API does not update the inventory item type contents if the MD5 hash has not changed since last update. 
--
-- /Note:/ Consider using 'contentHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriContentHash :: Lens.Lens' InventoryResultItem (Core.Maybe Types.InventoryItemContentHash)
iriContentHash = Lens.field @"contentHash"
{-# INLINEABLE iriContentHash #-}
{-# DEPRECATED contentHash "Use generic-lens or generic-optics with 'contentHash' instead"  #-}

instance Core.FromJSON InventoryResultItem where
        parseJSON
          = Core.withObject "InventoryResultItem" Core.$
              \ x ->
                InventoryResultItem' Core.<$>
                  (x Core..: "TypeName") Core.<*> x Core..: "SchemaVersion" Core.<*>
                    x Core..:? "Content" Core..!= Core.mempty
                    Core.<*> x Core..:? "CaptureTime"
                    Core.<*> x Core..:? "ContentHash"
