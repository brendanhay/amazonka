{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.InventoryItem
  ( InventoryItem (..)
  -- * Smart constructor
  , mkInventoryItem
  -- * Lenses
  , iiTypeName
  , iiSchemaVersion
  , iiCaptureTime
  , iiContent
  , iiContentHash
  , iiContext
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AttributeName as Types
import qualified Network.AWS.SSM.Types.AttributeValue as Types
import qualified Network.AWS.SSM.Types.CaptureTime as Types
import qualified Network.AWS.SSM.Types.ContentHash as Types
import qualified Network.AWS.SSM.Types.SchemaVersion as Types
import qualified Network.AWS.SSM.Types.TypeName as Types

-- | Information collected from managed instances based on your inventory policy document
--
-- /See:/ 'mkInventoryItem' smart constructor.
data InventoryItem = InventoryItem'
  { typeName :: Types.TypeName
    -- ^ The name of the inventory type. Default inventory item type names start with AWS. Custom inventory type names will start with Custom. Default inventory item types include the following: AWS:AWSComponent, AWS:Application, AWS:InstanceInformation, AWS:Network, and AWS:WindowsUpdate.
  , schemaVersion :: Types.SchemaVersion
    -- ^ The schema version for the inventory item.
  , captureTime :: Types.CaptureTime
    -- ^ The time the inventory information was collected.
  , content :: Core.Maybe [Core.HashMap Types.AttributeName Types.AttributeValue]
    -- ^ The inventory data of the inventory type.
  , contentHash :: Core.Maybe Types.ContentHash
    -- ^ MD5 hash of the inventory item type contents. The content hash is used to determine whether to update inventory information. The PutInventory API does not update the inventory item type contents if the MD5 hash has not changed since last update. 
  , context :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue)
    -- ^ A map of associated properties for a specified inventory type. For example, with this attribute, you can specify the @ExecutionId@ , @ExecutionType@ , @ComplianceType@ properties of the @AWS:ComplianceItem@ type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InventoryItem' value with any optional fields omitted.
mkInventoryItem
    :: Types.TypeName -- ^ 'typeName'
    -> Types.SchemaVersion -- ^ 'schemaVersion'
    -> Types.CaptureTime -- ^ 'captureTime'
    -> InventoryItem
mkInventoryItem typeName schemaVersion captureTime
  = InventoryItem'{typeName, schemaVersion, captureTime,
                   content = Core.Nothing, contentHash = Core.Nothing,
                   context = Core.Nothing}

-- | The name of the inventory type. Default inventory item type names start with AWS. Custom inventory type names will start with Custom. Default inventory item types include the following: AWS:AWSComponent, AWS:Application, AWS:InstanceInformation, AWS:Network, and AWS:WindowsUpdate.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiTypeName :: Lens.Lens' InventoryItem Types.TypeName
iiTypeName = Lens.field @"typeName"
{-# INLINEABLE iiTypeName #-}
{-# DEPRECATED typeName "Use generic-lens or generic-optics with 'typeName' instead"  #-}

-- | The schema version for the inventory item.
--
-- /Note:/ Consider using 'schemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiSchemaVersion :: Lens.Lens' InventoryItem Types.SchemaVersion
iiSchemaVersion = Lens.field @"schemaVersion"
{-# INLINEABLE iiSchemaVersion #-}
{-# DEPRECATED schemaVersion "Use generic-lens or generic-optics with 'schemaVersion' instead"  #-}

-- | The time the inventory information was collected.
--
-- /Note:/ Consider using 'captureTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiCaptureTime :: Lens.Lens' InventoryItem Types.CaptureTime
iiCaptureTime = Lens.field @"captureTime"
{-# INLINEABLE iiCaptureTime #-}
{-# DEPRECATED captureTime "Use generic-lens or generic-optics with 'captureTime' instead"  #-}

-- | The inventory data of the inventory type.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiContent :: Lens.Lens' InventoryItem (Core.Maybe [Core.HashMap Types.AttributeName Types.AttributeValue])
iiContent = Lens.field @"content"
{-# INLINEABLE iiContent #-}
{-# DEPRECATED content "Use generic-lens or generic-optics with 'content' instead"  #-}

-- | MD5 hash of the inventory item type contents. The content hash is used to determine whether to update inventory information. The PutInventory API does not update the inventory item type contents if the MD5 hash has not changed since last update. 
--
-- /Note:/ Consider using 'contentHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiContentHash :: Lens.Lens' InventoryItem (Core.Maybe Types.ContentHash)
iiContentHash = Lens.field @"contentHash"
{-# INLINEABLE iiContentHash #-}
{-# DEPRECATED contentHash "Use generic-lens or generic-optics with 'contentHash' instead"  #-}

-- | A map of associated properties for a specified inventory type. For example, with this attribute, you can specify the @ExecutionId@ , @ExecutionType@ , @ComplianceType@ properties of the @AWS:ComplianceItem@ type.
--
-- /Note:/ Consider using 'context' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiContext :: Lens.Lens' InventoryItem (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
iiContext = Lens.field @"context"
{-# INLINEABLE iiContext #-}
{-# DEPRECATED context "Use generic-lens or generic-optics with 'context' instead"  #-}

instance Core.FromJSON InventoryItem where
        toJSON InventoryItem{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TypeName" Core..= typeName),
                  Core.Just ("SchemaVersion" Core..= schemaVersion),
                  Core.Just ("CaptureTime" Core..= captureTime),
                  ("Content" Core..=) Core.<$> content,
                  ("ContentHash" Core..=) Core.<$> contentHash,
                  ("Context" Core..=) Core.<$> context])
