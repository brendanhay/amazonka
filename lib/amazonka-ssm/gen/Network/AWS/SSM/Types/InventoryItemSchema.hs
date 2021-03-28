{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryItemSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.InventoryItemSchema
  ( InventoryItemSchema (..)
  -- * Smart constructor
  , mkInventoryItemSchema
  -- * Lenses
  , iisTypeName
  , iisAttributes
  , iisDisplayName
  , iisVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.InventoryItemAttribute as Types
import qualified Network.AWS.SSM.Types.InventoryItemSchemaVersion as Types
import qualified Network.AWS.SSM.Types.InventoryItemTypeName as Types
import qualified Network.AWS.SSM.Types.InventoryTypeDisplayName as Types

-- | The inventory item schema definition. Users can use this to compose inventory query filters.
--
-- /See:/ 'mkInventoryItemSchema' smart constructor.
data InventoryItemSchema = InventoryItemSchema'
  { typeName :: Types.InventoryItemTypeName
    -- ^ The name of the inventory type. Default inventory item type names start with AWS. Custom inventory type names will start with Custom. Default inventory item types include the following: AWS:AWSComponent, AWS:Application, AWS:InstanceInformation, AWS:Network, and AWS:WindowsUpdate.
  , attributes :: Core.NonEmpty Types.InventoryItemAttribute
    -- ^ The schema attributes for inventory. This contains data type and attribute name.
  , displayName :: Core.Maybe Types.InventoryTypeDisplayName
    -- ^ The alias name of the inventory type. The alias name is used for display purposes.
  , version :: Core.Maybe Types.InventoryItemSchemaVersion
    -- ^ The schema version for the inventory item.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InventoryItemSchema' value with any optional fields omitted.
mkInventoryItemSchema
    :: Types.InventoryItemTypeName -- ^ 'typeName'
    -> Core.NonEmpty Types.InventoryItemAttribute -- ^ 'attributes'
    -> InventoryItemSchema
mkInventoryItemSchema typeName attributes
  = InventoryItemSchema'{typeName, attributes,
                         displayName = Core.Nothing, version = Core.Nothing}

-- | The name of the inventory type. Default inventory item type names start with AWS. Custom inventory type names will start with Custom. Default inventory item types include the following: AWS:AWSComponent, AWS:Application, AWS:InstanceInformation, AWS:Network, and AWS:WindowsUpdate.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iisTypeName :: Lens.Lens' InventoryItemSchema Types.InventoryItemTypeName
iisTypeName = Lens.field @"typeName"
{-# INLINEABLE iisTypeName #-}
{-# DEPRECATED typeName "Use generic-lens or generic-optics with 'typeName' instead"  #-}

-- | The schema attributes for inventory. This contains data type and attribute name.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iisAttributes :: Lens.Lens' InventoryItemSchema (Core.NonEmpty Types.InventoryItemAttribute)
iisAttributes = Lens.field @"attributes"
{-# INLINEABLE iisAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The alias name of the inventory type. The alias name is used for display purposes.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iisDisplayName :: Lens.Lens' InventoryItemSchema (Core.Maybe Types.InventoryTypeDisplayName)
iisDisplayName = Lens.field @"displayName"
{-# INLINEABLE iisDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | The schema version for the inventory item.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iisVersion :: Lens.Lens' InventoryItemSchema (Core.Maybe Types.InventoryItemSchemaVersion)
iisVersion = Lens.field @"version"
{-# INLINEABLE iisVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON InventoryItemSchema where
        parseJSON
          = Core.withObject "InventoryItemSchema" Core.$
              \ x ->
                InventoryItemSchema' Core.<$>
                  (x Core..: "TypeName") Core.<*> x Core..: "Attributes" Core.<*>
                    x Core..:? "DisplayName"
                    Core.<*> x Core..:? "Version"
