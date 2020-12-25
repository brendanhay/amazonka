{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryItemAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryItemAttribute
  ( InventoryItemAttribute (..),

    -- * Smart constructor
    mkInventoryItemAttribute,

    -- * Lenses
    iiaName,
    iiaDataType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.InventoryAttributeDataType as Types
import qualified Network.AWS.SSM.Types.InventoryItemAttributeName as Types

-- | Attributes are the entries within the inventory item content. It contains name and value.
--
-- /See:/ 'mkInventoryItemAttribute' smart constructor.
data InventoryItemAttribute = InventoryItemAttribute'
  { -- | Name of the inventory item attribute.
    name :: Types.InventoryItemAttributeName,
    -- | The data type of the inventory item attribute.
    dataType :: Types.InventoryAttributeDataType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InventoryItemAttribute' value with any optional fields omitted.
mkInventoryItemAttribute ::
  -- | 'name'
  Types.InventoryItemAttributeName ->
  -- | 'dataType'
  Types.InventoryAttributeDataType ->
  InventoryItemAttribute
mkInventoryItemAttribute name dataType =
  InventoryItemAttribute' {name, dataType}

-- | Name of the inventory item attribute.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiaName :: Lens.Lens' InventoryItemAttribute Types.InventoryItemAttributeName
iiaName = Lens.field @"name"
{-# DEPRECATED iiaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The data type of the inventory item attribute.
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiaDataType :: Lens.Lens' InventoryItemAttribute Types.InventoryAttributeDataType
iiaDataType = Lens.field @"dataType"
{-# DEPRECATED iiaDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

instance Core.FromJSON InventoryItemAttribute where
  parseJSON =
    Core.withObject "InventoryItemAttribute" Core.$
      \x ->
        InventoryItemAttribute'
          Core.<$> (x Core..: "Name") Core.<*> (x Core..: "DataType")
