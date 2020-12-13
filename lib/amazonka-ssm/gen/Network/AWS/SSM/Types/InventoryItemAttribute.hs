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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.InventoryAttributeDataType

-- | Attributes are the entries within the inventory item content. It contains name and value.
--
-- /See:/ 'mkInventoryItemAttribute' smart constructor.
data InventoryItemAttribute = InventoryItemAttribute'
  { -- | Name of the inventory item attribute.
    name :: Lude.Text,
    -- | The data type of the inventory item attribute.
    dataType :: InventoryAttributeDataType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InventoryItemAttribute' with the minimum fields required to make a request.
--
-- * 'name' - Name of the inventory item attribute.
-- * 'dataType' - The data type of the inventory item attribute.
mkInventoryItemAttribute ::
  -- | 'name'
  Lude.Text ->
  -- | 'dataType'
  InventoryAttributeDataType ->
  InventoryItemAttribute
mkInventoryItemAttribute pName_ pDataType_ =
  InventoryItemAttribute' {name = pName_, dataType = pDataType_}

-- | Name of the inventory item attribute.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiaName :: Lens.Lens' InventoryItemAttribute Lude.Text
iiaName = Lens.lens (name :: InventoryItemAttribute -> Lude.Text) (\s a -> s {name = a} :: InventoryItemAttribute)
{-# DEPRECATED iiaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The data type of the inventory item attribute.
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiaDataType :: Lens.Lens' InventoryItemAttribute InventoryAttributeDataType
iiaDataType = Lens.lens (dataType :: InventoryItemAttribute -> InventoryAttributeDataType) (\s a -> s {dataType = a} :: InventoryItemAttribute)
{-# DEPRECATED iiaDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

instance Lude.FromJSON InventoryItemAttribute where
  parseJSON =
    Lude.withObject
      "InventoryItemAttribute"
      ( \x ->
          InventoryItemAttribute'
            Lude.<$> (x Lude..: "Name") Lude.<*> (x Lude..: "DataType")
      )
