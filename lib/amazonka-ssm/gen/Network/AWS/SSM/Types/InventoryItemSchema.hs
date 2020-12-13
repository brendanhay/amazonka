{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryItemSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryItemSchema
  ( InventoryItemSchema (..),

    -- * Smart constructor
    mkInventoryItemSchema,

    -- * Lenses
    iisTypeName,
    iisAttributes,
    iisVersion,
    iisDisplayName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.InventoryItemAttribute

-- | The inventory item schema definition. Users can use this to compose inventory query filters.
--
-- /See:/ 'mkInventoryItemSchema' smart constructor.
data InventoryItemSchema = InventoryItemSchema'
  { -- | The name of the inventory type. Default inventory item type names start with AWS. Custom inventory type names will start with Custom. Default inventory item types include the following: AWS:AWSComponent, AWS:Application, AWS:InstanceInformation, AWS:Network, and AWS:WindowsUpdate.
    typeName :: Lude.Text,
    -- | The schema attributes for inventory. This contains data type and attribute name.
    attributes :: Lude.NonEmpty InventoryItemAttribute,
    -- | The schema version for the inventory item.
    version :: Lude.Maybe Lude.Text,
    -- | The alias name of the inventory type. The alias name is used for display purposes.
    displayName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InventoryItemSchema' with the minimum fields required to make a request.
--
-- * 'typeName' - The name of the inventory type. Default inventory item type names start with AWS. Custom inventory type names will start with Custom. Default inventory item types include the following: AWS:AWSComponent, AWS:Application, AWS:InstanceInformation, AWS:Network, and AWS:WindowsUpdate.
-- * 'attributes' - The schema attributes for inventory. This contains data type and attribute name.
-- * 'version' - The schema version for the inventory item.
-- * 'displayName' - The alias name of the inventory type. The alias name is used for display purposes.
mkInventoryItemSchema ::
  -- | 'typeName'
  Lude.Text ->
  -- | 'attributes'
  Lude.NonEmpty InventoryItemAttribute ->
  InventoryItemSchema
mkInventoryItemSchema pTypeName_ pAttributes_ =
  InventoryItemSchema'
    { typeName = pTypeName_,
      attributes = pAttributes_,
      version = Lude.Nothing,
      displayName = Lude.Nothing
    }

-- | The name of the inventory type. Default inventory item type names start with AWS. Custom inventory type names will start with Custom. Default inventory item types include the following: AWS:AWSComponent, AWS:Application, AWS:InstanceInformation, AWS:Network, and AWS:WindowsUpdate.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iisTypeName :: Lens.Lens' InventoryItemSchema Lude.Text
iisTypeName = Lens.lens (typeName :: InventoryItemSchema -> Lude.Text) (\s a -> s {typeName = a} :: InventoryItemSchema)
{-# DEPRECATED iisTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The schema attributes for inventory. This contains data type and attribute name.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iisAttributes :: Lens.Lens' InventoryItemSchema (Lude.NonEmpty InventoryItemAttribute)
iisAttributes = Lens.lens (attributes :: InventoryItemSchema -> Lude.NonEmpty InventoryItemAttribute) (\s a -> s {attributes = a} :: InventoryItemSchema)
{-# DEPRECATED iisAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The schema version for the inventory item.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iisVersion :: Lens.Lens' InventoryItemSchema (Lude.Maybe Lude.Text)
iisVersion = Lens.lens (version :: InventoryItemSchema -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: InventoryItemSchema)
{-# DEPRECATED iisVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The alias name of the inventory type. The alias name is used for display purposes.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iisDisplayName :: Lens.Lens' InventoryItemSchema (Lude.Maybe Lude.Text)
iisDisplayName = Lens.lens (displayName :: InventoryItemSchema -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: InventoryItemSchema)
{-# DEPRECATED iisDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

instance Lude.FromJSON InventoryItemSchema where
  parseJSON =
    Lude.withObject
      "InventoryItemSchema"
      ( \x ->
          InventoryItemSchema'
            Lude.<$> (x Lude..: "TypeName")
            Lude.<*> (x Lude..: "Attributes")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "DisplayName")
      )
