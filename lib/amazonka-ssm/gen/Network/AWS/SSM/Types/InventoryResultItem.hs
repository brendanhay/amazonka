-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryResultItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryResultItem
  ( InventoryResultItem (..),

    -- * Smart constructor
    mkInventoryResultItem,

    -- * Lenses
    iriContentHash,
    iriCaptureTime,
    iriTypeName,
    iriSchemaVersion,
    iriContent,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The inventory result item.
--
-- /See:/ 'mkInventoryResultItem' smart constructor.
data InventoryResultItem = InventoryResultItem'
  { contentHash ::
      Lude.Maybe Lude.Text,
    captureTime :: Lude.Maybe Lude.Text,
    typeName :: Lude.Text,
    schemaVersion :: Lude.Text,
    content :: [Lude.HashMap Lude.Text (Lude.Text)]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InventoryResultItem' with the minimum fields required to make a request.
--
-- * 'captureTime' - The time inventory item data was captured.
-- * 'content' - Contains all the inventory data of the item type. Results include attribute names and values.
-- * 'contentHash' - MD5 hash of the inventory item type contents. The content hash is used to determine whether to update inventory information. The PutInventory API does not update the inventory item type contents if the MD5 hash has not changed since last update.
-- * 'schemaVersion' - The schema version for the inventory result item/
-- * 'typeName' - The name of the inventory result item type.
mkInventoryResultItem ::
  -- | 'typeName'
  Lude.Text ->
  -- | 'schemaVersion'
  Lude.Text ->
  InventoryResultItem
mkInventoryResultItem pTypeName_ pSchemaVersion_ =
  InventoryResultItem'
    { contentHash = Lude.Nothing,
      captureTime = Lude.Nothing,
      typeName = pTypeName_,
      schemaVersion = pSchemaVersion_,
      content = Lude.mempty
    }

-- | MD5 hash of the inventory item type contents. The content hash is used to determine whether to update inventory information. The PutInventory API does not update the inventory item type contents if the MD5 hash has not changed since last update.
--
-- /Note:/ Consider using 'contentHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriContentHash :: Lens.Lens' InventoryResultItem (Lude.Maybe Lude.Text)
iriContentHash = Lens.lens (contentHash :: InventoryResultItem -> Lude.Maybe Lude.Text) (\s a -> s {contentHash = a} :: InventoryResultItem)
{-# DEPRECATED iriContentHash "Use generic-lens or generic-optics with 'contentHash' instead." #-}

-- | The time inventory item data was captured.
--
-- /Note:/ Consider using 'captureTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriCaptureTime :: Lens.Lens' InventoryResultItem (Lude.Maybe Lude.Text)
iriCaptureTime = Lens.lens (captureTime :: InventoryResultItem -> Lude.Maybe Lude.Text) (\s a -> s {captureTime = a} :: InventoryResultItem)
{-# DEPRECATED iriCaptureTime "Use generic-lens or generic-optics with 'captureTime' instead." #-}

-- | The name of the inventory result item type.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriTypeName :: Lens.Lens' InventoryResultItem Lude.Text
iriTypeName = Lens.lens (typeName :: InventoryResultItem -> Lude.Text) (\s a -> s {typeName = a} :: InventoryResultItem)
{-# DEPRECATED iriTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The schema version for the inventory result item/
--
-- /Note:/ Consider using 'schemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriSchemaVersion :: Lens.Lens' InventoryResultItem Lude.Text
iriSchemaVersion = Lens.lens (schemaVersion :: InventoryResultItem -> Lude.Text) (\s a -> s {schemaVersion = a} :: InventoryResultItem)
{-# DEPRECATED iriSchemaVersion "Use generic-lens or generic-optics with 'schemaVersion' instead." #-}

-- | Contains all the inventory data of the item type. Results include attribute names and values.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriContent :: Lens.Lens' InventoryResultItem [Lude.HashMap Lude.Text (Lude.Text)]
iriContent = Lens.lens (content :: InventoryResultItem -> [Lude.HashMap Lude.Text (Lude.Text)]) (\s a -> s {content = a} :: InventoryResultItem)
{-# DEPRECATED iriContent "Use generic-lens or generic-optics with 'content' instead." #-}

instance Lude.FromJSON InventoryResultItem where
  parseJSON =
    Lude.withObject
      "InventoryResultItem"
      ( \x ->
          InventoryResultItem'
            Lude.<$> (x Lude..:? "ContentHash")
            Lude.<*> (x Lude..:? "CaptureTime")
            Lude.<*> (x Lude..: "TypeName")
            Lude.<*> (x Lude..: "SchemaVersion")
            Lude.<*> (x Lude..:? "Content" Lude..!= Lude.mempty)
      )
