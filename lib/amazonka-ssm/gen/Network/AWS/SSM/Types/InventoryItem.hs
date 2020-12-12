{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryItem
  ( InventoryItem (..),

    -- * Smart constructor
    mkInventoryItem,

    -- * Lenses
    iiContext,
    iiContentHash,
    iiContent,
    iiTypeName,
    iiSchemaVersion,
    iiCaptureTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information collected from managed instances based on your inventory policy document
--
-- /See:/ 'mkInventoryItem' smart constructor.
data InventoryItem = InventoryItem'
  { context ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    contentHash :: Lude.Maybe Lude.Text,
    content :: Lude.Maybe [Lude.HashMap Lude.Text (Lude.Text)],
    typeName :: Lude.Text,
    schemaVersion :: Lude.Text,
    captureTime :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InventoryItem' with the minimum fields required to make a request.
--
-- * 'captureTime' - The time the inventory information was collected.
-- * 'content' - The inventory data of the inventory type.
-- * 'contentHash' - MD5 hash of the inventory item type contents. The content hash is used to determine whether to update inventory information. The PutInventory API does not update the inventory item type contents if the MD5 hash has not changed since last update.
-- * 'context' - A map of associated properties for a specified inventory type. For example, with this attribute, you can specify the @ExecutionId@ , @ExecutionType@ , @ComplianceType@ properties of the @AWS:ComplianceItem@ type.
-- * 'schemaVersion' - The schema version for the inventory item.
-- * 'typeName' - The name of the inventory type. Default inventory item type names start with AWS. Custom inventory type names will start with Custom. Default inventory item types include the following: AWS:AWSComponent, AWS:Application, AWS:InstanceInformation, AWS:Network, and AWS:WindowsUpdate.
mkInventoryItem ::
  -- | 'typeName'
  Lude.Text ->
  -- | 'schemaVersion'
  Lude.Text ->
  -- | 'captureTime'
  Lude.Text ->
  InventoryItem
mkInventoryItem pTypeName_ pSchemaVersion_ pCaptureTime_ =
  InventoryItem'
    { context = Lude.Nothing,
      contentHash = Lude.Nothing,
      content = Lude.Nothing,
      typeName = pTypeName_,
      schemaVersion = pSchemaVersion_,
      captureTime = pCaptureTime_
    }

-- | A map of associated properties for a specified inventory type. For example, with this attribute, you can specify the @ExecutionId@ , @ExecutionType@ , @ComplianceType@ properties of the @AWS:ComplianceItem@ type.
--
-- /Note:/ Consider using 'context' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiContext :: Lens.Lens' InventoryItem (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
iiContext = Lens.lens (context :: InventoryItem -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {context = a} :: InventoryItem)
{-# DEPRECATED iiContext "Use generic-lens or generic-optics with 'context' instead." #-}

-- | MD5 hash of the inventory item type contents. The content hash is used to determine whether to update inventory information. The PutInventory API does not update the inventory item type contents if the MD5 hash has not changed since last update.
--
-- /Note:/ Consider using 'contentHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiContentHash :: Lens.Lens' InventoryItem (Lude.Maybe Lude.Text)
iiContentHash = Lens.lens (contentHash :: InventoryItem -> Lude.Maybe Lude.Text) (\s a -> s {contentHash = a} :: InventoryItem)
{-# DEPRECATED iiContentHash "Use generic-lens or generic-optics with 'contentHash' instead." #-}

-- | The inventory data of the inventory type.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiContent :: Lens.Lens' InventoryItem (Lude.Maybe [Lude.HashMap Lude.Text (Lude.Text)])
iiContent = Lens.lens (content :: InventoryItem -> Lude.Maybe [Lude.HashMap Lude.Text (Lude.Text)]) (\s a -> s {content = a} :: InventoryItem)
{-# DEPRECATED iiContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The name of the inventory type. Default inventory item type names start with AWS. Custom inventory type names will start with Custom. Default inventory item types include the following: AWS:AWSComponent, AWS:Application, AWS:InstanceInformation, AWS:Network, and AWS:WindowsUpdate.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiTypeName :: Lens.Lens' InventoryItem Lude.Text
iiTypeName = Lens.lens (typeName :: InventoryItem -> Lude.Text) (\s a -> s {typeName = a} :: InventoryItem)
{-# DEPRECATED iiTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The schema version for the inventory item.
--
-- /Note:/ Consider using 'schemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiSchemaVersion :: Lens.Lens' InventoryItem Lude.Text
iiSchemaVersion = Lens.lens (schemaVersion :: InventoryItem -> Lude.Text) (\s a -> s {schemaVersion = a} :: InventoryItem)
{-# DEPRECATED iiSchemaVersion "Use generic-lens or generic-optics with 'schemaVersion' instead." #-}

-- | The time the inventory information was collected.
--
-- /Note:/ Consider using 'captureTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiCaptureTime :: Lens.Lens' InventoryItem Lude.Text
iiCaptureTime = Lens.lens (captureTime :: InventoryItem -> Lude.Text) (\s a -> s {captureTime = a} :: InventoryItem)
{-# DEPRECATED iiCaptureTime "Use generic-lens or generic-optics with 'captureTime' instead." #-}

instance Lude.ToJSON InventoryItem where
  toJSON InventoryItem' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Context" Lude..=) Lude.<$> context,
            ("ContentHash" Lude..=) Lude.<$> contentHash,
            ("Content" Lude..=) Lude.<$> content,
            Lude.Just ("TypeName" Lude..= typeName),
            Lude.Just ("SchemaVersion" Lude..= schemaVersion),
            Lude.Just ("CaptureTime" Lude..= captureTime)
          ]
      )
