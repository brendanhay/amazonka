{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStoreData.Types.Item
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStoreData.Types.Item
  ( Item (..),

    -- * Smart constructor
    mkItem,

    -- * Lenses
    iContentLength,
    iContentType,
    iETag,
    iLastModified,
    iName,
    iType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStoreData.Types.ContentType as Types
import qualified Network.AWS.MediaStoreData.Types.ETag as Types
import qualified Network.AWS.MediaStoreData.Types.ItemName as Types
import qualified Network.AWS.MediaStoreData.Types.ItemType as Types
import qualified Network.AWS.Prelude as Core

-- | A metadata entry for a folder or object.
--
-- /See:/ 'mkItem' smart constructor.
data Item = Item'
  { -- | The length of the item in bytes.
    contentLength :: Core.Maybe Core.Natural,
    -- | The content type of the item.
    contentType :: Core.Maybe Types.ContentType,
    -- | The ETag that represents a unique instance of the item.
    eTag :: Core.Maybe Types.ETag,
    -- | The date and time that the item was last modified.
    lastModified :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the item.
    name :: Core.Maybe Types.ItemName,
    -- | The item type (folder or object).
    type' :: Core.Maybe Types.ItemType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Item' value with any optional fields omitted.
mkItem ::
  Item
mkItem =
  Item'
    { contentLength = Core.Nothing,
      contentType = Core.Nothing,
      eTag = Core.Nothing,
      lastModified = Core.Nothing,
      name = Core.Nothing,
      type' = Core.Nothing
    }

-- | The length of the item in bytes.
--
-- /Note:/ Consider using 'contentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iContentLength :: Lens.Lens' Item (Core.Maybe Core.Natural)
iContentLength = Lens.field @"contentLength"
{-# DEPRECATED iContentLength "Use generic-lens or generic-optics with 'contentLength' instead." #-}

-- | The content type of the item.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iContentType :: Lens.Lens' Item (Core.Maybe Types.ContentType)
iContentType = Lens.field @"contentType"
{-# DEPRECATED iContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The ETag that represents a unique instance of the item.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iETag :: Lens.Lens' Item (Core.Maybe Types.ETag)
iETag = Lens.field @"eTag"
{-# DEPRECATED iETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The date and time that the item was last modified.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLastModified :: Lens.Lens' Item (Core.Maybe Core.NominalDiffTime)
iLastModified = Lens.field @"lastModified"
{-# DEPRECATED iLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | The name of the item.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iName :: Lens.Lens' Item (Core.Maybe Types.ItemName)
iName = Lens.field @"name"
{-# DEPRECATED iName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The item type (folder or object).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iType :: Lens.Lens' Item (Core.Maybe Types.ItemType)
iType = Lens.field @"type'"
{-# DEPRECATED iType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON Item where
  parseJSON =
    Core.withObject "Item" Core.$
      \x ->
        Item'
          Core.<$> (x Core..:? "ContentLength")
          Core.<*> (x Core..:? "ContentType")
          Core.<*> (x Core..:? "ETag")
          Core.<*> (x Core..:? "LastModified")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Type")
