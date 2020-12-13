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
    iETag,
    iContentLength,
    iName,
    iType,
    iLastModified,
    iContentType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStoreData.Types.ItemType
import qualified Network.AWS.Prelude as Lude

-- | A metadata entry for a folder or object.
--
-- /See:/ 'mkItem' smart constructor.
data Item = Item'
  { -- | The ETag that represents a unique instance of the item.
    eTag :: Lude.Maybe Lude.Text,
    -- | The length of the item in bytes.
    contentLength :: Lude.Maybe Lude.Natural,
    -- | The name of the item.
    name :: Lude.Maybe Lude.Text,
    -- | The item type (folder or object).
    type' :: Lude.Maybe ItemType,
    -- | The date and time that the item was last modified.
    lastModified :: Lude.Maybe Lude.Timestamp,
    -- | The content type of the item.
    contentType :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Item' with the minimum fields required to make a request.
--
-- * 'eTag' - The ETag that represents a unique instance of the item.
-- * 'contentLength' - The length of the item in bytes.
-- * 'name' - The name of the item.
-- * 'type'' - The item type (folder or object).
-- * 'lastModified' - The date and time that the item was last modified.
-- * 'contentType' - The content type of the item.
mkItem ::
  Item
mkItem =
  Item'
    { eTag = Lude.Nothing,
      contentLength = Lude.Nothing,
      name = Lude.Nothing,
      type' = Lude.Nothing,
      lastModified = Lude.Nothing,
      contentType = Lude.Nothing
    }

-- | The ETag that represents a unique instance of the item.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iETag :: Lens.Lens' Item (Lude.Maybe Lude.Text)
iETag = Lens.lens (eTag :: Item -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: Item)
{-# DEPRECATED iETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The length of the item in bytes.
--
-- /Note:/ Consider using 'contentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iContentLength :: Lens.Lens' Item (Lude.Maybe Lude.Natural)
iContentLength = Lens.lens (contentLength :: Item -> Lude.Maybe Lude.Natural) (\s a -> s {contentLength = a} :: Item)
{-# DEPRECATED iContentLength "Use generic-lens or generic-optics with 'contentLength' instead." #-}

-- | The name of the item.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iName :: Lens.Lens' Item (Lude.Maybe Lude.Text)
iName = Lens.lens (name :: Item -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Item)
{-# DEPRECATED iName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The item type (folder or object).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iType :: Lens.Lens' Item (Lude.Maybe ItemType)
iType = Lens.lens (type' :: Item -> Lude.Maybe ItemType) (\s a -> s {type' = a} :: Item)
{-# DEPRECATED iType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The date and time that the item was last modified.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLastModified :: Lens.Lens' Item (Lude.Maybe Lude.Timestamp)
iLastModified = Lens.lens (lastModified :: Item -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModified = a} :: Item)
{-# DEPRECATED iLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | The content type of the item.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iContentType :: Lens.Lens' Item (Lude.Maybe Lude.Text)
iContentType = Lens.lens (contentType :: Item -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: Item)
{-# DEPRECATED iContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

instance Lude.FromJSON Item where
  parseJSON =
    Lude.withObject
      "Item"
      ( \x ->
          Item'
            Lude.<$> (x Lude..:? "ETag")
            Lude.<*> (x Lude..:? "ContentLength")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "LastModified")
            Lude.<*> (x Lude..:? "ContentType")
      )
