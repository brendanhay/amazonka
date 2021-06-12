{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStoreData.Types.Item
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStoreData.Types.Item where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStoreData.Types.ItemType

-- | A metadata entry for a folder or object.
--
-- /See:/ 'newItem' smart constructor.
data Item = Item'
  { -- | The ETag that represents a unique instance of the item.
    eTag :: Core.Maybe Core.Text,
    -- | The content type of the item.
    contentType :: Core.Maybe Core.Text,
    -- | The length of the item in bytes.
    contentLength :: Core.Maybe Core.Natural,
    -- | The name of the item.
    name :: Core.Maybe Core.Text,
    -- | The date and time that the item was last modified.
    lastModified :: Core.Maybe Core.POSIX,
    -- | The item type (folder or object).
    type' :: Core.Maybe ItemType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Item' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'item_eTag' - The ETag that represents a unique instance of the item.
--
-- 'contentType', 'item_contentType' - The content type of the item.
--
-- 'contentLength', 'item_contentLength' - The length of the item in bytes.
--
-- 'name', 'item_name' - The name of the item.
--
-- 'lastModified', 'item_lastModified' - The date and time that the item was last modified.
--
-- 'type'', 'item_type' - The item type (folder or object).
newItem ::
  Item
newItem =
  Item'
    { eTag = Core.Nothing,
      contentType = Core.Nothing,
      contentLength = Core.Nothing,
      name = Core.Nothing,
      lastModified = Core.Nothing,
      type' = Core.Nothing
    }

-- | The ETag that represents a unique instance of the item.
item_eTag :: Lens.Lens' Item (Core.Maybe Core.Text)
item_eTag = Lens.lens (\Item' {eTag} -> eTag) (\s@Item' {} a -> s {eTag = a} :: Item)

-- | The content type of the item.
item_contentType :: Lens.Lens' Item (Core.Maybe Core.Text)
item_contentType = Lens.lens (\Item' {contentType} -> contentType) (\s@Item' {} a -> s {contentType = a} :: Item)

-- | The length of the item in bytes.
item_contentLength :: Lens.Lens' Item (Core.Maybe Core.Natural)
item_contentLength = Lens.lens (\Item' {contentLength} -> contentLength) (\s@Item' {} a -> s {contentLength = a} :: Item)

-- | The name of the item.
item_name :: Lens.Lens' Item (Core.Maybe Core.Text)
item_name = Lens.lens (\Item' {name} -> name) (\s@Item' {} a -> s {name = a} :: Item)

-- | The date and time that the item was last modified.
item_lastModified :: Lens.Lens' Item (Core.Maybe Core.UTCTime)
item_lastModified = Lens.lens (\Item' {lastModified} -> lastModified) (\s@Item' {} a -> s {lastModified = a} :: Item) Core.. Lens.mapping Core._Time

-- | The item type (folder or object).
item_type :: Lens.Lens' Item (Core.Maybe ItemType)
item_type = Lens.lens (\Item' {type'} -> type') (\s@Item' {} a -> s {type' = a} :: Item)

instance Core.FromJSON Item where
  parseJSON =
    Core.withObject
      "Item"
      ( \x ->
          Item'
            Core.<$> (x Core..:? "ETag")
            Core.<*> (x Core..:? "ContentType")
            Core.<*> (x Core..:? "ContentLength")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "LastModified")
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable Item

instance Core.NFData Item
