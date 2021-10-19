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
import qualified Network.AWS.Prelude as Prelude

-- | A metadata entry for a folder or object.
--
-- /See:/ 'newItem' smart constructor.
data Item = Item'
  { -- | The ETag that represents a unique instance of the item.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The length of the item in bytes.
    contentLength :: Prelude.Maybe Prelude.Natural,
    -- | The name of the item.
    name :: Prelude.Maybe Prelude.Text,
    -- | The item type (folder or object).
    type' :: Prelude.Maybe ItemType,
    -- | The date and time that the item was last modified.
    lastModified :: Prelude.Maybe Core.POSIX,
    -- | The content type of the item.
    contentType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'contentLength', 'item_contentLength' - The length of the item in bytes.
--
-- 'name', 'item_name' - The name of the item.
--
-- 'type'', 'item_type' - The item type (folder or object).
--
-- 'lastModified', 'item_lastModified' - The date and time that the item was last modified.
--
-- 'contentType', 'item_contentType' - The content type of the item.
newItem ::
  Item
newItem =
  Item'
    { eTag = Prelude.Nothing,
      contentLength = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      contentType = Prelude.Nothing
    }

-- | The ETag that represents a unique instance of the item.
item_eTag :: Lens.Lens' Item (Prelude.Maybe Prelude.Text)
item_eTag = Lens.lens (\Item' {eTag} -> eTag) (\s@Item' {} a -> s {eTag = a} :: Item)

-- | The length of the item in bytes.
item_contentLength :: Lens.Lens' Item (Prelude.Maybe Prelude.Natural)
item_contentLength = Lens.lens (\Item' {contentLength} -> contentLength) (\s@Item' {} a -> s {contentLength = a} :: Item)

-- | The name of the item.
item_name :: Lens.Lens' Item (Prelude.Maybe Prelude.Text)
item_name = Lens.lens (\Item' {name} -> name) (\s@Item' {} a -> s {name = a} :: Item)

-- | The item type (folder or object).
item_type :: Lens.Lens' Item (Prelude.Maybe ItemType)
item_type = Lens.lens (\Item' {type'} -> type') (\s@Item' {} a -> s {type' = a} :: Item)

-- | The date and time that the item was last modified.
item_lastModified :: Lens.Lens' Item (Prelude.Maybe Prelude.UTCTime)
item_lastModified = Lens.lens (\Item' {lastModified} -> lastModified) (\s@Item' {} a -> s {lastModified = a} :: Item) Prelude.. Lens.mapping Core._Time

-- | The content type of the item.
item_contentType :: Lens.Lens' Item (Prelude.Maybe Prelude.Text)
item_contentType = Lens.lens (\Item' {contentType} -> contentType) (\s@Item' {} a -> s {contentType = a} :: Item)

instance Core.FromJSON Item where
  parseJSON =
    Core.withObject
      "Item"
      ( \x ->
          Item'
            Prelude.<$> (x Core..:? "ETag")
            Prelude.<*> (x Core..:? "ContentLength")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "LastModified")
            Prelude.<*> (x Core..:? "ContentType")
      )

instance Prelude.Hashable Item

instance Prelude.NFData Item
