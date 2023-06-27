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
-- Module      : Amazonka.MediaStoreData.Types.Item
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaStoreData.Types.Item where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaStoreData.Types.ItemType
import qualified Amazonka.Prelude as Prelude

-- | A metadata entry for a folder or object.
--
-- /See:/ 'newItem' smart constructor.
data Item = Item'
  { -- | The length of the item in bytes.
    contentLength :: Prelude.Maybe Prelude.Natural,
    -- | The content type of the item.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The ETag that represents a unique instance of the item.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the item was last modified.
    lastModified :: Prelude.Maybe Data.POSIX,
    -- | The name of the item.
    name :: Prelude.Maybe Prelude.Text,
    -- | The item type (folder or object).
    type' :: Prelude.Maybe ItemType
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
-- 'contentLength', 'item_contentLength' - The length of the item in bytes.
--
-- 'contentType', 'item_contentType' - The content type of the item.
--
-- 'eTag', 'item_eTag' - The ETag that represents a unique instance of the item.
--
-- 'lastModified', 'item_lastModified' - The date and time that the item was last modified.
--
-- 'name', 'item_name' - The name of the item.
--
-- 'type'', 'item_type' - The item type (folder or object).
newItem ::
  Item
newItem =
  Item'
    { contentLength = Prelude.Nothing,
      contentType = Prelude.Nothing,
      eTag = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The length of the item in bytes.
item_contentLength :: Lens.Lens' Item (Prelude.Maybe Prelude.Natural)
item_contentLength = Lens.lens (\Item' {contentLength} -> contentLength) (\s@Item' {} a -> s {contentLength = a} :: Item)

-- | The content type of the item.
item_contentType :: Lens.Lens' Item (Prelude.Maybe Prelude.Text)
item_contentType = Lens.lens (\Item' {contentType} -> contentType) (\s@Item' {} a -> s {contentType = a} :: Item)

-- | The ETag that represents a unique instance of the item.
item_eTag :: Lens.Lens' Item (Prelude.Maybe Prelude.Text)
item_eTag = Lens.lens (\Item' {eTag} -> eTag) (\s@Item' {} a -> s {eTag = a} :: Item)

-- | The date and time that the item was last modified.
item_lastModified :: Lens.Lens' Item (Prelude.Maybe Prelude.UTCTime)
item_lastModified = Lens.lens (\Item' {lastModified} -> lastModified) (\s@Item' {} a -> s {lastModified = a} :: Item) Prelude.. Lens.mapping Data._Time

-- | The name of the item.
item_name :: Lens.Lens' Item (Prelude.Maybe Prelude.Text)
item_name = Lens.lens (\Item' {name} -> name) (\s@Item' {} a -> s {name = a} :: Item)

-- | The item type (folder or object).
item_type :: Lens.Lens' Item (Prelude.Maybe ItemType)
item_type = Lens.lens (\Item' {type'} -> type') (\s@Item' {} a -> s {type' = a} :: Item)

instance Data.FromJSON Item where
  parseJSON =
    Data.withObject
      "Item"
      ( \x ->
          Item'
            Prelude.<$> (x Data..:? "ContentLength")
            Prelude.<*> (x Data..:? "ContentType")
            Prelude.<*> (x Data..:? "ETag")
            Prelude.<*> (x Data..:? "LastModified")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Item where
  hashWithSalt _salt Item' {..} =
    _salt
      `Prelude.hashWithSalt` contentLength
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` eTag
      `Prelude.hashWithSalt` lastModified
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Item where
  rnf Item' {..} =
    Prelude.rnf contentLength
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf lastModified
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
