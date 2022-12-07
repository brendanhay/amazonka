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
-- Module      : Amazonka.SSM.Types.InventoryResultItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InventoryResultItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The inventory result item.
--
-- /See:/ 'newInventoryResultItem' smart constructor.
data InventoryResultItem = InventoryResultItem'
  { -- | MD5 hash of the inventory item type contents. The content hash is used
    -- to determine whether to update inventory information. The PutInventory
    -- API doesn\'t update the inventory item type contents if the MD5 hash
    -- hasn\'t changed since last update.
    contentHash :: Prelude.Maybe Prelude.Text,
    -- | The time inventory item data was captured.
    captureTime :: Prelude.Maybe Prelude.Text,
    -- | The name of the inventory result item type.
    typeName :: Prelude.Text,
    -- | The schema version for the inventory result item\/
    schemaVersion :: Prelude.Text,
    -- | Contains all the inventory data of the item type. Results include
    -- attribute names and values.
    content :: [Prelude.HashMap Prelude.Text Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InventoryResultItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentHash', 'inventoryResultItem_contentHash' - MD5 hash of the inventory item type contents. The content hash is used
-- to determine whether to update inventory information. The PutInventory
-- API doesn\'t update the inventory item type contents if the MD5 hash
-- hasn\'t changed since last update.
--
-- 'captureTime', 'inventoryResultItem_captureTime' - The time inventory item data was captured.
--
-- 'typeName', 'inventoryResultItem_typeName' - The name of the inventory result item type.
--
-- 'schemaVersion', 'inventoryResultItem_schemaVersion' - The schema version for the inventory result item\/
--
-- 'content', 'inventoryResultItem_content' - Contains all the inventory data of the item type. Results include
-- attribute names and values.
newInventoryResultItem ::
  -- | 'typeName'
  Prelude.Text ->
  -- | 'schemaVersion'
  Prelude.Text ->
  InventoryResultItem
newInventoryResultItem pTypeName_ pSchemaVersion_ =
  InventoryResultItem'
    { contentHash = Prelude.Nothing,
      captureTime = Prelude.Nothing,
      typeName = pTypeName_,
      schemaVersion = pSchemaVersion_,
      content = Prelude.mempty
    }

-- | MD5 hash of the inventory item type contents. The content hash is used
-- to determine whether to update inventory information. The PutInventory
-- API doesn\'t update the inventory item type contents if the MD5 hash
-- hasn\'t changed since last update.
inventoryResultItem_contentHash :: Lens.Lens' InventoryResultItem (Prelude.Maybe Prelude.Text)
inventoryResultItem_contentHash = Lens.lens (\InventoryResultItem' {contentHash} -> contentHash) (\s@InventoryResultItem' {} a -> s {contentHash = a} :: InventoryResultItem)

-- | The time inventory item data was captured.
inventoryResultItem_captureTime :: Lens.Lens' InventoryResultItem (Prelude.Maybe Prelude.Text)
inventoryResultItem_captureTime = Lens.lens (\InventoryResultItem' {captureTime} -> captureTime) (\s@InventoryResultItem' {} a -> s {captureTime = a} :: InventoryResultItem)

-- | The name of the inventory result item type.
inventoryResultItem_typeName :: Lens.Lens' InventoryResultItem Prelude.Text
inventoryResultItem_typeName = Lens.lens (\InventoryResultItem' {typeName} -> typeName) (\s@InventoryResultItem' {} a -> s {typeName = a} :: InventoryResultItem)

-- | The schema version for the inventory result item\/
inventoryResultItem_schemaVersion :: Lens.Lens' InventoryResultItem Prelude.Text
inventoryResultItem_schemaVersion = Lens.lens (\InventoryResultItem' {schemaVersion} -> schemaVersion) (\s@InventoryResultItem' {} a -> s {schemaVersion = a} :: InventoryResultItem)

-- | Contains all the inventory data of the item type. Results include
-- attribute names and values.
inventoryResultItem_content :: Lens.Lens' InventoryResultItem [Prelude.HashMap Prelude.Text Prelude.Text]
inventoryResultItem_content = Lens.lens (\InventoryResultItem' {content} -> content) (\s@InventoryResultItem' {} a -> s {content = a} :: InventoryResultItem) Prelude.. Lens.coerced

instance Data.FromJSON InventoryResultItem where
  parseJSON =
    Data.withObject
      "InventoryResultItem"
      ( \x ->
          InventoryResultItem'
            Prelude.<$> (x Data..:? "ContentHash")
            Prelude.<*> (x Data..:? "CaptureTime")
            Prelude.<*> (x Data..: "TypeName")
            Prelude.<*> (x Data..: "SchemaVersion")
            Prelude.<*> (x Data..:? "Content" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable InventoryResultItem where
  hashWithSalt _salt InventoryResultItem' {..} =
    _salt `Prelude.hashWithSalt` contentHash
      `Prelude.hashWithSalt` captureTime
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` schemaVersion
      `Prelude.hashWithSalt` content

instance Prelude.NFData InventoryResultItem where
  rnf InventoryResultItem' {..} =
    Prelude.rnf contentHash
      `Prelude.seq` Prelude.rnf captureTime
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf schemaVersion
      `Prelude.seq` Prelude.rnf content
