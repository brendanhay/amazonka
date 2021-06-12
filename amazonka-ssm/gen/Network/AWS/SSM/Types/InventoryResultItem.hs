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
-- Module      : Network.AWS.SSM.Types.InventoryResultItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryResultItem where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The inventory result item.
--
-- /See:/ 'newInventoryResultItem' smart constructor.
data InventoryResultItem = InventoryResultItem'
  { -- | The time inventory item data was captured.
    captureTime :: Core.Maybe Core.Text,
    -- | MD5 hash of the inventory item type contents. The content hash is used
    -- to determine whether to update inventory information. The PutInventory
    -- API does not update the inventory item type contents if the MD5 hash has
    -- not changed since last update.
    contentHash :: Core.Maybe Core.Text,
    -- | The name of the inventory result item type.
    typeName :: Core.Text,
    -- | The schema version for the inventory result item\/
    schemaVersion :: Core.Text,
    -- | Contains all the inventory data of the item type. Results include
    -- attribute names and values.
    content :: [Core.HashMap Core.Text Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InventoryResultItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'captureTime', 'inventoryResultItem_captureTime' - The time inventory item data was captured.
--
-- 'contentHash', 'inventoryResultItem_contentHash' - MD5 hash of the inventory item type contents. The content hash is used
-- to determine whether to update inventory information. The PutInventory
-- API does not update the inventory item type contents if the MD5 hash has
-- not changed since last update.
--
-- 'typeName', 'inventoryResultItem_typeName' - The name of the inventory result item type.
--
-- 'schemaVersion', 'inventoryResultItem_schemaVersion' - The schema version for the inventory result item\/
--
-- 'content', 'inventoryResultItem_content' - Contains all the inventory data of the item type. Results include
-- attribute names and values.
newInventoryResultItem ::
  -- | 'typeName'
  Core.Text ->
  -- | 'schemaVersion'
  Core.Text ->
  InventoryResultItem
newInventoryResultItem pTypeName_ pSchemaVersion_ =
  InventoryResultItem'
    { captureTime = Core.Nothing,
      contentHash = Core.Nothing,
      typeName = pTypeName_,
      schemaVersion = pSchemaVersion_,
      content = Core.mempty
    }

-- | The time inventory item data was captured.
inventoryResultItem_captureTime :: Lens.Lens' InventoryResultItem (Core.Maybe Core.Text)
inventoryResultItem_captureTime = Lens.lens (\InventoryResultItem' {captureTime} -> captureTime) (\s@InventoryResultItem' {} a -> s {captureTime = a} :: InventoryResultItem)

-- | MD5 hash of the inventory item type contents. The content hash is used
-- to determine whether to update inventory information. The PutInventory
-- API does not update the inventory item type contents if the MD5 hash has
-- not changed since last update.
inventoryResultItem_contentHash :: Lens.Lens' InventoryResultItem (Core.Maybe Core.Text)
inventoryResultItem_contentHash = Lens.lens (\InventoryResultItem' {contentHash} -> contentHash) (\s@InventoryResultItem' {} a -> s {contentHash = a} :: InventoryResultItem)

-- | The name of the inventory result item type.
inventoryResultItem_typeName :: Lens.Lens' InventoryResultItem Core.Text
inventoryResultItem_typeName = Lens.lens (\InventoryResultItem' {typeName} -> typeName) (\s@InventoryResultItem' {} a -> s {typeName = a} :: InventoryResultItem)

-- | The schema version for the inventory result item\/
inventoryResultItem_schemaVersion :: Lens.Lens' InventoryResultItem Core.Text
inventoryResultItem_schemaVersion = Lens.lens (\InventoryResultItem' {schemaVersion} -> schemaVersion) (\s@InventoryResultItem' {} a -> s {schemaVersion = a} :: InventoryResultItem)

-- | Contains all the inventory data of the item type. Results include
-- attribute names and values.
inventoryResultItem_content :: Lens.Lens' InventoryResultItem [Core.HashMap Core.Text Core.Text]
inventoryResultItem_content = Lens.lens (\InventoryResultItem' {content} -> content) (\s@InventoryResultItem' {} a -> s {content = a} :: InventoryResultItem) Core.. Lens._Coerce

instance Core.FromJSON InventoryResultItem where
  parseJSON =
    Core.withObject
      "InventoryResultItem"
      ( \x ->
          InventoryResultItem'
            Core.<$> (x Core..:? "CaptureTime")
            Core.<*> (x Core..:? "ContentHash")
            Core.<*> (x Core..: "TypeName")
            Core.<*> (x Core..: "SchemaVersion")
            Core.<*> (x Core..:? "Content" Core..!= Core.mempty)
      )

instance Core.Hashable InventoryResultItem

instance Core.NFData InventoryResultItem
