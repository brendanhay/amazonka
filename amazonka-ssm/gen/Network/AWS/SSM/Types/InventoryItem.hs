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
-- Module      : Network.AWS.SSM.Types.InventoryItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryItem where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information collected from managed instances based on your inventory
-- policy document
--
-- /See:/ 'newInventoryItem' smart constructor.
data InventoryItem = InventoryItem'
  { -- | A map of associated properties for a specified inventory type. For
    -- example, with this attribute, you can specify the @ExecutionId@,
    -- @ExecutionType@, @ComplianceType@ properties of the @AWS:ComplianceItem@
    -- type.
    context :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The inventory data of the inventory type.
    content :: Prelude.Maybe [Prelude.HashMap Prelude.Text Prelude.Text],
    -- | MD5 hash of the inventory item type contents. The content hash is used
    -- to determine whether to update inventory information. The PutInventory
    -- API does not update the inventory item type contents if the MD5 hash has
    -- not changed since last update.
    contentHash :: Prelude.Maybe Prelude.Text,
    -- | The name of the inventory type. Default inventory item type names start
    -- with AWS. Custom inventory type names will start with Custom. Default
    -- inventory item types include the following: AWS:AWSComponent,
    -- AWS:Application, AWS:InstanceInformation, AWS:Network, and
    -- AWS:WindowsUpdate.
    typeName :: Prelude.Text,
    -- | The schema version for the inventory item.
    schemaVersion :: Prelude.Text,
    -- | The time the inventory information was collected.
    captureTime :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InventoryItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'context', 'inventoryItem_context' - A map of associated properties for a specified inventory type. For
-- example, with this attribute, you can specify the @ExecutionId@,
-- @ExecutionType@, @ComplianceType@ properties of the @AWS:ComplianceItem@
-- type.
--
-- 'content', 'inventoryItem_content' - The inventory data of the inventory type.
--
-- 'contentHash', 'inventoryItem_contentHash' - MD5 hash of the inventory item type contents. The content hash is used
-- to determine whether to update inventory information. The PutInventory
-- API does not update the inventory item type contents if the MD5 hash has
-- not changed since last update.
--
-- 'typeName', 'inventoryItem_typeName' - The name of the inventory type. Default inventory item type names start
-- with AWS. Custom inventory type names will start with Custom. Default
-- inventory item types include the following: AWS:AWSComponent,
-- AWS:Application, AWS:InstanceInformation, AWS:Network, and
-- AWS:WindowsUpdate.
--
-- 'schemaVersion', 'inventoryItem_schemaVersion' - The schema version for the inventory item.
--
-- 'captureTime', 'inventoryItem_captureTime' - The time the inventory information was collected.
newInventoryItem ::
  -- | 'typeName'
  Prelude.Text ->
  -- | 'schemaVersion'
  Prelude.Text ->
  -- | 'captureTime'
  Prelude.Text ->
  InventoryItem
newInventoryItem
  pTypeName_
  pSchemaVersion_
  pCaptureTime_ =
    InventoryItem'
      { context = Prelude.Nothing,
        content = Prelude.Nothing,
        contentHash = Prelude.Nothing,
        typeName = pTypeName_,
        schemaVersion = pSchemaVersion_,
        captureTime = pCaptureTime_
      }

-- | A map of associated properties for a specified inventory type. For
-- example, with this attribute, you can specify the @ExecutionId@,
-- @ExecutionType@, @ComplianceType@ properties of the @AWS:ComplianceItem@
-- type.
inventoryItem_context :: Lens.Lens' InventoryItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
inventoryItem_context = Lens.lens (\InventoryItem' {context} -> context) (\s@InventoryItem' {} a -> s {context = a} :: InventoryItem) Prelude.. Lens.mapping Lens._Coerce

-- | The inventory data of the inventory type.
inventoryItem_content :: Lens.Lens' InventoryItem (Prelude.Maybe [Prelude.HashMap Prelude.Text Prelude.Text])
inventoryItem_content = Lens.lens (\InventoryItem' {content} -> content) (\s@InventoryItem' {} a -> s {content = a} :: InventoryItem) Prelude.. Lens.mapping Lens._Coerce

-- | MD5 hash of the inventory item type contents. The content hash is used
-- to determine whether to update inventory information. The PutInventory
-- API does not update the inventory item type contents if the MD5 hash has
-- not changed since last update.
inventoryItem_contentHash :: Lens.Lens' InventoryItem (Prelude.Maybe Prelude.Text)
inventoryItem_contentHash = Lens.lens (\InventoryItem' {contentHash} -> contentHash) (\s@InventoryItem' {} a -> s {contentHash = a} :: InventoryItem)

-- | The name of the inventory type. Default inventory item type names start
-- with AWS. Custom inventory type names will start with Custom. Default
-- inventory item types include the following: AWS:AWSComponent,
-- AWS:Application, AWS:InstanceInformation, AWS:Network, and
-- AWS:WindowsUpdate.
inventoryItem_typeName :: Lens.Lens' InventoryItem Prelude.Text
inventoryItem_typeName = Lens.lens (\InventoryItem' {typeName} -> typeName) (\s@InventoryItem' {} a -> s {typeName = a} :: InventoryItem)

-- | The schema version for the inventory item.
inventoryItem_schemaVersion :: Lens.Lens' InventoryItem Prelude.Text
inventoryItem_schemaVersion = Lens.lens (\InventoryItem' {schemaVersion} -> schemaVersion) (\s@InventoryItem' {} a -> s {schemaVersion = a} :: InventoryItem)

-- | The time the inventory information was collected.
inventoryItem_captureTime :: Lens.Lens' InventoryItem Prelude.Text
inventoryItem_captureTime = Lens.lens (\InventoryItem' {captureTime} -> captureTime) (\s@InventoryItem' {} a -> s {captureTime = a} :: InventoryItem)

instance Prelude.Hashable InventoryItem

instance Prelude.NFData InventoryItem

instance Core.ToJSON InventoryItem where
  toJSON InventoryItem' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Context" Core..=) Prelude.<$> context,
            ("Content" Core..=) Prelude.<$> content,
            ("ContentHash" Core..=) Prelude.<$> contentHash,
            Prelude.Just ("TypeName" Core..= typeName),
            Prelude.Just ("SchemaVersion" Core..= schemaVersion),
            Prelude.Just ("CaptureTime" Core..= captureTime)
          ]
      )
