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
-- Module      : Network.AWS.SSM.Types.InventoryDeletionStatusItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryDeletionStatusItem where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.InventoryDeletionStatus
import Network.AWS.SSM.Types.InventoryDeletionSummary

-- | Status information returned by the @DeleteInventory@ action.
--
-- /See:/ 'newInventoryDeletionStatusItem' smart constructor.
data InventoryDeletionStatusItem = InventoryDeletionStatusItem'
  { -- | The name of the inventory data type.
    typeName :: Core.Maybe Core.Text,
    -- | Information about the status.
    lastStatusMessage :: Core.Maybe Core.Text,
    -- | The UTC timestamp of when the last status report.
    lastStatusUpdateTime :: Core.Maybe Core.POSIX,
    -- | The deletion ID returned by the @DeleteInventory@ action.
    deletionId :: Core.Maybe Core.Text,
    -- | The UTC timestamp when the delete operation started.
    deletionStartTime :: Core.Maybe Core.POSIX,
    -- | The status of the operation. Possible values are InProgress and
    -- Complete.
    lastStatus :: Core.Maybe InventoryDeletionStatus,
    -- | Information about the delete operation. For more information about this
    -- summary, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete Understanding the delete inventory summary>
    -- in the /AWS Systems Manager User Guide/.
    deletionSummary :: Core.Maybe InventoryDeletionSummary
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InventoryDeletionStatusItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeName', 'inventoryDeletionStatusItem_typeName' - The name of the inventory data type.
--
-- 'lastStatusMessage', 'inventoryDeletionStatusItem_lastStatusMessage' - Information about the status.
--
-- 'lastStatusUpdateTime', 'inventoryDeletionStatusItem_lastStatusUpdateTime' - The UTC timestamp of when the last status report.
--
-- 'deletionId', 'inventoryDeletionStatusItem_deletionId' - The deletion ID returned by the @DeleteInventory@ action.
--
-- 'deletionStartTime', 'inventoryDeletionStatusItem_deletionStartTime' - The UTC timestamp when the delete operation started.
--
-- 'lastStatus', 'inventoryDeletionStatusItem_lastStatus' - The status of the operation. Possible values are InProgress and
-- Complete.
--
-- 'deletionSummary', 'inventoryDeletionStatusItem_deletionSummary' - Information about the delete operation. For more information about this
-- summary, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete Understanding the delete inventory summary>
-- in the /AWS Systems Manager User Guide/.
newInventoryDeletionStatusItem ::
  InventoryDeletionStatusItem
newInventoryDeletionStatusItem =
  InventoryDeletionStatusItem'
    { typeName =
        Core.Nothing,
      lastStatusMessage = Core.Nothing,
      lastStatusUpdateTime = Core.Nothing,
      deletionId = Core.Nothing,
      deletionStartTime = Core.Nothing,
      lastStatus = Core.Nothing,
      deletionSummary = Core.Nothing
    }

-- | The name of the inventory data type.
inventoryDeletionStatusItem_typeName :: Lens.Lens' InventoryDeletionStatusItem (Core.Maybe Core.Text)
inventoryDeletionStatusItem_typeName = Lens.lens (\InventoryDeletionStatusItem' {typeName} -> typeName) (\s@InventoryDeletionStatusItem' {} a -> s {typeName = a} :: InventoryDeletionStatusItem)

-- | Information about the status.
inventoryDeletionStatusItem_lastStatusMessage :: Lens.Lens' InventoryDeletionStatusItem (Core.Maybe Core.Text)
inventoryDeletionStatusItem_lastStatusMessage = Lens.lens (\InventoryDeletionStatusItem' {lastStatusMessage} -> lastStatusMessage) (\s@InventoryDeletionStatusItem' {} a -> s {lastStatusMessage = a} :: InventoryDeletionStatusItem)

-- | The UTC timestamp of when the last status report.
inventoryDeletionStatusItem_lastStatusUpdateTime :: Lens.Lens' InventoryDeletionStatusItem (Core.Maybe Core.UTCTime)
inventoryDeletionStatusItem_lastStatusUpdateTime = Lens.lens (\InventoryDeletionStatusItem' {lastStatusUpdateTime} -> lastStatusUpdateTime) (\s@InventoryDeletionStatusItem' {} a -> s {lastStatusUpdateTime = a} :: InventoryDeletionStatusItem) Core.. Lens.mapping Core._Time

-- | The deletion ID returned by the @DeleteInventory@ action.
inventoryDeletionStatusItem_deletionId :: Lens.Lens' InventoryDeletionStatusItem (Core.Maybe Core.Text)
inventoryDeletionStatusItem_deletionId = Lens.lens (\InventoryDeletionStatusItem' {deletionId} -> deletionId) (\s@InventoryDeletionStatusItem' {} a -> s {deletionId = a} :: InventoryDeletionStatusItem)

-- | The UTC timestamp when the delete operation started.
inventoryDeletionStatusItem_deletionStartTime :: Lens.Lens' InventoryDeletionStatusItem (Core.Maybe Core.UTCTime)
inventoryDeletionStatusItem_deletionStartTime = Lens.lens (\InventoryDeletionStatusItem' {deletionStartTime} -> deletionStartTime) (\s@InventoryDeletionStatusItem' {} a -> s {deletionStartTime = a} :: InventoryDeletionStatusItem) Core.. Lens.mapping Core._Time

-- | The status of the operation. Possible values are InProgress and
-- Complete.
inventoryDeletionStatusItem_lastStatus :: Lens.Lens' InventoryDeletionStatusItem (Core.Maybe InventoryDeletionStatus)
inventoryDeletionStatusItem_lastStatus = Lens.lens (\InventoryDeletionStatusItem' {lastStatus} -> lastStatus) (\s@InventoryDeletionStatusItem' {} a -> s {lastStatus = a} :: InventoryDeletionStatusItem)

-- | Information about the delete operation. For more information about this
-- summary, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete Understanding the delete inventory summary>
-- in the /AWS Systems Manager User Guide/.
inventoryDeletionStatusItem_deletionSummary :: Lens.Lens' InventoryDeletionStatusItem (Core.Maybe InventoryDeletionSummary)
inventoryDeletionStatusItem_deletionSummary = Lens.lens (\InventoryDeletionStatusItem' {deletionSummary} -> deletionSummary) (\s@InventoryDeletionStatusItem' {} a -> s {deletionSummary = a} :: InventoryDeletionStatusItem)

instance Core.FromJSON InventoryDeletionStatusItem where
  parseJSON =
    Core.withObject
      "InventoryDeletionStatusItem"
      ( \x ->
          InventoryDeletionStatusItem'
            Core.<$> (x Core..:? "TypeName")
            Core.<*> (x Core..:? "LastStatusMessage")
            Core.<*> (x Core..:? "LastStatusUpdateTime")
            Core.<*> (x Core..:? "DeletionId")
            Core.<*> (x Core..:? "DeletionStartTime")
            Core.<*> (x Core..:? "LastStatus")
            Core.<*> (x Core..:? "DeletionSummary")
      )

instance Core.Hashable InventoryDeletionStatusItem

instance Core.NFData InventoryDeletionStatusItem
