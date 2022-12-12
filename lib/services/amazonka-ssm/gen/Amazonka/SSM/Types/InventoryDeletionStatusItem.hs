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
-- Module      : Amazonka.SSM.Types.InventoryDeletionStatusItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InventoryDeletionStatusItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.InventoryDeletionStatus
import Amazonka.SSM.Types.InventoryDeletionSummary

-- | Status information returned by the @DeleteInventory@ operation.
--
-- /See:/ 'newInventoryDeletionStatusItem' smart constructor.
data InventoryDeletionStatusItem = InventoryDeletionStatusItem'
  { -- | The deletion ID returned by the @DeleteInventory@ operation.
    deletionId :: Prelude.Maybe Prelude.Text,
    -- | The UTC timestamp when the delete operation started.
    deletionStartTime :: Prelude.Maybe Data.POSIX,
    -- | Information about the delete operation. For more information about this
    -- summary, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete Understanding the delete inventory summary>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    deletionSummary :: Prelude.Maybe InventoryDeletionSummary,
    -- | The status of the operation. Possible values are InProgress and
    -- Complete.
    lastStatus :: Prelude.Maybe InventoryDeletionStatus,
    -- | Information about the status.
    lastStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The UTC timestamp of when the last status report.
    lastStatusUpdateTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the inventory data type.
    typeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InventoryDeletionStatusItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionId', 'inventoryDeletionStatusItem_deletionId' - The deletion ID returned by the @DeleteInventory@ operation.
--
-- 'deletionStartTime', 'inventoryDeletionStatusItem_deletionStartTime' - The UTC timestamp when the delete operation started.
--
-- 'deletionSummary', 'inventoryDeletionStatusItem_deletionSummary' - Information about the delete operation. For more information about this
-- summary, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete Understanding the delete inventory summary>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'lastStatus', 'inventoryDeletionStatusItem_lastStatus' - The status of the operation. Possible values are InProgress and
-- Complete.
--
-- 'lastStatusMessage', 'inventoryDeletionStatusItem_lastStatusMessage' - Information about the status.
--
-- 'lastStatusUpdateTime', 'inventoryDeletionStatusItem_lastStatusUpdateTime' - The UTC timestamp of when the last status report.
--
-- 'typeName', 'inventoryDeletionStatusItem_typeName' - The name of the inventory data type.
newInventoryDeletionStatusItem ::
  InventoryDeletionStatusItem
newInventoryDeletionStatusItem =
  InventoryDeletionStatusItem'
    { deletionId =
        Prelude.Nothing,
      deletionStartTime = Prelude.Nothing,
      deletionSummary = Prelude.Nothing,
      lastStatus = Prelude.Nothing,
      lastStatusMessage = Prelude.Nothing,
      lastStatusUpdateTime = Prelude.Nothing,
      typeName = Prelude.Nothing
    }

-- | The deletion ID returned by the @DeleteInventory@ operation.
inventoryDeletionStatusItem_deletionId :: Lens.Lens' InventoryDeletionStatusItem (Prelude.Maybe Prelude.Text)
inventoryDeletionStatusItem_deletionId = Lens.lens (\InventoryDeletionStatusItem' {deletionId} -> deletionId) (\s@InventoryDeletionStatusItem' {} a -> s {deletionId = a} :: InventoryDeletionStatusItem)

-- | The UTC timestamp when the delete operation started.
inventoryDeletionStatusItem_deletionStartTime :: Lens.Lens' InventoryDeletionStatusItem (Prelude.Maybe Prelude.UTCTime)
inventoryDeletionStatusItem_deletionStartTime = Lens.lens (\InventoryDeletionStatusItem' {deletionStartTime} -> deletionStartTime) (\s@InventoryDeletionStatusItem' {} a -> s {deletionStartTime = a} :: InventoryDeletionStatusItem) Prelude.. Lens.mapping Data._Time

-- | Information about the delete operation. For more information about this
-- summary, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-custom.html#sysman-inventory-delete Understanding the delete inventory summary>
-- in the /Amazon Web Services Systems Manager User Guide/.
inventoryDeletionStatusItem_deletionSummary :: Lens.Lens' InventoryDeletionStatusItem (Prelude.Maybe InventoryDeletionSummary)
inventoryDeletionStatusItem_deletionSummary = Lens.lens (\InventoryDeletionStatusItem' {deletionSummary} -> deletionSummary) (\s@InventoryDeletionStatusItem' {} a -> s {deletionSummary = a} :: InventoryDeletionStatusItem)

-- | The status of the operation. Possible values are InProgress and
-- Complete.
inventoryDeletionStatusItem_lastStatus :: Lens.Lens' InventoryDeletionStatusItem (Prelude.Maybe InventoryDeletionStatus)
inventoryDeletionStatusItem_lastStatus = Lens.lens (\InventoryDeletionStatusItem' {lastStatus} -> lastStatus) (\s@InventoryDeletionStatusItem' {} a -> s {lastStatus = a} :: InventoryDeletionStatusItem)

-- | Information about the status.
inventoryDeletionStatusItem_lastStatusMessage :: Lens.Lens' InventoryDeletionStatusItem (Prelude.Maybe Prelude.Text)
inventoryDeletionStatusItem_lastStatusMessage = Lens.lens (\InventoryDeletionStatusItem' {lastStatusMessage} -> lastStatusMessage) (\s@InventoryDeletionStatusItem' {} a -> s {lastStatusMessage = a} :: InventoryDeletionStatusItem)

-- | The UTC timestamp of when the last status report.
inventoryDeletionStatusItem_lastStatusUpdateTime :: Lens.Lens' InventoryDeletionStatusItem (Prelude.Maybe Prelude.UTCTime)
inventoryDeletionStatusItem_lastStatusUpdateTime = Lens.lens (\InventoryDeletionStatusItem' {lastStatusUpdateTime} -> lastStatusUpdateTime) (\s@InventoryDeletionStatusItem' {} a -> s {lastStatusUpdateTime = a} :: InventoryDeletionStatusItem) Prelude.. Lens.mapping Data._Time

-- | The name of the inventory data type.
inventoryDeletionStatusItem_typeName :: Lens.Lens' InventoryDeletionStatusItem (Prelude.Maybe Prelude.Text)
inventoryDeletionStatusItem_typeName = Lens.lens (\InventoryDeletionStatusItem' {typeName} -> typeName) (\s@InventoryDeletionStatusItem' {} a -> s {typeName = a} :: InventoryDeletionStatusItem)

instance Data.FromJSON InventoryDeletionStatusItem where
  parseJSON =
    Data.withObject
      "InventoryDeletionStatusItem"
      ( \x ->
          InventoryDeletionStatusItem'
            Prelude.<$> (x Data..:? "DeletionId")
            Prelude.<*> (x Data..:? "DeletionStartTime")
            Prelude.<*> (x Data..:? "DeletionSummary")
            Prelude.<*> (x Data..:? "LastStatus")
            Prelude.<*> (x Data..:? "LastStatusMessage")
            Prelude.<*> (x Data..:? "LastStatusUpdateTime")
            Prelude.<*> (x Data..:? "TypeName")
      )

instance Prelude.Hashable InventoryDeletionStatusItem where
  hashWithSalt _salt InventoryDeletionStatusItem' {..} =
    _salt `Prelude.hashWithSalt` deletionId
      `Prelude.hashWithSalt` deletionStartTime
      `Prelude.hashWithSalt` deletionSummary
      `Prelude.hashWithSalt` lastStatus
      `Prelude.hashWithSalt` lastStatusMessage
      `Prelude.hashWithSalt` lastStatusUpdateTime
      `Prelude.hashWithSalt` typeName

instance Prelude.NFData InventoryDeletionStatusItem where
  rnf InventoryDeletionStatusItem' {..} =
    Prelude.rnf deletionId
      `Prelude.seq` Prelude.rnf deletionStartTime
      `Prelude.seq` Prelude.rnf deletionSummary
      `Prelude.seq` Prelude.rnf lastStatus
      `Prelude.seq` Prelude.rnf lastStatusMessage
      `Prelude.seq` Prelude.rnf lastStatusUpdateTime
      `Prelude.seq` Prelude.rnf typeName
