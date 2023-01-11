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
-- Module      : Amazonka.EC2.Types.SnapshotTierStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SnapshotTierStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.SnapshotState
import Amazonka.EC2.Types.StorageTier
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.TieringOperationStatus
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a snapshot\'s storage tier.
--
-- /See:/ 'newSnapshotTierStatus' smart constructor.
data SnapshotTierStatus = SnapshotTierStatus'
  { -- | The date and time when the last archive process was completed.
    archivalCompleteTime :: Prelude.Maybe Data.ISO8601,
    -- | The status of the last archive or restore process.
    lastTieringOperationStatus :: Prelude.Maybe TieringOperationStatus,
    -- | A message describing the status of the last archive or restore process.
    lastTieringOperationStatusDetail :: Prelude.Maybe Prelude.Text,
    -- | The progress of the last archive or restore process, as a percentage.
    lastTieringProgress :: Prelude.Maybe Prelude.Int,
    -- | The date and time when the last archive or restore process was started.
    lastTieringStartTime :: Prelude.Maybe Data.ISO8601,
    -- | The ID of the Amazon Web Services account that owns the snapshot.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | Only for archived snapshots that are temporarily restored. Indicates the
    -- date and time when a temporarily restored snapshot will be automatically
    -- re-archived.
    restoreExpiryTime :: Prelude.Maybe Data.ISO8601,
    -- | The ID of the snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The state of the snapshot.
    status :: Prelude.Maybe SnapshotState,
    -- | The storage tier in which the snapshot is stored. @standard@ indicates
    -- that the snapshot is stored in the standard snapshot storage tier and
    -- that it is ready for use. @archive@ indicates that the snapshot is
    -- currently archived and that it must be restored before it can be used.
    storageTier :: Prelude.Maybe StorageTier,
    -- | The tags that are assigned to the snapshot.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the volume from which the snapshot was created.
    volumeId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnapshotTierStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'archivalCompleteTime', 'snapshotTierStatus_archivalCompleteTime' - The date and time when the last archive process was completed.
--
-- 'lastTieringOperationStatus', 'snapshotTierStatus_lastTieringOperationStatus' - The status of the last archive or restore process.
--
-- 'lastTieringOperationStatusDetail', 'snapshotTierStatus_lastTieringOperationStatusDetail' - A message describing the status of the last archive or restore process.
--
-- 'lastTieringProgress', 'snapshotTierStatus_lastTieringProgress' - The progress of the last archive or restore process, as a percentage.
--
-- 'lastTieringStartTime', 'snapshotTierStatus_lastTieringStartTime' - The date and time when the last archive or restore process was started.
--
-- 'ownerId', 'snapshotTierStatus_ownerId' - The ID of the Amazon Web Services account that owns the snapshot.
--
-- 'restoreExpiryTime', 'snapshotTierStatus_restoreExpiryTime' - Only for archived snapshots that are temporarily restored. Indicates the
-- date and time when a temporarily restored snapshot will be automatically
-- re-archived.
--
-- 'snapshotId', 'snapshotTierStatus_snapshotId' - The ID of the snapshot.
--
-- 'status', 'snapshotTierStatus_status' - The state of the snapshot.
--
-- 'storageTier', 'snapshotTierStatus_storageTier' - The storage tier in which the snapshot is stored. @standard@ indicates
-- that the snapshot is stored in the standard snapshot storage tier and
-- that it is ready for use. @archive@ indicates that the snapshot is
-- currently archived and that it must be restored before it can be used.
--
-- 'tags', 'snapshotTierStatus_tags' - The tags that are assigned to the snapshot.
--
-- 'volumeId', 'snapshotTierStatus_volumeId' - The ID of the volume from which the snapshot was created.
newSnapshotTierStatus ::
  SnapshotTierStatus
newSnapshotTierStatus =
  SnapshotTierStatus'
    { archivalCompleteTime =
        Prelude.Nothing,
      lastTieringOperationStatus = Prelude.Nothing,
      lastTieringOperationStatusDetail = Prelude.Nothing,
      lastTieringProgress = Prelude.Nothing,
      lastTieringStartTime = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      restoreExpiryTime = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      status = Prelude.Nothing,
      storageTier = Prelude.Nothing,
      tags = Prelude.Nothing,
      volumeId = Prelude.Nothing
    }

-- | The date and time when the last archive process was completed.
snapshotTierStatus_archivalCompleteTime :: Lens.Lens' SnapshotTierStatus (Prelude.Maybe Prelude.UTCTime)
snapshotTierStatus_archivalCompleteTime = Lens.lens (\SnapshotTierStatus' {archivalCompleteTime} -> archivalCompleteTime) (\s@SnapshotTierStatus' {} a -> s {archivalCompleteTime = a} :: SnapshotTierStatus) Prelude.. Lens.mapping Data._Time

-- | The status of the last archive or restore process.
snapshotTierStatus_lastTieringOperationStatus :: Lens.Lens' SnapshotTierStatus (Prelude.Maybe TieringOperationStatus)
snapshotTierStatus_lastTieringOperationStatus = Lens.lens (\SnapshotTierStatus' {lastTieringOperationStatus} -> lastTieringOperationStatus) (\s@SnapshotTierStatus' {} a -> s {lastTieringOperationStatus = a} :: SnapshotTierStatus)

-- | A message describing the status of the last archive or restore process.
snapshotTierStatus_lastTieringOperationStatusDetail :: Lens.Lens' SnapshotTierStatus (Prelude.Maybe Prelude.Text)
snapshotTierStatus_lastTieringOperationStatusDetail = Lens.lens (\SnapshotTierStatus' {lastTieringOperationStatusDetail} -> lastTieringOperationStatusDetail) (\s@SnapshotTierStatus' {} a -> s {lastTieringOperationStatusDetail = a} :: SnapshotTierStatus)

-- | The progress of the last archive or restore process, as a percentage.
snapshotTierStatus_lastTieringProgress :: Lens.Lens' SnapshotTierStatus (Prelude.Maybe Prelude.Int)
snapshotTierStatus_lastTieringProgress = Lens.lens (\SnapshotTierStatus' {lastTieringProgress} -> lastTieringProgress) (\s@SnapshotTierStatus' {} a -> s {lastTieringProgress = a} :: SnapshotTierStatus)

-- | The date and time when the last archive or restore process was started.
snapshotTierStatus_lastTieringStartTime :: Lens.Lens' SnapshotTierStatus (Prelude.Maybe Prelude.UTCTime)
snapshotTierStatus_lastTieringStartTime = Lens.lens (\SnapshotTierStatus' {lastTieringStartTime} -> lastTieringStartTime) (\s@SnapshotTierStatus' {} a -> s {lastTieringStartTime = a} :: SnapshotTierStatus) Prelude.. Lens.mapping Data._Time

-- | The ID of the Amazon Web Services account that owns the snapshot.
snapshotTierStatus_ownerId :: Lens.Lens' SnapshotTierStatus (Prelude.Maybe Prelude.Text)
snapshotTierStatus_ownerId = Lens.lens (\SnapshotTierStatus' {ownerId} -> ownerId) (\s@SnapshotTierStatus' {} a -> s {ownerId = a} :: SnapshotTierStatus)

-- | Only for archived snapshots that are temporarily restored. Indicates the
-- date and time when a temporarily restored snapshot will be automatically
-- re-archived.
snapshotTierStatus_restoreExpiryTime :: Lens.Lens' SnapshotTierStatus (Prelude.Maybe Prelude.UTCTime)
snapshotTierStatus_restoreExpiryTime = Lens.lens (\SnapshotTierStatus' {restoreExpiryTime} -> restoreExpiryTime) (\s@SnapshotTierStatus' {} a -> s {restoreExpiryTime = a} :: SnapshotTierStatus) Prelude.. Lens.mapping Data._Time

-- | The ID of the snapshot.
snapshotTierStatus_snapshotId :: Lens.Lens' SnapshotTierStatus (Prelude.Maybe Prelude.Text)
snapshotTierStatus_snapshotId = Lens.lens (\SnapshotTierStatus' {snapshotId} -> snapshotId) (\s@SnapshotTierStatus' {} a -> s {snapshotId = a} :: SnapshotTierStatus)

-- | The state of the snapshot.
snapshotTierStatus_status :: Lens.Lens' SnapshotTierStatus (Prelude.Maybe SnapshotState)
snapshotTierStatus_status = Lens.lens (\SnapshotTierStatus' {status} -> status) (\s@SnapshotTierStatus' {} a -> s {status = a} :: SnapshotTierStatus)

-- | The storage tier in which the snapshot is stored. @standard@ indicates
-- that the snapshot is stored in the standard snapshot storage tier and
-- that it is ready for use. @archive@ indicates that the snapshot is
-- currently archived and that it must be restored before it can be used.
snapshotTierStatus_storageTier :: Lens.Lens' SnapshotTierStatus (Prelude.Maybe StorageTier)
snapshotTierStatus_storageTier = Lens.lens (\SnapshotTierStatus' {storageTier} -> storageTier) (\s@SnapshotTierStatus' {} a -> s {storageTier = a} :: SnapshotTierStatus)

-- | The tags that are assigned to the snapshot.
snapshotTierStatus_tags :: Lens.Lens' SnapshotTierStatus (Prelude.Maybe [Tag])
snapshotTierStatus_tags = Lens.lens (\SnapshotTierStatus' {tags} -> tags) (\s@SnapshotTierStatus' {} a -> s {tags = a} :: SnapshotTierStatus) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the volume from which the snapshot was created.
snapshotTierStatus_volumeId :: Lens.Lens' SnapshotTierStatus (Prelude.Maybe Prelude.Text)
snapshotTierStatus_volumeId = Lens.lens (\SnapshotTierStatus' {volumeId} -> volumeId) (\s@SnapshotTierStatus' {} a -> s {volumeId = a} :: SnapshotTierStatus)

instance Data.FromXML SnapshotTierStatus where
  parseXML x =
    SnapshotTierStatus'
      Prelude.<$> (x Data..@? "archivalCompleteTime")
      Prelude.<*> (x Data..@? "lastTieringOperationStatus")
      Prelude.<*> (x Data..@? "lastTieringOperationStatusDetail")
      Prelude.<*> (x Data..@? "lastTieringProgress")
      Prelude.<*> (x Data..@? "lastTieringStartTime")
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "restoreExpiryTime")
      Prelude.<*> (x Data..@? "snapshotId")
      Prelude.<*> (x Data..@? "status")
      Prelude.<*> (x Data..@? "storageTier")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "volumeId")

instance Prelude.Hashable SnapshotTierStatus where
  hashWithSalt _salt SnapshotTierStatus' {..} =
    _salt `Prelude.hashWithSalt` archivalCompleteTime
      `Prelude.hashWithSalt` lastTieringOperationStatus
      `Prelude.hashWithSalt` lastTieringOperationStatusDetail
      `Prelude.hashWithSalt` lastTieringProgress
      `Prelude.hashWithSalt` lastTieringStartTime
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` restoreExpiryTime
      `Prelude.hashWithSalt` snapshotId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` storageTier
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` volumeId

instance Prelude.NFData SnapshotTierStatus where
  rnf SnapshotTierStatus' {..} =
    Prelude.rnf archivalCompleteTime
      `Prelude.seq` Prelude.rnf lastTieringOperationStatus
      `Prelude.seq` Prelude.rnf lastTieringOperationStatusDetail
      `Prelude.seq` Prelude.rnf lastTieringProgress
      `Prelude.seq` Prelude.rnf lastTieringStartTime
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf restoreExpiryTime
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf storageTier
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf volumeId
