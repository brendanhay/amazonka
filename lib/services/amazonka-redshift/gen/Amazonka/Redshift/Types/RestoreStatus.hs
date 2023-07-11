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
-- Module      : Amazonka.Redshift.Types.RestoreStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.RestoreStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- | Describes the status of a cluster restore action. Returns null if the
-- cluster was not created by restoring a snapshot.
--
-- /See:/ 'newRestoreStatus' smart constructor.
data RestoreStatus = RestoreStatus'
  { -- | The number of megabytes per second being transferred from the backup
    -- storage. Returns the average rate for a completed backup. This field is
    -- only updated when you restore to DC2 and DS2 node types.
    currentRestoreRateInMegaBytesPerSecond :: Prelude.Maybe Prelude.Double,
    -- | The amount of time an in-progress restore has been running, or the
    -- amount of time it took a completed restore to finish. This field is only
    -- updated when you restore to DC2 and DS2 node types.
    elapsedTimeInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The estimate of the time remaining before the restore will complete.
    -- Returns 0 for a completed restore. This field is only updated when you
    -- restore to DC2 and DS2 node types.
    estimatedTimeToCompletionInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The number of megabytes that have been transferred from snapshot
    -- storage. This field is only updated when you restore to DC2 and DS2 node
    -- types.
    progressInMegaBytes :: Prelude.Maybe Prelude.Integer,
    -- | The size of the set of snapshot data used to restore the cluster. This
    -- field is only updated when you restore to DC2 and DS2 node types.
    snapshotSizeInMegaBytes :: Prelude.Maybe Prelude.Integer,
    -- | The status of the restore action. Returns starting, restoring,
    -- completed, or failed.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentRestoreRateInMegaBytesPerSecond', 'restoreStatus_currentRestoreRateInMegaBytesPerSecond' - The number of megabytes per second being transferred from the backup
-- storage. Returns the average rate for a completed backup. This field is
-- only updated when you restore to DC2 and DS2 node types.
--
-- 'elapsedTimeInSeconds', 'restoreStatus_elapsedTimeInSeconds' - The amount of time an in-progress restore has been running, or the
-- amount of time it took a completed restore to finish. This field is only
-- updated when you restore to DC2 and DS2 node types.
--
-- 'estimatedTimeToCompletionInSeconds', 'restoreStatus_estimatedTimeToCompletionInSeconds' - The estimate of the time remaining before the restore will complete.
-- Returns 0 for a completed restore. This field is only updated when you
-- restore to DC2 and DS2 node types.
--
-- 'progressInMegaBytes', 'restoreStatus_progressInMegaBytes' - The number of megabytes that have been transferred from snapshot
-- storage. This field is only updated when you restore to DC2 and DS2 node
-- types.
--
-- 'snapshotSizeInMegaBytes', 'restoreStatus_snapshotSizeInMegaBytes' - The size of the set of snapshot data used to restore the cluster. This
-- field is only updated when you restore to DC2 and DS2 node types.
--
-- 'status', 'restoreStatus_status' - The status of the restore action. Returns starting, restoring,
-- completed, or failed.
newRestoreStatus ::
  RestoreStatus
newRestoreStatus =
  RestoreStatus'
    { currentRestoreRateInMegaBytesPerSecond =
        Prelude.Nothing,
      elapsedTimeInSeconds = Prelude.Nothing,
      estimatedTimeToCompletionInSeconds = Prelude.Nothing,
      progressInMegaBytes = Prelude.Nothing,
      snapshotSizeInMegaBytes = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The number of megabytes per second being transferred from the backup
-- storage. Returns the average rate for a completed backup. This field is
-- only updated when you restore to DC2 and DS2 node types.
restoreStatus_currentRestoreRateInMegaBytesPerSecond :: Lens.Lens' RestoreStatus (Prelude.Maybe Prelude.Double)
restoreStatus_currentRestoreRateInMegaBytesPerSecond = Lens.lens (\RestoreStatus' {currentRestoreRateInMegaBytesPerSecond} -> currentRestoreRateInMegaBytesPerSecond) (\s@RestoreStatus' {} a -> s {currentRestoreRateInMegaBytesPerSecond = a} :: RestoreStatus)

-- | The amount of time an in-progress restore has been running, or the
-- amount of time it took a completed restore to finish. This field is only
-- updated when you restore to DC2 and DS2 node types.
restoreStatus_elapsedTimeInSeconds :: Lens.Lens' RestoreStatus (Prelude.Maybe Prelude.Integer)
restoreStatus_elapsedTimeInSeconds = Lens.lens (\RestoreStatus' {elapsedTimeInSeconds} -> elapsedTimeInSeconds) (\s@RestoreStatus' {} a -> s {elapsedTimeInSeconds = a} :: RestoreStatus)

-- | The estimate of the time remaining before the restore will complete.
-- Returns 0 for a completed restore. This field is only updated when you
-- restore to DC2 and DS2 node types.
restoreStatus_estimatedTimeToCompletionInSeconds :: Lens.Lens' RestoreStatus (Prelude.Maybe Prelude.Integer)
restoreStatus_estimatedTimeToCompletionInSeconds = Lens.lens (\RestoreStatus' {estimatedTimeToCompletionInSeconds} -> estimatedTimeToCompletionInSeconds) (\s@RestoreStatus' {} a -> s {estimatedTimeToCompletionInSeconds = a} :: RestoreStatus)

-- | The number of megabytes that have been transferred from snapshot
-- storage. This field is only updated when you restore to DC2 and DS2 node
-- types.
restoreStatus_progressInMegaBytes :: Lens.Lens' RestoreStatus (Prelude.Maybe Prelude.Integer)
restoreStatus_progressInMegaBytes = Lens.lens (\RestoreStatus' {progressInMegaBytes} -> progressInMegaBytes) (\s@RestoreStatus' {} a -> s {progressInMegaBytes = a} :: RestoreStatus)

-- | The size of the set of snapshot data used to restore the cluster. This
-- field is only updated when you restore to DC2 and DS2 node types.
restoreStatus_snapshotSizeInMegaBytes :: Lens.Lens' RestoreStatus (Prelude.Maybe Prelude.Integer)
restoreStatus_snapshotSizeInMegaBytes = Lens.lens (\RestoreStatus' {snapshotSizeInMegaBytes} -> snapshotSizeInMegaBytes) (\s@RestoreStatus' {} a -> s {snapshotSizeInMegaBytes = a} :: RestoreStatus)

-- | The status of the restore action. Returns starting, restoring,
-- completed, or failed.
restoreStatus_status :: Lens.Lens' RestoreStatus (Prelude.Maybe Prelude.Text)
restoreStatus_status = Lens.lens (\RestoreStatus' {status} -> status) (\s@RestoreStatus' {} a -> s {status = a} :: RestoreStatus)

instance Data.FromXML RestoreStatus where
  parseXML x =
    RestoreStatus'
      Prelude.<$> (x Data..@? "CurrentRestoreRateInMegaBytesPerSecond")
      Prelude.<*> (x Data..@? "ElapsedTimeInSeconds")
      Prelude.<*> (x Data..@? "EstimatedTimeToCompletionInSeconds")
      Prelude.<*> (x Data..@? "ProgressInMegaBytes")
      Prelude.<*> (x Data..@? "SnapshotSizeInMegaBytes")
      Prelude.<*> (x Data..@? "Status")

instance Prelude.Hashable RestoreStatus where
  hashWithSalt _salt RestoreStatus' {..} =
    _salt
      `Prelude.hashWithSalt` currentRestoreRateInMegaBytesPerSecond
      `Prelude.hashWithSalt` elapsedTimeInSeconds
      `Prelude.hashWithSalt` estimatedTimeToCompletionInSeconds
      `Prelude.hashWithSalt` progressInMegaBytes
      `Prelude.hashWithSalt` snapshotSizeInMegaBytes
      `Prelude.hashWithSalt` status

instance Prelude.NFData RestoreStatus where
  rnf RestoreStatus' {..} =
    Prelude.rnf currentRestoreRateInMegaBytesPerSecond
      `Prelude.seq` Prelude.rnf elapsedTimeInSeconds
      `Prelude.seq` Prelude.rnf estimatedTimeToCompletionInSeconds
      `Prelude.seq` Prelude.rnf progressInMegaBytes
      `Prelude.seq` Prelude.rnf snapshotSizeInMegaBytes
      `Prelude.seq` Prelude.rnf status
