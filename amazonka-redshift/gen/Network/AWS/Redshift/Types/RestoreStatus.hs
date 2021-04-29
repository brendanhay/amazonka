{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Redshift.Types.RestoreStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.RestoreStatus where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

-- | Describes the status of a cluster restore action. Returns null if the
-- cluster was not created by restoring a snapshot.
--
-- /See:/ 'newRestoreStatus' smart constructor.
data RestoreStatus = RestoreStatus'
  { -- | The status of the restore action. Returns starting, restoring,
    -- completed, or failed.
    status :: Prelude.Maybe Prelude.Text,
    -- | The estimate of the time remaining before the restore will complete.
    -- Returns 0 for a completed restore. This field is only updated when you
    -- restore to DC2 and DS2 node types.
    estimatedTimeToCompletionInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The size of the set of snapshot data used to restore the cluster. This
    -- field is only updated when you restore to DC2 and DS2 node types.
    snapshotSizeInMegaBytes :: Prelude.Maybe Prelude.Integer,
    -- | The number of megabytes per second being transferred from the backup
    -- storage. Returns the average rate for a completed backup. This field is
    -- only updated when you restore to DC2 and DS2 node types.
    currentRestoreRateInMegaBytesPerSecond :: Prelude.Maybe Prelude.Double,
    -- | The amount of time an in-progress restore has been running, or the
    -- amount of time it took a completed restore to finish. This field is only
    -- updated when you restore to DC2 and DS2 node types.
    elapsedTimeInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The number of megabytes that have been transferred from snapshot
    -- storage. This field is only updated when you restore to DC2 and DS2 node
    -- types.
    progressInMegaBytes :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RestoreStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'restoreStatus_status' - The status of the restore action. Returns starting, restoring,
-- completed, or failed.
--
-- 'estimatedTimeToCompletionInSeconds', 'restoreStatus_estimatedTimeToCompletionInSeconds' - The estimate of the time remaining before the restore will complete.
-- Returns 0 for a completed restore. This field is only updated when you
-- restore to DC2 and DS2 node types.
--
-- 'snapshotSizeInMegaBytes', 'restoreStatus_snapshotSizeInMegaBytes' - The size of the set of snapshot data used to restore the cluster. This
-- field is only updated when you restore to DC2 and DS2 node types.
--
-- 'currentRestoreRateInMegaBytesPerSecond', 'restoreStatus_currentRestoreRateInMegaBytesPerSecond' - The number of megabytes per second being transferred from the backup
-- storage. Returns the average rate for a completed backup. This field is
-- only updated when you restore to DC2 and DS2 node types.
--
-- 'elapsedTimeInSeconds', 'restoreStatus_elapsedTimeInSeconds' - The amount of time an in-progress restore has been running, or the
-- amount of time it took a completed restore to finish. This field is only
-- updated when you restore to DC2 and DS2 node types.
--
-- 'progressInMegaBytes', 'restoreStatus_progressInMegaBytes' - The number of megabytes that have been transferred from snapshot
-- storage. This field is only updated when you restore to DC2 and DS2 node
-- types.
newRestoreStatus ::
  RestoreStatus
newRestoreStatus =
  RestoreStatus'
    { status = Prelude.Nothing,
      estimatedTimeToCompletionInSeconds = Prelude.Nothing,
      snapshotSizeInMegaBytes = Prelude.Nothing,
      currentRestoreRateInMegaBytesPerSecond =
        Prelude.Nothing,
      elapsedTimeInSeconds = Prelude.Nothing,
      progressInMegaBytes = Prelude.Nothing
    }

-- | The status of the restore action. Returns starting, restoring,
-- completed, or failed.
restoreStatus_status :: Lens.Lens' RestoreStatus (Prelude.Maybe Prelude.Text)
restoreStatus_status = Lens.lens (\RestoreStatus' {status} -> status) (\s@RestoreStatus' {} a -> s {status = a} :: RestoreStatus)

-- | The estimate of the time remaining before the restore will complete.
-- Returns 0 for a completed restore. This field is only updated when you
-- restore to DC2 and DS2 node types.
restoreStatus_estimatedTimeToCompletionInSeconds :: Lens.Lens' RestoreStatus (Prelude.Maybe Prelude.Integer)
restoreStatus_estimatedTimeToCompletionInSeconds = Lens.lens (\RestoreStatus' {estimatedTimeToCompletionInSeconds} -> estimatedTimeToCompletionInSeconds) (\s@RestoreStatus' {} a -> s {estimatedTimeToCompletionInSeconds = a} :: RestoreStatus)

-- | The size of the set of snapshot data used to restore the cluster. This
-- field is only updated when you restore to DC2 and DS2 node types.
restoreStatus_snapshotSizeInMegaBytes :: Lens.Lens' RestoreStatus (Prelude.Maybe Prelude.Integer)
restoreStatus_snapshotSizeInMegaBytes = Lens.lens (\RestoreStatus' {snapshotSizeInMegaBytes} -> snapshotSizeInMegaBytes) (\s@RestoreStatus' {} a -> s {snapshotSizeInMegaBytes = a} :: RestoreStatus)

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

-- | The number of megabytes that have been transferred from snapshot
-- storage. This field is only updated when you restore to DC2 and DS2 node
-- types.
restoreStatus_progressInMegaBytes :: Lens.Lens' RestoreStatus (Prelude.Maybe Prelude.Integer)
restoreStatus_progressInMegaBytes = Lens.lens (\RestoreStatus' {progressInMegaBytes} -> progressInMegaBytes) (\s@RestoreStatus' {} a -> s {progressInMegaBytes = a} :: RestoreStatus)

instance Prelude.FromXML RestoreStatus where
  parseXML x =
    RestoreStatus'
      Prelude.<$> (x Prelude..@? "Status")
      Prelude.<*> (x Prelude..@? "EstimatedTimeToCompletionInSeconds")
      Prelude.<*> (x Prelude..@? "SnapshotSizeInMegaBytes")
      Prelude.<*> ( x
                      Prelude..@? "CurrentRestoreRateInMegaBytesPerSecond"
                  )
      Prelude.<*> (x Prelude..@? "ElapsedTimeInSeconds")
      Prelude.<*> (x Prelude..@? "ProgressInMegaBytes")

instance Prelude.Hashable RestoreStatus

instance Prelude.NFData RestoreStatus
