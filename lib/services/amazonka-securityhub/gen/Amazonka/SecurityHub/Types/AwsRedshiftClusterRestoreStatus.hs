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
-- Module      : Amazonka.SecurityHub.Types.AwsRedshiftClusterRestoreStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRedshiftClusterRestoreStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the status of a cluster restore action. It only
-- applies if the cluster was created by restoring a snapshot.
--
-- /See:/ 'newAwsRedshiftClusterRestoreStatus' smart constructor.
data AwsRedshiftClusterRestoreStatus = AwsRedshiftClusterRestoreStatus'
  { -- | The status of the restore action.
    --
    -- Valid values: @starting@ | @restoring@ | @completed@ | @failed@
    status :: Prelude.Maybe Prelude.Text,
    -- | The estimate of the time remaining before the restore is complete.
    -- Returns 0 for a completed restore.
    --
    -- This field is only updated when you restore to DC2 and DS2 node types.
    estimatedTimeToCompletionInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The number of megabytes per second being transferred from the backup
    -- storage. Returns the average rate for a completed backup.
    --
    -- This field is only updated when you restore to DC2 and DS2 node types.
    currentRestoreRateInMegaBytesPerSecond :: Prelude.Maybe Prelude.Double,
    -- | The number of megabytes that were transferred from snapshot storage.
    --
    -- This field is only updated when you restore to DC2 and DS2 node types.
    progressInMegaBytes :: Prelude.Maybe Prelude.Integer,
    -- | The amount of time an in-progress restore has been running, or the
    -- amount of time it took a completed restore to finish.
    --
    -- This field is only updated when you restore to DC2 and DS2 node types.
    elapsedTimeInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The size of the set of snapshot data that was used to restore the
    -- cluster.
    --
    -- This field is only updated when you restore to DC2 and DS2 node types.
    snapshotSizeInMegaBytes :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRedshiftClusterRestoreStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'awsRedshiftClusterRestoreStatus_status' - The status of the restore action.
--
-- Valid values: @starting@ | @restoring@ | @completed@ | @failed@
--
-- 'estimatedTimeToCompletionInSeconds', 'awsRedshiftClusterRestoreStatus_estimatedTimeToCompletionInSeconds' - The estimate of the time remaining before the restore is complete.
-- Returns 0 for a completed restore.
--
-- This field is only updated when you restore to DC2 and DS2 node types.
--
-- 'currentRestoreRateInMegaBytesPerSecond', 'awsRedshiftClusterRestoreStatus_currentRestoreRateInMegaBytesPerSecond' - The number of megabytes per second being transferred from the backup
-- storage. Returns the average rate for a completed backup.
--
-- This field is only updated when you restore to DC2 and DS2 node types.
--
-- 'progressInMegaBytes', 'awsRedshiftClusterRestoreStatus_progressInMegaBytes' - The number of megabytes that were transferred from snapshot storage.
--
-- This field is only updated when you restore to DC2 and DS2 node types.
--
-- 'elapsedTimeInSeconds', 'awsRedshiftClusterRestoreStatus_elapsedTimeInSeconds' - The amount of time an in-progress restore has been running, or the
-- amount of time it took a completed restore to finish.
--
-- This field is only updated when you restore to DC2 and DS2 node types.
--
-- 'snapshotSizeInMegaBytes', 'awsRedshiftClusterRestoreStatus_snapshotSizeInMegaBytes' - The size of the set of snapshot data that was used to restore the
-- cluster.
--
-- This field is only updated when you restore to DC2 and DS2 node types.
newAwsRedshiftClusterRestoreStatus ::
  AwsRedshiftClusterRestoreStatus
newAwsRedshiftClusterRestoreStatus =
  AwsRedshiftClusterRestoreStatus'
    { status =
        Prelude.Nothing,
      estimatedTimeToCompletionInSeconds =
        Prelude.Nothing,
      currentRestoreRateInMegaBytesPerSecond =
        Prelude.Nothing,
      progressInMegaBytes = Prelude.Nothing,
      elapsedTimeInSeconds = Prelude.Nothing,
      snapshotSizeInMegaBytes = Prelude.Nothing
    }

-- | The status of the restore action.
--
-- Valid values: @starting@ | @restoring@ | @completed@ | @failed@
awsRedshiftClusterRestoreStatus_status :: Lens.Lens' AwsRedshiftClusterRestoreStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterRestoreStatus_status = Lens.lens (\AwsRedshiftClusterRestoreStatus' {status} -> status) (\s@AwsRedshiftClusterRestoreStatus' {} a -> s {status = a} :: AwsRedshiftClusterRestoreStatus)

-- | The estimate of the time remaining before the restore is complete.
-- Returns 0 for a completed restore.
--
-- This field is only updated when you restore to DC2 and DS2 node types.
awsRedshiftClusterRestoreStatus_estimatedTimeToCompletionInSeconds :: Lens.Lens' AwsRedshiftClusterRestoreStatus (Prelude.Maybe Prelude.Integer)
awsRedshiftClusterRestoreStatus_estimatedTimeToCompletionInSeconds = Lens.lens (\AwsRedshiftClusterRestoreStatus' {estimatedTimeToCompletionInSeconds} -> estimatedTimeToCompletionInSeconds) (\s@AwsRedshiftClusterRestoreStatus' {} a -> s {estimatedTimeToCompletionInSeconds = a} :: AwsRedshiftClusterRestoreStatus)

-- | The number of megabytes per second being transferred from the backup
-- storage. Returns the average rate for a completed backup.
--
-- This field is only updated when you restore to DC2 and DS2 node types.
awsRedshiftClusterRestoreStatus_currentRestoreRateInMegaBytesPerSecond :: Lens.Lens' AwsRedshiftClusterRestoreStatus (Prelude.Maybe Prelude.Double)
awsRedshiftClusterRestoreStatus_currentRestoreRateInMegaBytesPerSecond = Lens.lens (\AwsRedshiftClusterRestoreStatus' {currentRestoreRateInMegaBytesPerSecond} -> currentRestoreRateInMegaBytesPerSecond) (\s@AwsRedshiftClusterRestoreStatus' {} a -> s {currentRestoreRateInMegaBytesPerSecond = a} :: AwsRedshiftClusterRestoreStatus)

-- | The number of megabytes that were transferred from snapshot storage.
--
-- This field is only updated when you restore to DC2 and DS2 node types.
awsRedshiftClusterRestoreStatus_progressInMegaBytes :: Lens.Lens' AwsRedshiftClusterRestoreStatus (Prelude.Maybe Prelude.Integer)
awsRedshiftClusterRestoreStatus_progressInMegaBytes = Lens.lens (\AwsRedshiftClusterRestoreStatus' {progressInMegaBytes} -> progressInMegaBytes) (\s@AwsRedshiftClusterRestoreStatus' {} a -> s {progressInMegaBytes = a} :: AwsRedshiftClusterRestoreStatus)

-- | The amount of time an in-progress restore has been running, or the
-- amount of time it took a completed restore to finish.
--
-- This field is only updated when you restore to DC2 and DS2 node types.
awsRedshiftClusterRestoreStatus_elapsedTimeInSeconds :: Lens.Lens' AwsRedshiftClusterRestoreStatus (Prelude.Maybe Prelude.Integer)
awsRedshiftClusterRestoreStatus_elapsedTimeInSeconds = Lens.lens (\AwsRedshiftClusterRestoreStatus' {elapsedTimeInSeconds} -> elapsedTimeInSeconds) (\s@AwsRedshiftClusterRestoreStatus' {} a -> s {elapsedTimeInSeconds = a} :: AwsRedshiftClusterRestoreStatus)

-- | The size of the set of snapshot data that was used to restore the
-- cluster.
--
-- This field is only updated when you restore to DC2 and DS2 node types.
awsRedshiftClusterRestoreStatus_snapshotSizeInMegaBytes :: Lens.Lens' AwsRedshiftClusterRestoreStatus (Prelude.Maybe Prelude.Integer)
awsRedshiftClusterRestoreStatus_snapshotSizeInMegaBytes = Lens.lens (\AwsRedshiftClusterRestoreStatus' {snapshotSizeInMegaBytes} -> snapshotSizeInMegaBytes) (\s@AwsRedshiftClusterRestoreStatus' {} a -> s {snapshotSizeInMegaBytes = a} :: AwsRedshiftClusterRestoreStatus)

instance
  Core.FromJSON
    AwsRedshiftClusterRestoreStatus
  where
  parseJSON =
    Core.withObject
      "AwsRedshiftClusterRestoreStatus"
      ( \x ->
          AwsRedshiftClusterRestoreStatus'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "EstimatedTimeToCompletionInSeconds")
            Prelude.<*> (x Core..:? "CurrentRestoreRateInMegaBytesPerSecond")
            Prelude.<*> (x Core..:? "ProgressInMegaBytes")
            Prelude.<*> (x Core..:? "ElapsedTimeInSeconds")
            Prelude.<*> (x Core..:? "SnapshotSizeInMegaBytes")
      )

instance
  Prelude.Hashable
    AwsRedshiftClusterRestoreStatus
  where
  hashWithSalt
    _salt
    AwsRedshiftClusterRestoreStatus' {..} =
      _salt `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` estimatedTimeToCompletionInSeconds
        `Prelude.hashWithSalt` currentRestoreRateInMegaBytesPerSecond
        `Prelude.hashWithSalt` progressInMegaBytes
        `Prelude.hashWithSalt` elapsedTimeInSeconds
        `Prelude.hashWithSalt` snapshotSizeInMegaBytes

instance
  Prelude.NFData
    AwsRedshiftClusterRestoreStatus
  where
  rnf AwsRedshiftClusterRestoreStatus' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf estimatedTimeToCompletionInSeconds
      `Prelude.seq` Prelude.rnf currentRestoreRateInMegaBytesPerSecond
      `Prelude.seq` Prelude.rnf progressInMegaBytes
      `Prelude.seq` Prelude.rnf elapsedTimeInSeconds
      `Prelude.seq` Prelude.rnf snapshotSizeInMegaBytes

instance Core.ToJSON AwsRedshiftClusterRestoreStatus where
  toJSON AwsRedshiftClusterRestoreStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Status" Core..=) Prelude.<$> status,
            ("EstimatedTimeToCompletionInSeconds" Core..=)
              Prelude.<$> estimatedTimeToCompletionInSeconds,
            ("CurrentRestoreRateInMegaBytesPerSecond" Core..=)
              Prelude.<$> currentRestoreRateInMegaBytesPerSecond,
            ("ProgressInMegaBytes" Core..=)
              Prelude.<$> progressInMegaBytes,
            ("ElapsedTimeInSeconds" Core..=)
              Prelude.<$> elapsedTimeInSeconds,
            ("SnapshotSizeInMegaBytes" Core..=)
              Prelude.<$> snapshotSizeInMegaBytes
          ]
      )
