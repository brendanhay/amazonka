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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRedshiftClusterRestoreStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the status of a cluster restore action. It only
-- applies if the cluster was created by restoring a snapshot.
--
-- /See:/ 'newAwsRedshiftClusterRestoreStatus' smart constructor.
data AwsRedshiftClusterRestoreStatus = AwsRedshiftClusterRestoreStatus'
  { -- | The number of megabytes per second being transferred from the backup
    -- storage. Returns the average rate for a completed backup.
    --
    -- This field is only updated when you restore to DC2 and DS2 node types.
    currentRestoreRateInMegaBytesPerSecond :: Prelude.Maybe Prelude.Double,
    -- | The amount of time an in-progress restore has been running, or the
    -- amount of time it took a completed restore to finish.
    --
    -- This field is only updated when you restore to DC2 and DS2 node types.
    elapsedTimeInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The estimate of the time remaining before the restore is complete.
    -- Returns 0 for a completed restore.
    --
    -- This field is only updated when you restore to DC2 and DS2 node types.
    estimatedTimeToCompletionInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The number of megabytes that were transferred from snapshot storage.
    --
    -- This field is only updated when you restore to DC2 and DS2 node types.
    progressInMegaBytes :: Prelude.Maybe Prelude.Integer,
    -- | The size of the set of snapshot data that was used to restore the
    -- cluster.
    --
    -- This field is only updated when you restore to DC2 and DS2 node types.
    snapshotSizeInMegaBytes :: Prelude.Maybe Prelude.Integer,
    -- | The status of the restore action.
    --
    -- Valid values: @starting@ | @restoring@ | @completed@ | @failed@
    status :: Prelude.Maybe Prelude.Text
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
-- 'currentRestoreRateInMegaBytesPerSecond', 'awsRedshiftClusterRestoreStatus_currentRestoreRateInMegaBytesPerSecond' - The number of megabytes per second being transferred from the backup
-- storage. Returns the average rate for a completed backup.
--
-- This field is only updated when you restore to DC2 and DS2 node types.
--
-- 'elapsedTimeInSeconds', 'awsRedshiftClusterRestoreStatus_elapsedTimeInSeconds' - The amount of time an in-progress restore has been running, or the
-- amount of time it took a completed restore to finish.
--
-- This field is only updated when you restore to DC2 and DS2 node types.
--
-- 'estimatedTimeToCompletionInSeconds', 'awsRedshiftClusterRestoreStatus_estimatedTimeToCompletionInSeconds' - The estimate of the time remaining before the restore is complete.
-- Returns 0 for a completed restore.
--
-- This field is only updated when you restore to DC2 and DS2 node types.
--
-- 'progressInMegaBytes', 'awsRedshiftClusterRestoreStatus_progressInMegaBytes' - The number of megabytes that were transferred from snapshot storage.
--
-- This field is only updated when you restore to DC2 and DS2 node types.
--
-- 'snapshotSizeInMegaBytes', 'awsRedshiftClusterRestoreStatus_snapshotSizeInMegaBytes' - The size of the set of snapshot data that was used to restore the
-- cluster.
--
-- This field is only updated when you restore to DC2 and DS2 node types.
--
-- 'status', 'awsRedshiftClusterRestoreStatus_status' - The status of the restore action.
--
-- Valid values: @starting@ | @restoring@ | @completed@ | @failed@
newAwsRedshiftClusterRestoreStatus ::
  AwsRedshiftClusterRestoreStatus
newAwsRedshiftClusterRestoreStatus =
  AwsRedshiftClusterRestoreStatus'
    { currentRestoreRateInMegaBytesPerSecond =
        Prelude.Nothing,
      elapsedTimeInSeconds = Prelude.Nothing,
      estimatedTimeToCompletionInSeconds =
        Prelude.Nothing,
      progressInMegaBytes = Prelude.Nothing,
      snapshotSizeInMegaBytes = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The number of megabytes per second being transferred from the backup
-- storage. Returns the average rate for a completed backup.
--
-- This field is only updated when you restore to DC2 and DS2 node types.
awsRedshiftClusterRestoreStatus_currentRestoreRateInMegaBytesPerSecond :: Lens.Lens' AwsRedshiftClusterRestoreStatus (Prelude.Maybe Prelude.Double)
awsRedshiftClusterRestoreStatus_currentRestoreRateInMegaBytesPerSecond = Lens.lens (\AwsRedshiftClusterRestoreStatus' {currentRestoreRateInMegaBytesPerSecond} -> currentRestoreRateInMegaBytesPerSecond) (\s@AwsRedshiftClusterRestoreStatus' {} a -> s {currentRestoreRateInMegaBytesPerSecond = a} :: AwsRedshiftClusterRestoreStatus)

-- | The amount of time an in-progress restore has been running, or the
-- amount of time it took a completed restore to finish.
--
-- This field is only updated when you restore to DC2 and DS2 node types.
awsRedshiftClusterRestoreStatus_elapsedTimeInSeconds :: Lens.Lens' AwsRedshiftClusterRestoreStatus (Prelude.Maybe Prelude.Integer)
awsRedshiftClusterRestoreStatus_elapsedTimeInSeconds = Lens.lens (\AwsRedshiftClusterRestoreStatus' {elapsedTimeInSeconds} -> elapsedTimeInSeconds) (\s@AwsRedshiftClusterRestoreStatus' {} a -> s {elapsedTimeInSeconds = a} :: AwsRedshiftClusterRestoreStatus)

-- | The estimate of the time remaining before the restore is complete.
-- Returns 0 for a completed restore.
--
-- This field is only updated when you restore to DC2 and DS2 node types.
awsRedshiftClusterRestoreStatus_estimatedTimeToCompletionInSeconds :: Lens.Lens' AwsRedshiftClusterRestoreStatus (Prelude.Maybe Prelude.Integer)
awsRedshiftClusterRestoreStatus_estimatedTimeToCompletionInSeconds = Lens.lens (\AwsRedshiftClusterRestoreStatus' {estimatedTimeToCompletionInSeconds} -> estimatedTimeToCompletionInSeconds) (\s@AwsRedshiftClusterRestoreStatus' {} a -> s {estimatedTimeToCompletionInSeconds = a} :: AwsRedshiftClusterRestoreStatus)

-- | The number of megabytes that were transferred from snapshot storage.
--
-- This field is only updated when you restore to DC2 and DS2 node types.
awsRedshiftClusterRestoreStatus_progressInMegaBytes :: Lens.Lens' AwsRedshiftClusterRestoreStatus (Prelude.Maybe Prelude.Integer)
awsRedshiftClusterRestoreStatus_progressInMegaBytes = Lens.lens (\AwsRedshiftClusterRestoreStatus' {progressInMegaBytes} -> progressInMegaBytes) (\s@AwsRedshiftClusterRestoreStatus' {} a -> s {progressInMegaBytes = a} :: AwsRedshiftClusterRestoreStatus)

-- | The size of the set of snapshot data that was used to restore the
-- cluster.
--
-- This field is only updated when you restore to DC2 and DS2 node types.
awsRedshiftClusterRestoreStatus_snapshotSizeInMegaBytes :: Lens.Lens' AwsRedshiftClusterRestoreStatus (Prelude.Maybe Prelude.Integer)
awsRedshiftClusterRestoreStatus_snapshotSizeInMegaBytes = Lens.lens (\AwsRedshiftClusterRestoreStatus' {snapshotSizeInMegaBytes} -> snapshotSizeInMegaBytes) (\s@AwsRedshiftClusterRestoreStatus' {} a -> s {snapshotSizeInMegaBytes = a} :: AwsRedshiftClusterRestoreStatus)

-- | The status of the restore action.
--
-- Valid values: @starting@ | @restoring@ | @completed@ | @failed@
awsRedshiftClusterRestoreStatus_status :: Lens.Lens' AwsRedshiftClusterRestoreStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterRestoreStatus_status = Lens.lens (\AwsRedshiftClusterRestoreStatus' {status} -> status) (\s@AwsRedshiftClusterRestoreStatus' {} a -> s {status = a} :: AwsRedshiftClusterRestoreStatus)

instance
  Data.FromJSON
    AwsRedshiftClusterRestoreStatus
  where
  parseJSON =
    Data.withObject
      "AwsRedshiftClusterRestoreStatus"
      ( \x ->
          AwsRedshiftClusterRestoreStatus'
            Prelude.<$> (x Data..:? "CurrentRestoreRateInMegaBytesPerSecond")
            Prelude.<*> (x Data..:? "ElapsedTimeInSeconds")
            Prelude.<*> (x Data..:? "EstimatedTimeToCompletionInSeconds")
            Prelude.<*> (x Data..:? "ProgressInMegaBytes")
            Prelude.<*> (x Data..:? "SnapshotSizeInMegaBytes")
            Prelude.<*> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    AwsRedshiftClusterRestoreStatus
  where
  hashWithSalt
    _salt
    AwsRedshiftClusterRestoreStatus' {..} =
      _salt
        `Prelude.hashWithSalt` currentRestoreRateInMegaBytesPerSecond
        `Prelude.hashWithSalt` elapsedTimeInSeconds
        `Prelude.hashWithSalt` estimatedTimeToCompletionInSeconds
        `Prelude.hashWithSalt` progressInMegaBytes
        `Prelude.hashWithSalt` snapshotSizeInMegaBytes
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    AwsRedshiftClusterRestoreStatus
  where
  rnf AwsRedshiftClusterRestoreStatus' {..} =
    Prelude.rnf currentRestoreRateInMegaBytesPerSecond `Prelude.seq`
      Prelude.rnf elapsedTimeInSeconds `Prelude.seq`
        Prelude.rnf estimatedTimeToCompletionInSeconds `Prelude.seq`
          Prelude.rnf progressInMegaBytes `Prelude.seq`
            Prelude.rnf snapshotSizeInMegaBytes `Prelude.seq`
              Prelude.rnf status

instance Data.ToJSON AwsRedshiftClusterRestoreStatus where
  toJSON AwsRedshiftClusterRestoreStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CurrentRestoreRateInMegaBytesPerSecond" Data..=)
              Prelude.<$> currentRestoreRateInMegaBytesPerSecond,
            ("ElapsedTimeInSeconds" Data..=)
              Prelude.<$> elapsedTimeInSeconds,
            ("EstimatedTimeToCompletionInSeconds" Data..=)
              Prelude.<$> estimatedTimeToCompletionInSeconds,
            ("ProgressInMegaBytes" Data..=)
              Prelude.<$> progressInMegaBytes,
            ("SnapshotSizeInMegaBytes" Data..=)
              Prelude.<$> snapshotSizeInMegaBytes,
            ("Status" Data..=) Prelude.<$> status
          ]
      )
