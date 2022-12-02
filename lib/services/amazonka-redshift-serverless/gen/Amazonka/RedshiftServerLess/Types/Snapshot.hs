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
-- Module      : Amazonka.RedshiftServerLess.Types.Snapshot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftServerLess.Types.Snapshot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types.SnapshotStatus

-- | A snapshot object that contains databases.
--
-- /See:/ 'newSnapshot' smart constructor.
data Snapshot = Snapshot'
  { -- | The rate at which data is backed up into a snapshot in megabytes per
    -- second.
    currentBackupRateInMegaBytesPerSecond :: Prelude.Maybe Prelude.Double,
    -- | The name of the namepsace.
    namespaceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the snapshot.
    snapshotName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the snapshot.
    snapshotArn :: Prelude.Maybe Prelude.Text,
    -- | The amount of days until the snapshot is deleted.
    snapshotRemainingDays :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the namespace the snapshot was created
    -- from.
    namespaceArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the snapshot.
    status :: Prelude.Maybe SnapshotStatus,
    -- | The amount of time it took to back up data into a snapshot.
    elapsedTimeInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The timestamp of when data within the snapshot started getting retained.
    snapshotRetentionStartTime :: Prelude.Maybe Data.POSIX,
    -- | The timestamp of when the snapshot was created.
    snapshotCreateTime :: Prelude.Maybe Data.POSIX,
    -- | The period of time, in days, of how long the snapshot is retained.
    snapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The estimated amount of seconds until the snapshot completes backup.
    estimatedSecondsToCompletion :: Prelude.Maybe Prelude.Integer,
    -- | The unique identifier of the KMS key used to encrypt the snapshot.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The total size, in megabytes, of how big the snapshot is.
    totalBackupSizeInMegaBytes :: Prelude.Maybe Prelude.Double,
    -- | All of the Amazon Web Services accounts that have access to restore a
    -- snapshot to a namespace.
    accountsWithRestoreAccess :: Prelude.Maybe [Prelude.Text],
    -- | The owner Amazon Web Services; account of the snapshot.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | The size in megabytes of the data that has been backed up to a snapshot.
    backupProgressInMegaBytes :: Prelude.Maybe Prelude.Double,
    -- | The username of the database within a snapshot.
    adminUsername :: Prelude.Maybe Prelude.Text,
    -- | The size of the incremental backup in megabytes.
    actualIncrementalBackupSizeInMegaBytes :: Prelude.Maybe Prelude.Double,
    -- | All of the Amazon Web Services accounts that have access to restore a
    -- snapshot to a provisioned cluster.
    accountsWithProvisionedRestoreAccess :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Snapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentBackupRateInMegaBytesPerSecond', 'snapshot_currentBackupRateInMegaBytesPerSecond' - The rate at which data is backed up into a snapshot in megabytes per
-- second.
--
-- 'namespaceName', 'snapshot_namespaceName' - The name of the namepsace.
--
-- 'snapshotName', 'snapshot_snapshotName' - The name of the snapshot.
--
-- 'snapshotArn', 'snapshot_snapshotArn' - The Amazon Resource Name (ARN) of the snapshot.
--
-- 'snapshotRemainingDays', 'snapshot_snapshotRemainingDays' - The amount of days until the snapshot is deleted.
--
-- 'namespaceArn', 'snapshot_namespaceArn' - The Amazon Resource Name (ARN) of the namespace the snapshot was created
-- from.
--
-- 'status', 'snapshot_status' - The status of the snapshot.
--
-- 'elapsedTimeInSeconds', 'snapshot_elapsedTimeInSeconds' - The amount of time it took to back up data into a snapshot.
--
-- 'snapshotRetentionStartTime', 'snapshot_snapshotRetentionStartTime' - The timestamp of when data within the snapshot started getting retained.
--
-- 'snapshotCreateTime', 'snapshot_snapshotCreateTime' - The timestamp of when the snapshot was created.
--
-- 'snapshotRetentionPeriod', 'snapshot_snapshotRetentionPeriod' - The period of time, in days, of how long the snapshot is retained.
--
-- 'estimatedSecondsToCompletion', 'snapshot_estimatedSecondsToCompletion' - The estimated amount of seconds until the snapshot completes backup.
--
-- 'kmsKeyId', 'snapshot_kmsKeyId' - The unique identifier of the KMS key used to encrypt the snapshot.
--
-- 'totalBackupSizeInMegaBytes', 'snapshot_totalBackupSizeInMegaBytes' - The total size, in megabytes, of how big the snapshot is.
--
-- 'accountsWithRestoreAccess', 'snapshot_accountsWithRestoreAccess' - All of the Amazon Web Services accounts that have access to restore a
-- snapshot to a namespace.
--
-- 'ownerAccount', 'snapshot_ownerAccount' - The owner Amazon Web Services; account of the snapshot.
--
-- 'backupProgressInMegaBytes', 'snapshot_backupProgressInMegaBytes' - The size in megabytes of the data that has been backed up to a snapshot.
--
-- 'adminUsername', 'snapshot_adminUsername' - The username of the database within a snapshot.
--
-- 'actualIncrementalBackupSizeInMegaBytes', 'snapshot_actualIncrementalBackupSizeInMegaBytes' - The size of the incremental backup in megabytes.
--
-- 'accountsWithProvisionedRestoreAccess', 'snapshot_accountsWithProvisionedRestoreAccess' - All of the Amazon Web Services accounts that have access to restore a
-- snapshot to a provisioned cluster.
newSnapshot ::
  Snapshot
newSnapshot =
  Snapshot'
    { currentBackupRateInMegaBytesPerSecond =
        Prelude.Nothing,
      namespaceName = Prelude.Nothing,
      snapshotName = Prelude.Nothing,
      snapshotArn = Prelude.Nothing,
      snapshotRemainingDays = Prelude.Nothing,
      namespaceArn = Prelude.Nothing,
      status = Prelude.Nothing,
      elapsedTimeInSeconds = Prelude.Nothing,
      snapshotRetentionStartTime = Prelude.Nothing,
      snapshotCreateTime = Prelude.Nothing,
      snapshotRetentionPeriod = Prelude.Nothing,
      estimatedSecondsToCompletion = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      totalBackupSizeInMegaBytes = Prelude.Nothing,
      accountsWithRestoreAccess = Prelude.Nothing,
      ownerAccount = Prelude.Nothing,
      backupProgressInMegaBytes = Prelude.Nothing,
      adminUsername = Prelude.Nothing,
      actualIncrementalBackupSizeInMegaBytes =
        Prelude.Nothing,
      accountsWithProvisionedRestoreAccess =
        Prelude.Nothing
    }

-- | The rate at which data is backed up into a snapshot in megabytes per
-- second.
snapshot_currentBackupRateInMegaBytesPerSecond :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Double)
snapshot_currentBackupRateInMegaBytesPerSecond = Lens.lens (\Snapshot' {currentBackupRateInMegaBytesPerSecond} -> currentBackupRateInMegaBytesPerSecond) (\s@Snapshot' {} a -> s {currentBackupRateInMegaBytesPerSecond = a} :: Snapshot)

-- | The name of the namepsace.
snapshot_namespaceName :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_namespaceName = Lens.lens (\Snapshot' {namespaceName} -> namespaceName) (\s@Snapshot' {} a -> s {namespaceName = a} :: Snapshot)

-- | The name of the snapshot.
snapshot_snapshotName :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_snapshotName = Lens.lens (\Snapshot' {snapshotName} -> snapshotName) (\s@Snapshot' {} a -> s {snapshotName = a} :: Snapshot)

-- | The Amazon Resource Name (ARN) of the snapshot.
snapshot_snapshotArn :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_snapshotArn = Lens.lens (\Snapshot' {snapshotArn} -> snapshotArn) (\s@Snapshot' {} a -> s {snapshotArn = a} :: Snapshot)

-- | The amount of days until the snapshot is deleted.
snapshot_snapshotRemainingDays :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Int)
snapshot_snapshotRemainingDays = Lens.lens (\Snapshot' {snapshotRemainingDays} -> snapshotRemainingDays) (\s@Snapshot' {} a -> s {snapshotRemainingDays = a} :: Snapshot)

-- | The Amazon Resource Name (ARN) of the namespace the snapshot was created
-- from.
snapshot_namespaceArn :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_namespaceArn = Lens.lens (\Snapshot' {namespaceArn} -> namespaceArn) (\s@Snapshot' {} a -> s {namespaceArn = a} :: Snapshot)

-- | The status of the snapshot.
snapshot_status :: Lens.Lens' Snapshot (Prelude.Maybe SnapshotStatus)
snapshot_status = Lens.lens (\Snapshot' {status} -> status) (\s@Snapshot' {} a -> s {status = a} :: Snapshot)

-- | The amount of time it took to back up data into a snapshot.
snapshot_elapsedTimeInSeconds :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Integer)
snapshot_elapsedTimeInSeconds = Lens.lens (\Snapshot' {elapsedTimeInSeconds} -> elapsedTimeInSeconds) (\s@Snapshot' {} a -> s {elapsedTimeInSeconds = a} :: Snapshot)

-- | The timestamp of when data within the snapshot started getting retained.
snapshot_snapshotRetentionStartTime :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.UTCTime)
snapshot_snapshotRetentionStartTime = Lens.lens (\Snapshot' {snapshotRetentionStartTime} -> snapshotRetentionStartTime) (\s@Snapshot' {} a -> s {snapshotRetentionStartTime = a} :: Snapshot) Prelude.. Lens.mapping Data._Time

-- | The timestamp of when the snapshot was created.
snapshot_snapshotCreateTime :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.UTCTime)
snapshot_snapshotCreateTime = Lens.lens (\Snapshot' {snapshotCreateTime} -> snapshotCreateTime) (\s@Snapshot' {} a -> s {snapshotCreateTime = a} :: Snapshot) Prelude.. Lens.mapping Data._Time

-- | The period of time, in days, of how long the snapshot is retained.
snapshot_snapshotRetentionPeriod :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Int)
snapshot_snapshotRetentionPeriod = Lens.lens (\Snapshot' {snapshotRetentionPeriod} -> snapshotRetentionPeriod) (\s@Snapshot' {} a -> s {snapshotRetentionPeriod = a} :: Snapshot)

-- | The estimated amount of seconds until the snapshot completes backup.
snapshot_estimatedSecondsToCompletion :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Integer)
snapshot_estimatedSecondsToCompletion = Lens.lens (\Snapshot' {estimatedSecondsToCompletion} -> estimatedSecondsToCompletion) (\s@Snapshot' {} a -> s {estimatedSecondsToCompletion = a} :: Snapshot)

-- | The unique identifier of the KMS key used to encrypt the snapshot.
snapshot_kmsKeyId :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_kmsKeyId = Lens.lens (\Snapshot' {kmsKeyId} -> kmsKeyId) (\s@Snapshot' {} a -> s {kmsKeyId = a} :: Snapshot)

-- | The total size, in megabytes, of how big the snapshot is.
snapshot_totalBackupSizeInMegaBytes :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Double)
snapshot_totalBackupSizeInMegaBytes = Lens.lens (\Snapshot' {totalBackupSizeInMegaBytes} -> totalBackupSizeInMegaBytes) (\s@Snapshot' {} a -> s {totalBackupSizeInMegaBytes = a} :: Snapshot)

-- | All of the Amazon Web Services accounts that have access to restore a
-- snapshot to a namespace.
snapshot_accountsWithRestoreAccess :: Lens.Lens' Snapshot (Prelude.Maybe [Prelude.Text])
snapshot_accountsWithRestoreAccess = Lens.lens (\Snapshot' {accountsWithRestoreAccess} -> accountsWithRestoreAccess) (\s@Snapshot' {} a -> s {accountsWithRestoreAccess = a} :: Snapshot) Prelude.. Lens.mapping Lens.coerced

-- | The owner Amazon Web Services; account of the snapshot.
snapshot_ownerAccount :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_ownerAccount = Lens.lens (\Snapshot' {ownerAccount} -> ownerAccount) (\s@Snapshot' {} a -> s {ownerAccount = a} :: Snapshot)

-- | The size in megabytes of the data that has been backed up to a snapshot.
snapshot_backupProgressInMegaBytes :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Double)
snapshot_backupProgressInMegaBytes = Lens.lens (\Snapshot' {backupProgressInMegaBytes} -> backupProgressInMegaBytes) (\s@Snapshot' {} a -> s {backupProgressInMegaBytes = a} :: Snapshot)

-- | The username of the database within a snapshot.
snapshot_adminUsername :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_adminUsername = Lens.lens (\Snapshot' {adminUsername} -> adminUsername) (\s@Snapshot' {} a -> s {adminUsername = a} :: Snapshot)

-- | The size of the incremental backup in megabytes.
snapshot_actualIncrementalBackupSizeInMegaBytes :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Double)
snapshot_actualIncrementalBackupSizeInMegaBytes = Lens.lens (\Snapshot' {actualIncrementalBackupSizeInMegaBytes} -> actualIncrementalBackupSizeInMegaBytes) (\s@Snapshot' {} a -> s {actualIncrementalBackupSizeInMegaBytes = a} :: Snapshot)

-- | All of the Amazon Web Services accounts that have access to restore a
-- snapshot to a provisioned cluster.
snapshot_accountsWithProvisionedRestoreAccess :: Lens.Lens' Snapshot (Prelude.Maybe [Prelude.Text])
snapshot_accountsWithProvisionedRestoreAccess = Lens.lens (\Snapshot' {accountsWithProvisionedRestoreAccess} -> accountsWithProvisionedRestoreAccess) (\s@Snapshot' {} a -> s {accountsWithProvisionedRestoreAccess = a} :: Snapshot) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Snapshot where
  parseJSON =
    Data.withObject
      "Snapshot"
      ( \x ->
          Snapshot'
            Prelude.<$> (x Data..:? "currentBackupRateInMegaBytesPerSecond")
            Prelude.<*> (x Data..:? "namespaceName")
            Prelude.<*> (x Data..:? "snapshotName")
            Prelude.<*> (x Data..:? "snapshotArn")
            Prelude.<*> (x Data..:? "snapshotRemainingDays")
            Prelude.<*> (x Data..:? "namespaceArn")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "elapsedTimeInSeconds")
            Prelude.<*> (x Data..:? "snapshotRetentionStartTime")
            Prelude.<*> (x Data..:? "snapshotCreateTime")
            Prelude.<*> (x Data..:? "snapshotRetentionPeriod")
            Prelude.<*> (x Data..:? "estimatedSecondsToCompletion")
            Prelude.<*> (x Data..:? "kmsKeyId")
            Prelude.<*> (x Data..:? "totalBackupSizeInMegaBytes")
            Prelude.<*> ( x Data..:? "accountsWithRestoreAccess"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ownerAccount")
            Prelude.<*> (x Data..:? "backupProgressInMegaBytes")
            Prelude.<*> (x Data..:? "adminUsername")
            Prelude.<*> (x Data..:? "actualIncrementalBackupSizeInMegaBytes")
            Prelude.<*> ( x Data..:? "accountsWithProvisionedRestoreAccess"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Snapshot where
  hashWithSalt _salt Snapshot' {..} =
    _salt
      `Prelude.hashWithSalt` currentBackupRateInMegaBytesPerSecond
      `Prelude.hashWithSalt` namespaceName
      `Prelude.hashWithSalt` snapshotName
      `Prelude.hashWithSalt` snapshotArn
      `Prelude.hashWithSalt` snapshotRemainingDays
      `Prelude.hashWithSalt` namespaceArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` elapsedTimeInSeconds
      `Prelude.hashWithSalt` snapshotRetentionStartTime
      `Prelude.hashWithSalt` snapshotCreateTime
      `Prelude.hashWithSalt` snapshotRetentionPeriod
      `Prelude.hashWithSalt` estimatedSecondsToCompletion
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` totalBackupSizeInMegaBytes
      `Prelude.hashWithSalt` accountsWithRestoreAccess
      `Prelude.hashWithSalt` ownerAccount
      `Prelude.hashWithSalt` backupProgressInMegaBytes
      `Prelude.hashWithSalt` adminUsername
      `Prelude.hashWithSalt` actualIncrementalBackupSizeInMegaBytes
      `Prelude.hashWithSalt` accountsWithProvisionedRestoreAccess

instance Prelude.NFData Snapshot where
  rnf Snapshot' {..} =
    Prelude.rnf currentBackupRateInMegaBytesPerSecond
      `Prelude.seq` Prelude.rnf namespaceName
      `Prelude.seq` Prelude.rnf snapshotName
      `Prelude.seq` Prelude.rnf snapshotArn
      `Prelude.seq` Prelude.rnf snapshotRemainingDays
      `Prelude.seq` Prelude.rnf namespaceArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf elapsedTimeInSeconds
      `Prelude.seq` Prelude.rnf snapshotRetentionStartTime
      `Prelude.seq` Prelude.rnf snapshotCreateTime
      `Prelude.seq` Prelude.rnf snapshotRetentionPeriod
      `Prelude.seq` Prelude.rnf estimatedSecondsToCompletion
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf totalBackupSizeInMegaBytes
      `Prelude.seq` Prelude.rnf accountsWithRestoreAccess
      `Prelude.seq` Prelude.rnf ownerAccount
      `Prelude.seq` Prelude.rnf
        backupProgressInMegaBytes
      `Prelude.seq` Prelude.rnf adminUsername
      `Prelude.seq` Prelude.rnf
        actualIncrementalBackupSizeInMegaBytes
      `Prelude.seq` Prelude.rnf
        accountsWithProvisionedRestoreAccess
