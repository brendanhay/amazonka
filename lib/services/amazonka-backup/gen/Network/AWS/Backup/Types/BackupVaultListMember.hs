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
-- Module      : Network.AWS.Backup.Types.BackupVaultListMember
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Backup.Types.BackupVaultListMember where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains metadata about a backup vault.
--
-- /See:/ 'newBackupVaultListMember' smart constructor.
data BackupVaultListMember = BackupVaultListMember'
  { -- | The date and time when Backup Vault Lock configuration becomes
    -- immutable, meaning it cannot be changed or deleted.
    --
    -- If you applied Vault Lock to your vault without specifying a lock date,
    -- you can change your Vault Lock settings, or delete Vault Lock from the
    -- vault entirely, at any time.
    --
    -- This value is in Unix format, Coordinated Universal Time (UTC), and
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    lockDate :: Prelude.Maybe Core.POSIX,
    -- | The Backup Vault Lock setting that specifies the maximum retention
    -- period that the vault retains its recovery points. If this parameter is
    -- not specified, Vault Lock does not enforce a maximum retention period on
    -- the recovery points in the vault (allowing indefinite storage).
    --
    -- If specified, any backup or copy job to the vault must have a lifecycle
    -- policy with a retention period equal to or shorter than the maximum
    -- retention period. If the job\'s retention period is longer than that
    -- maximum retention period, then the vault fails the backup or copy job,
    -- and you should either modify your lifecycle settings or use a different
    -- vault. Recovery points already stored in the vault prior to Vault Lock
    -- are not affected.
    maxRetentionDays :: Prelude.Maybe Prelude.Integer,
    -- | A Boolean value that indicates whether Backup Vault Lock applies to the
    -- selected backup vault. If @true@, Vault Lock prevents delete and update
    -- operations on the recovery points in the selected vault.
    locked :: Prelude.Maybe Prelude.Bool,
    -- | A unique string that identifies the request and allows failed requests
    -- to be retried without the risk of running the operation twice.
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | The number of recovery points that are stored in a backup vault.
    numberOfRecoveryPoints :: Prelude.Maybe Prelude.Integer,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault;
    -- for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
    backupVaultArn :: Prelude.Maybe Prelude.Text,
    -- | The server-side encryption key that is used to protect your backups; for
    -- example,
    -- @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
    encryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time a resource backup is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Amazon Web Services Region where they are created. They
    -- consist of lowercase letters, numbers, and hyphens.
    backupVaultName :: Prelude.Maybe Prelude.Text,
    -- | The Backup Vault Lock setting that specifies the minimum retention
    -- period that the vault retains its recovery points. If this parameter is
    -- not specified, Vault Lock does not enforce a minimum retention period.
    --
    -- If specified, any backup or copy job to the vault must have a lifecycle
    -- policy with a retention period equal to or longer than the minimum
    -- retention period. If the job\'s retention period is shorter than that
    -- minimum retention period, then the vault fails the backup or copy job,
    -- and you should either modify your lifecycle settings or use a different
    -- vault. Recovery points already stored in the vault prior to Vault Lock
    -- are not affected.
    minRetentionDays :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackupVaultListMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lockDate', 'backupVaultListMember_lockDate' - The date and time when Backup Vault Lock configuration becomes
-- immutable, meaning it cannot be changed or deleted.
--
-- If you applied Vault Lock to your vault without specifying a lock date,
-- you can change your Vault Lock settings, or delete Vault Lock from the
-- vault entirely, at any time.
--
-- This value is in Unix format, Coordinated Universal Time (UTC), and
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'maxRetentionDays', 'backupVaultListMember_maxRetentionDays' - The Backup Vault Lock setting that specifies the maximum retention
-- period that the vault retains its recovery points. If this parameter is
-- not specified, Vault Lock does not enforce a maximum retention period on
-- the recovery points in the vault (allowing indefinite storage).
--
-- If specified, any backup or copy job to the vault must have a lifecycle
-- policy with a retention period equal to or shorter than the maximum
-- retention period. If the job\'s retention period is longer than that
-- maximum retention period, then the vault fails the backup or copy job,
-- and you should either modify your lifecycle settings or use a different
-- vault. Recovery points already stored in the vault prior to Vault Lock
-- are not affected.
--
-- 'locked', 'backupVaultListMember_locked' - A Boolean value that indicates whether Backup Vault Lock applies to the
-- selected backup vault. If @true@, Vault Lock prevents delete and update
-- operations on the recovery points in the selected vault.
--
-- 'creatorRequestId', 'backupVaultListMember_creatorRequestId' - A unique string that identifies the request and allows failed requests
-- to be retried without the risk of running the operation twice.
--
-- 'numberOfRecoveryPoints', 'backupVaultListMember_numberOfRecoveryPoints' - The number of recovery points that are stored in a backup vault.
--
-- 'backupVaultArn', 'backupVaultListMember_backupVaultArn' - An Amazon Resource Name (ARN) that uniquely identifies a backup vault;
-- for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
--
-- 'encryptionKeyArn', 'backupVaultListMember_encryptionKeyArn' - The server-side encryption key that is used to protect your backups; for
-- example,
-- @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
--
-- 'creationDate', 'backupVaultListMember_creationDate' - The date and time a resource backup is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'backupVaultName', 'backupVaultListMember_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
--
-- 'minRetentionDays', 'backupVaultListMember_minRetentionDays' - The Backup Vault Lock setting that specifies the minimum retention
-- period that the vault retains its recovery points. If this parameter is
-- not specified, Vault Lock does not enforce a minimum retention period.
--
-- If specified, any backup or copy job to the vault must have a lifecycle
-- policy with a retention period equal to or longer than the minimum
-- retention period. If the job\'s retention period is shorter than that
-- minimum retention period, then the vault fails the backup or copy job,
-- and you should either modify your lifecycle settings or use a different
-- vault. Recovery points already stored in the vault prior to Vault Lock
-- are not affected.
newBackupVaultListMember ::
  BackupVaultListMember
newBackupVaultListMember =
  BackupVaultListMember'
    { lockDate = Prelude.Nothing,
      maxRetentionDays = Prelude.Nothing,
      locked = Prelude.Nothing,
      creatorRequestId = Prelude.Nothing,
      numberOfRecoveryPoints = Prelude.Nothing,
      backupVaultArn = Prelude.Nothing,
      encryptionKeyArn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      backupVaultName = Prelude.Nothing,
      minRetentionDays = Prelude.Nothing
    }

-- | The date and time when Backup Vault Lock configuration becomes
-- immutable, meaning it cannot be changed or deleted.
--
-- If you applied Vault Lock to your vault without specifying a lock date,
-- you can change your Vault Lock settings, or delete Vault Lock from the
-- vault entirely, at any time.
--
-- This value is in Unix format, Coordinated Universal Time (UTC), and
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
backupVaultListMember_lockDate :: Lens.Lens' BackupVaultListMember (Prelude.Maybe Prelude.UTCTime)
backupVaultListMember_lockDate = Lens.lens (\BackupVaultListMember' {lockDate} -> lockDate) (\s@BackupVaultListMember' {} a -> s {lockDate = a} :: BackupVaultListMember) Prelude.. Lens.mapping Core._Time

-- | The Backup Vault Lock setting that specifies the maximum retention
-- period that the vault retains its recovery points. If this parameter is
-- not specified, Vault Lock does not enforce a maximum retention period on
-- the recovery points in the vault (allowing indefinite storage).
--
-- If specified, any backup or copy job to the vault must have a lifecycle
-- policy with a retention period equal to or shorter than the maximum
-- retention period. If the job\'s retention period is longer than that
-- maximum retention period, then the vault fails the backup or copy job,
-- and you should either modify your lifecycle settings or use a different
-- vault. Recovery points already stored in the vault prior to Vault Lock
-- are not affected.
backupVaultListMember_maxRetentionDays :: Lens.Lens' BackupVaultListMember (Prelude.Maybe Prelude.Integer)
backupVaultListMember_maxRetentionDays = Lens.lens (\BackupVaultListMember' {maxRetentionDays} -> maxRetentionDays) (\s@BackupVaultListMember' {} a -> s {maxRetentionDays = a} :: BackupVaultListMember)

-- | A Boolean value that indicates whether Backup Vault Lock applies to the
-- selected backup vault. If @true@, Vault Lock prevents delete and update
-- operations on the recovery points in the selected vault.
backupVaultListMember_locked :: Lens.Lens' BackupVaultListMember (Prelude.Maybe Prelude.Bool)
backupVaultListMember_locked = Lens.lens (\BackupVaultListMember' {locked} -> locked) (\s@BackupVaultListMember' {} a -> s {locked = a} :: BackupVaultListMember)

-- | A unique string that identifies the request and allows failed requests
-- to be retried without the risk of running the operation twice.
backupVaultListMember_creatorRequestId :: Lens.Lens' BackupVaultListMember (Prelude.Maybe Prelude.Text)
backupVaultListMember_creatorRequestId = Lens.lens (\BackupVaultListMember' {creatorRequestId} -> creatorRequestId) (\s@BackupVaultListMember' {} a -> s {creatorRequestId = a} :: BackupVaultListMember)

-- | The number of recovery points that are stored in a backup vault.
backupVaultListMember_numberOfRecoveryPoints :: Lens.Lens' BackupVaultListMember (Prelude.Maybe Prelude.Integer)
backupVaultListMember_numberOfRecoveryPoints = Lens.lens (\BackupVaultListMember' {numberOfRecoveryPoints} -> numberOfRecoveryPoints) (\s@BackupVaultListMember' {} a -> s {numberOfRecoveryPoints = a} :: BackupVaultListMember)

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault;
-- for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
backupVaultListMember_backupVaultArn :: Lens.Lens' BackupVaultListMember (Prelude.Maybe Prelude.Text)
backupVaultListMember_backupVaultArn = Lens.lens (\BackupVaultListMember' {backupVaultArn} -> backupVaultArn) (\s@BackupVaultListMember' {} a -> s {backupVaultArn = a} :: BackupVaultListMember)

-- | The server-side encryption key that is used to protect your backups; for
-- example,
-- @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
backupVaultListMember_encryptionKeyArn :: Lens.Lens' BackupVaultListMember (Prelude.Maybe Prelude.Text)
backupVaultListMember_encryptionKeyArn = Lens.lens (\BackupVaultListMember' {encryptionKeyArn} -> encryptionKeyArn) (\s@BackupVaultListMember' {} a -> s {encryptionKeyArn = a} :: BackupVaultListMember)

-- | The date and time a resource backup is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
backupVaultListMember_creationDate :: Lens.Lens' BackupVaultListMember (Prelude.Maybe Prelude.UTCTime)
backupVaultListMember_creationDate = Lens.lens (\BackupVaultListMember' {creationDate} -> creationDate) (\s@BackupVaultListMember' {} a -> s {creationDate = a} :: BackupVaultListMember) Prelude.. Lens.mapping Core._Time

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
backupVaultListMember_backupVaultName :: Lens.Lens' BackupVaultListMember (Prelude.Maybe Prelude.Text)
backupVaultListMember_backupVaultName = Lens.lens (\BackupVaultListMember' {backupVaultName} -> backupVaultName) (\s@BackupVaultListMember' {} a -> s {backupVaultName = a} :: BackupVaultListMember)

-- | The Backup Vault Lock setting that specifies the minimum retention
-- period that the vault retains its recovery points. If this parameter is
-- not specified, Vault Lock does not enforce a minimum retention period.
--
-- If specified, any backup or copy job to the vault must have a lifecycle
-- policy with a retention period equal to or longer than the minimum
-- retention period. If the job\'s retention period is shorter than that
-- minimum retention period, then the vault fails the backup or copy job,
-- and you should either modify your lifecycle settings or use a different
-- vault. Recovery points already stored in the vault prior to Vault Lock
-- are not affected.
backupVaultListMember_minRetentionDays :: Lens.Lens' BackupVaultListMember (Prelude.Maybe Prelude.Integer)
backupVaultListMember_minRetentionDays = Lens.lens (\BackupVaultListMember' {minRetentionDays} -> minRetentionDays) (\s@BackupVaultListMember' {} a -> s {minRetentionDays = a} :: BackupVaultListMember)

instance Core.FromJSON BackupVaultListMember where
  parseJSON =
    Core.withObject
      "BackupVaultListMember"
      ( \x ->
          BackupVaultListMember'
            Prelude.<$> (x Core..:? "LockDate")
            Prelude.<*> (x Core..:? "MaxRetentionDays")
            Prelude.<*> (x Core..:? "Locked")
            Prelude.<*> (x Core..:? "CreatorRequestId")
            Prelude.<*> (x Core..:? "NumberOfRecoveryPoints")
            Prelude.<*> (x Core..:? "BackupVaultArn")
            Prelude.<*> (x Core..:? "EncryptionKeyArn")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "BackupVaultName")
            Prelude.<*> (x Core..:? "MinRetentionDays")
      )

instance Prelude.Hashable BackupVaultListMember

instance Prelude.NFData BackupVaultListMember
