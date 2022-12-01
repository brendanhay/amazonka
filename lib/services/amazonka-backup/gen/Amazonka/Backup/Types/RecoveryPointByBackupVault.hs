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
-- Module      : Amazonka.Backup.Types.RecoveryPointByBackupVault
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.RecoveryPointByBackupVault where

import Amazonka.Backup.Types.CalculatedLifecycle
import Amazonka.Backup.Types.Lifecycle
import Amazonka.Backup.Types.RecoveryPointCreator
import Amazonka.Backup.Types.RecoveryPointStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains detailed information about the recovery points stored in a
-- backup vault.
--
-- /See:/ 'newRecoveryPointByBackupVault' smart constructor.
data RecoveryPointByBackupVault = RecoveryPointByBackupVault'
  { -- | The server-side encryption key that is used to protect your backups; for
    -- example,
    -- @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
    encryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The type of Amazon Web Services resource saved as a recovery point; for
    -- example, an Amazon Elastic Block Store (Amazon EBS) volume or an Amazon
    -- Relational Database Service (Amazon RDS) database. For Windows Volume
    -- Shadow Copy Service (VSS) backups, the only supported resource type is
    -- Amazon EC2.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The lifecycle defines when a protected resource is transitioned to cold
    -- storage and when it expires. Backup transitions and expires backups
    -- automatically according to the lifecycle that you define.
    --
    -- Backups transitioned to cold storage must be stored in cold storage for
    -- a minimum of 90 days. Therefore, the “retention” setting must be 90 days
    -- greater than the “transition to cold after days” setting. The
    -- “transition to cold after days” setting cannot be changed after a backup
    -- has been transitioned to cold.
    --
    -- Resource types that are able to be transitioned to cold storage are
    -- listed in the \"Lifecycle to cold storage\" section of the
    -- <https://docs.aws.amazon.com/aws-backup/latest/devguide/whatisbackup.html#features-by-resource Feature availability by resource>
    -- table. Backup ignores this expression for other resource types.
    lifecycle :: Prelude.Maybe Lifecycle,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a recovery point;
    -- for example,
    -- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
    recoveryPointArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time a job to restore a recovery point is completed, in
    -- Unix format and Coordinated Universal Time (UTC). The value of
    -- @CompletionDate@ is accurate to milliseconds. For example, the value
    -- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
    completionDate :: Prelude.Maybe Core.POSIX,
    -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Amazon Web Services Region where they are created. They
    -- consist of lowercase letters, numbers, and hyphens.
    backupVaultName :: Prelude.Maybe Prelude.Text,
    -- | The date and time a recovery point is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The size, in bytes, of a backup.
    backupSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | A status code specifying the state of the recovery point.
    status :: Prelude.Maybe RecoveryPointStatus,
    -- | An ARN that uniquely identifies a backup vault; for example,
    -- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
    backupVaultArn :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value that is returned as @TRUE@ if the specified recovery
    -- point is encrypted, or @FALSE@ if the recovery point is not encrypted.
    isEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the IAM role ARN used to create the target recovery point; for
    -- example, @arn:aws:iam::123456789012:role\/S3Access@.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The backup vault where the recovery point was originally copied from. If
    -- the recovery point is restored to the same account this value will be
    -- @null@.
    sourceBackupVaultArn :: Prelude.Maybe Prelude.Text,
    -- | An ARN that uniquely identifies a resource. The format of the ARN
    -- depends on the resource type.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | A message explaining the reason of the recovery point deletion failure.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | Contains identifying information about the creation of a recovery point,
    -- including the @BackupPlanArn@, @BackupPlanId@, @BackupPlanVersion@, and
    -- @BackupRuleId@ of the backup plan that is used to create it.
    createdBy :: Prelude.Maybe RecoveryPointCreator,
    -- | The date and time a recovery point was last restored, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @LastRestoreTime@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    lastRestoreTime :: Prelude.Maybe Core.POSIX,
    -- | A @CalculatedLifecycle@ object containing @DeleteAt@ and
    -- @MoveToColdStorageAt@ timestamps.
    calculatedLifecycle :: Prelude.Maybe CalculatedLifecycle
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecoveryPointByBackupVault' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionKeyArn', 'recoveryPointByBackupVault_encryptionKeyArn' - The server-side encryption key that is used to protect your backups; for
-- example,
-- @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
--
-- 'resourceType', 'recoveryPointByBackupVault_resourceType' - The type of Amazon Web Services resource saved as a recovery point; for
-- example, an Amazon Elastic Block Store (Amazon EBS) volume or an Amazon
-- Relational Database Service (Amazon RDS) database. For Windows Volume
-- Shadow Copy Service (VSS) backups, the only supported resource type is
-- Amazon EC2.
--
-- 'lifecycle', 'recoveryPointByBackupVault_lifecycle' - The lifecycle defines when a protected resource is transitioned to cold
-- storage and when it expires. Backup transitions and expires backups
-- automatically according to the lifecycle that you define.
--
-- Backups transitioned to cold storage must be stored in cold storage for
-- a minimum of 90 days. Therefore, the “retention” setting must be 90 days
-- greater than the “transition to cold after days” setting. The
-- “transition to cold after days” setting cannot be changed after a backup
-- has been transitioned to cold.
--
-- Resource types that are able to be transitioned to cold storage are
-- listed in the \"Lifecycle to cold storage\" section of the
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/whatisbackup.html#features-by-resource Feature availability by resource>
-- table. Backup ignores this expression for other resource types.
--
-- 'recoveryPointArn', 'recoveryPointByBackupVault_recoveryPointArn' - An Amazon Resource Name (ARN) that uniquely identifies a recovery point;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
--
-- 'completionDate', 'recoveryPointByBackupVault_completionDate' - The date and time a job to restore a recovery point is completed, in
-- Unix format and Coordinated Universal Time (UTC). The value of
-- @CompletionDate@ is accurate to milliseconds. For example, the value
-- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'backupVaultName', 'recoveryPointByBackupVault_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
--
-- 'creationDate', 'recoveryPointByBackupVault_creationDate' - The date and time a recovery point is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'backupSizeInBytes', 'recoveryPointByBackupVault_backupSizeInBytes' - The size, in bytes, of a backup.
--
-- 'status', 'recoveryPointByBackupVault_status' - A status code specifying the state of the recovery point.
--
-- 'backupVaultArn', 'recoveryPointByBackupVault_backupVaultArn' - An ARN that uniquely identifies a backup vault; for example,
-- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
--
-- 'isEncrypted', 'recoveryPointByBackupVault_isEncrypted' - A Boolean value that is returned as @TRUE@ if the specified recovery
-- point is encrypted, or @FALSE@ if the recovery point is not encrypted.
--
-- 'iamRoleArn', 'recoveryPointByBackupVault_iamRoleArn' - Specifies the IAM role ARN used to create the target recovery point; for
-- example, @arn:aws:iam::123456789012:role\/S3Access@.
--
-- 'sourceBackupVaultArn', 'recoveryPointByBackupVault_sourceBackupVaultArn' - The backup vault where the recovery point was originally copied from. If
-- the recovery point is restored to the same account this value will be
-- @null@.
--
-- 'resourceArn', 'recoveryPointByBackupVault_resourceArn' - An ARN that uniquely identifies a resource. The format of the ARN
-- depends on the resource type.
--
-- 'statusMessage', 'recoveryPointByBackupVault_statusMessage' - A message explaining the reason of the recovery point deletion failure.
--
-- 'createdBy', 'recoveryPointByBackupVault_createdBy' - Contains identifying information about the creation of a recovery point,
-- including the @BackupPlanArn@, @BackupPlanId@, @BackupPlanVersion@, and
-- @BackupRuleId@ of the backup plan that is used to create it.
--
-- 'lastRestoreTime', 'recoveryPointByBackupVault_lastRestoreTime' - The date and time a recovery point was last restored, in Unix format and
-- Coordinated Universal Time (UTC). The value of @LastRestoreTime@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'calculatedLifecycle', 'recoveryPointByBackupVault_calculatedLifecycle' - A @CalculatedLifecycle@ object containing @DeleteAt@ and
-- @MoveToColdStorageAt@ timestamps.
newRecoveryPointByBackupVault ::
  RecoveryPointByBackupVault
newRecoveryPointByBackupVault =
  RecoveryPointByBackupVault'
    { encryptionKeyArn =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      lifecycle = Prelude.Nothing,
      recoveryPointArn = Prelude.Nothing,
      completionDate = Prelude.Nothing,
      backupVaultName = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      backupSizeInBytes = Prelude.Nothing,
      status = Prelude.Nothing,
      backupVaultArn = Prelude.Nothing,
      isEncrypted = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      sourceBackupVaultArn = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      lastRestoreTime = Prelude.Nothing,
      calculatedLifecycle = Prelude.Nothing
    }

-- | The server-side encryption key that is used to protect your backups; for
-- example,
-- @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
recoveryPointByBackupVault_encryptionKeyArn :: Lens.Lens' RecoveryPointByBackupVault (Prelude.Maybe Prelude.Text)
recoveryPointByBackupVault_encryptionKeyArn = Lens.lens (\RecoveryPointByBackupVault' {encryptionKeyArn} -> encryptionKeyArn) (\s@RecoveryPointByBackupVault' {} a -> s {encryptionKeyArn = a} :: RecoveryPointByBackupVault)

-- | The type of Amazon Web Services resource saved as a recovery point; for
-- example, an Amazon Elastic Block Store (Amazon EBS) volume or an Amazon
-- Relational Database Service (Amazon RDS) database. For Windows Volume
-- Shadow Copy Service (VSS) backups, the only supported resource type is
-- Amazon EC2.
recoveryPointByBackupVault_resourceType :: Lens.Lens' RecoveryPointByBackupVault (Prelude.Maybe Prelude.Text)
recoveryPointByBackupVault_resourceType = Lens.lens (\RecoveryPointByBackupVault' {resourceType} -> resourceType) (\s@RecoveryPointByBackupVault' {} a -> s {resourceType = a} :: RecoveryPointByBackupVault)

-- | The lifecycle defines when a protected resource is transitioned to cold
-- storage and when it expires. Backup transitions and expires backups
-- automatically according to the lifecycle that you define.
--
-- Backups transitioned to cold storage must be stored in cold storage for
-- a minimum of 90 days. Therefore, the “retention” setting must be 90 days
-- greater than the “transition to cold after days” setting. The
-- “transition to cold after days” setting cannot be changed after a backup
-- has been transitioned to cold.
--
-- Resource types that are able to be transitioned to cold storage are
-- listed in the \"Lifecycle to cold storage\" section of the
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/whatisbackup.html#features-by-resource Feature availability by resource>
-- table. Backup ignores this expression for other resource types.
recoveryPointByBackupVault_lifecycle :: Lens.Lens' RecoveryPointByBackupVault (Prelude.Maybe Lifecycle)
recoveryPointByBackupVault_lifecycle = Lens.lens (\RecoveryPointByBackupVault' {lifecycle} -> lifecycle) (\s@RecoveryPointByBackupVault' {} a -> s {lifecycle = a} :: RecoveryPointByBackupVault)

-- | An Amazon Resource Name (ARN) that uniquely identifies a recovery point;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
recoveryPointByBackupVault_recoveryPointArn :: Lens.Lens' RecoveryPointByBackupVault (Prelude.Maybe Prelude.Text)
recoveryPointByBackupVault_recoveryPointArn = Lens.lens (\RecoveryPointByBackupVault' {recoveryPointArn} -> recoveryPointArn) (\s@RecoveryPointByBackupVault' {} a -> s {recoveryPointArn = a} :: RecoveryPointByBackupVault)

-- | The date and time a job to restore a recovery point is completed, in
-- Unix format and Coordinated Universal Time (UTC). The value of
-- @CompletionDate@ is accurate to milliseconds. For example, the value
-- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
recoveryPointByBackupVault_completionDate :: Lens.Lens' RecoveryPointByBackupVault (Prelude.Maybe Prelude.UTCTime)
recoveryPointByBackupVault_completionDate = Lens.lens (\RecoveryPointByBackupVault' {completionDate} -> completionDate) (\s@RecoveryPointByBackupVault' {} a -> s {completionDate = a} :: RecoveryPointByBackupVault) Prelude.. Lens.mapping Core._Time

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
recoveryPointByBackupVault_backupVaultName :: Lens.Lens' RecoveryPointByBackupVault (Prelude.Maybe Prelude.Text)
recoveryPointByBackupVault_backupVaultName = Lens.lens (\RecoveryPointByBackupVault' {backupVaultName} -> backupVaultName) (\s@RecoveryPointByBackupVault' {} a -> s {backupVaultName = a} :: RecoveryPointByBackupVault)

-- | The date and time a recovery point is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
recoveryPointByBackupVault_creationDate :: Lens.Lens' RecoveryPointByBackupVault (Prelude.Maybe Prelude.UTCTime)
recoveryPointByBackupVault_creationDate = Lens.lens (\RecoveryPointByBackupVault' {creationDate} -> creationDate) (\s@RecoveryPointByBackupVault' {} a -> s {creationDate = a} :: RecoveryPointByBackupVault) Prelude.. Lens.mapping Core._Time

-- | The size, in bytes, of a backup.
recoveryPointByBackupVault_backupSizeInBytes :: Lens.Lens' RecoveryPointByBackupVault (Prelude.Maybe Prelude.Integer)
recoveryPointByBackupVault_backupSizeInBytes = Lens.lens (\RecoveryPointByBackupVault' {backupSizeInBytes} -> backupSizeInBytes) (\s@RecoveryPointByBackupVault' {} a -> s {backupSizeInBytes = a} :: RecoveryPointByBackupVault)

-- | A status code specifying the state of the recovery point.
recoveryPointByBackupVault_status :: Lens.Lens' RecoveryPointByBackupVault (Prelude.Maybe RecoveryPointStatus)
recoveryPointByBackupVault_status = Lens.lens (\RecoveryPointByBackupVault' {status} -> status) (\s@RecoveryPointByBackupVault' {} a -> s {status = a} :: RecoveryPointByBackupVault)

-- | An ARN that uniquely identifies a backup vault; for example,
-- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
recoveryPointByBackupVault_backupVaultArn :: Lens.Lens' RecoveryPointByBackupVault (Prelude.Maybe Prelude.Text)
recoveryPointByBackupVault_backupVaultArn = Lens.lens (\RecoveryPointByBackupVault' {backupVaultArn} -> backupVaultArn) (\s@RecoveryPointByBackupVault' {} a -> s {backupVaultArn = a} :: RecoveryPointByBackupVault)

-- | A Boolean value that is returned as @TRUE@ if the specified recovery
-- point is encrypted, or @FALSE@ if the recovery point is not encrypted.
recoveryPointByBackupVault_isEncrypted :: Lens.Lens' RecoveryPointByBackupVault (Prelude.Maybe Prelude.Bool)
recoveryPointByBackupVault_isEncrypted = Lens.lens (\RecoveryPointByBackupVault' {isEncrypted} -> isEncrypted) (\s@RecoveryPointByBackupVault' {} a -> s {isEncrypted = a} :: RecoveryPointByBackupVault)

-- | Specifies the IAM role ARN used to create the target recovery point; for
-- example, @arn:aws:iam::123456789012:role\/S3Access@.
recoveryPointByBackupVault_iamRoleArn :: Lens.Lens' RecoveryPointByBackupVault (Prelude.Maybe Prelude.Text)
recoveryPointByBackupVault_iamRoleArn = Lens.lens (\RecoveryPointByBackupVault' {iamRoleArn} -> iamRoleArn) (\s@RecoveryPointByBackupVault' {} a -> s {iamRoleArn = a} :: RecoveryPointByBackupVault)

-- | The backup vault where the recovery point was originally copied from. If
-- the recovery point is restored to the same account this value will be
-- @null@.
recoveryPointByBackupVault_sourceBackupVaultArn :: Lens.Lens' RecoveryPointByBackupVault (Prelude.Maybe Prelude.Text)
recoveryPointByBackupVault_sourceBackupVaultArn = Lens.lens (\RecoveryPointByBackupVault' {sourceBackupVaultArn} -> sourceBackupVaultArn) (\s@RecoveryPointByBackupVault' {} a -> s {sourceBackupVaultArn = a} :: RecoveryPointByBackupVault)

-- | An ARN that uniquely identifies a resource. The format of the ARN
-- depends on the resource type.
recoveryPointByBackupVault_resourceArn :: Lens.Lens' RecoveryPointByBackupVault (Prelude.Maybe Prelude.Text)
recoveryPointByBackupVault_resourceArn = Lens.lens (\RecoveryPointByBackupVault' {resourceArn} -> resourceArn) (\s@RecoveryPointByBackupVault' {} a -> s {resourceArn = a} :: RecoveryPointByBackupVault)

-- | A message explaining the reason of the recovery point deletion failure.
recoveryPointByBackupVault_statusMessage :: Lens.Lens' RecoveryPointByBackupVault (Prelude.Maybe Prelude.Text)
recoveryPointByBackupVault_statusMessage = Lens.lens (\RecoveryPointByBackupVault' {statusMessage} -> statusMessage) (\s@RecoveryPointByBackupVault' {} a -> s {statusMessage = a} :: RecoveryPointByBackupVault)

-- | Contains identifying information about the creation of a recovery point,
-- including the @BackupPlanArn@, @BackupPlanId@, @BackupPlanVersion@, and
-- @BackupRuleId@ of the backup plan that is used to create it.
recoveryPointByBackupVault_createdBy :: Lens.Lens' RecoveryPointByBackupVault (Prelude.Maybe RecoveryPointCreator)
recoveryPointByBackupVault_createdBy = Lens.lens (\RecoveryPointByBackupVault' {createdBy} -> createdBy) (\s@RecoveryPointByBackupVault' {} a -> s {createdBy = a} :: RecoveryPointByBackupVault)

-- | The date and time a recovery point was last restored, in Unix format and
-- Coordinated Universal Time (UTC). The value of @LastRestoreTime@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
recoveryPointByBackupVault_lastRestoreTime :: Lens.Lens' RecoveryPointByBackupVault (Prelude.Maybe Prelude.UTCTime)
recoveryPointByBackupVault_lastRestoreTime = Lens.lens (\RecoveryPointByBackupVault' {lastRestoreTime} -> lastRestoreTime) (\s@RecoveryPointByBackupVault' {} a -> s {lastRestoreTime = a} :: RecoveryPointByBackupVault) Prelude.. Lens.mapping Core._Time

-- | A @CalculatedLifecycle@ object containing @DeleteAt@ and
-- @MoveToColdStorageAt@ timestamps.
recoveryPointByBackupVault_calculatedLifecycle :: Lens.Lens' RecoveryPointByBackupVault (Prelude.Maybe CalculatedLifecycle)
recoveryPointByBackupVault_calculatedLifecycle = Lens.lens (\RecoveryPointByBackupVault' {calculatedLifecycle} -> calculatedLifecycle) (\s@RecoveryPointByBackupVault' {} a -> s {calculatedLifecycle = a} :: RecoveryPointByBackupVault)

instance Core.FromJSON RecoveryPointByBackupVault where
  parseJSON =
    Core.withObject
      "RecoveryPointByBackupVault"
      ( \x ->
          RecoveryPointByBackupVault'
            Prelude.<$> (x Core..:? "EncryptionKeyArn")
            Prelude.<*> (x Core..:? "ResourceType")
            Prelude.<*> (x Core..:? "Lifecycle")
            Prelude.<*> (x Core..:? "RecoveryPointArn")
            Prelude.<*> (x Core..:? "CompletionDate")
            Prelude.<*> (x Core..:? "BackupVaultName")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "BackupSizeInBytes")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "BackupVaultArn")
            Prelude.<*> (x Core..:? "IsEncrypted")
            Prelude.<*> (x Core..:? "IamRoleArn")
            Prelude.<*> (x Core..:? "SourceBackupVaultArn")
            Prelude.<*> (x Core..:? "ResourceArn")
            Prelude.<*> (x Core..:? "StatusMessage")
            Prelude.<*> (x Core..:? "CreatedBy")
            Prelude.<*> (x Core..:? "LastRestoreTime")
            Prelude.<*> (x Core..:? "CalculatedLifecycle")
      )

instance Prelude.Hashable RecoveryPointByBackupVault where
  hashWithSalt _salt RecoveryPointByBackupVault' {..} =
    _salt `Prelude.hashWithSalt` encryptionKeyArn
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` lifecycle
      `Prelude.hashWithSalt` recoveryPointArn
      `Prelude.hashWithSalt` completionDate
      `Prelude.hashWithSalt` backupVaultName
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` backupSizeInBytes
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` backupVaultArn
      `Prelude.hashWithSalt` isEncrypted
      `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` sourceBackupVaultArn
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` lastRestoreTime
      `Prelude.hashWithSalt` calculatedLifecycle

instance Prelude.NFData RecoveryPointByBackupVault where
  rnf RecoveryPointByBackupVault' {..} =
    Prelude.rnf encryptionKeyArn
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf recoveryPointArn
      `Prelude.seq` Prelude.rnf completionDate
      `Prelude.seq` Prelude.rnf backupVaultName
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf backupSizeInBytes
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf backupVaultArn
      `Prelude.seq` Prelude.rnf isEncrypted
      `Prelude.seq` Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf sourceBackupVaultArn
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf lastRestoreTime
      `Prelude.seq` Prelude.rnf calculatedLifecycle
