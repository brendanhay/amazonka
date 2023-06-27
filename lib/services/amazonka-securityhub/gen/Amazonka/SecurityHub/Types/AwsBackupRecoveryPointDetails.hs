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
-- Module      : Amazonka.SecurityHub.Types.AwsBackupRecoveryPointDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsBackupRecoveryPointDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsBackupRecoveryPointCalculatedLifecycleDetails
import Amazonka.SecurityHub.Types.AwsBackupRecoveryPointCreatedByDetails
import Amazonka.SecurityHub.Types.AwsBackupRecoveryPointLifecycleDetails

-- | Contains detailed information about the recovery points stored in an
-- Backup backup vault. A backup, or recovery point, represents the content
-- of a resource at a specified time.
--
-- /See:/ 'newAwsBackupRecoveryPointDetails' smart constructor.
data AwsBackupRecoveryPointDetails = AwsBackupRecoveryPointDetails'
  { -- | The size, in bytes, of a backup.
    backupSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault.
    backupVaultArn :: Prelude.Maybe Prelude.Text,
    -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the Amazon Web Services
    -- account used to create them and the Amazon Web Services Region where
    -- they are created. They consist of lowercase letters, numbers, and
    -- hyphens.
    backupVaultName :: Prelude.Maybe Prelude.Text,
    -- | A @CalculatedLifecycle@ object containing @DeleteAt@ and
    -- @MoveToColdStorageAt@ timestamps.
    calculatedLifecycle :: Prelude.Maybe AwsBackupRecoveryPointCalculatedLifecycleDetails,
    -- | The date and time that a job to create a recovery point is completed, in
    -- Unix format and UTC. The value of @CompletionDate@ is accurate to
    -- milliseconds. For example, the value 1516925490.087 represents Friday,
    -- January 26, 2018 12:11:30.087 AM.
    completionDate :: Prelude.Maybe Prelude.Text,
    -- | Contains identifying information about the creation of a recovery point,
    -- including the @BackupPlanArn@, @BackupPlanId@, @BackupPlanVersion@, and
    -- @BackupRuleId@ of the backup plan that is used to create it.
    createdBy :: Prelude.Maybe AwsBackupRecoveryPointCreatedByDetails,
    -- | The date and time a recovery point is created, in Unix format and UTC.
    -- The value of @CreationDate@ is accurate to milliseconds. For example,
    -- the value 1516925490.087 represents Friday, January 26, 2018
    -- 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the server-side encryption key that is used to protect your
    -- backups.
    encryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the IAM role ARN used to create the target recovery point
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value that is returned as @TRUE@ if the specified recovery
    -- point is encrypted, or @FALSE@ if the recovery point is not encrypted.
    isEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | The date and time that a recovery point was last restored, in Unix
    -- format and UTC. The value of @LastRestoreTime@ is accurate to
    -- milliseconds. For example, the value 1516925490.087 represents Friday,
    -- January 26, 2018 12:11:30.087 AM.
    lastRestoreTime :: Prelude.Maybe Prelude.Text,
    -- | The lifecycle defines when a protected resource is transitioned to cold
    -- storage and when it expires. Backup transitions and expires backups
    -- automatically according to the lifecycle that you define
    lifecycle :: Prelude.Maybe AwsBackupRecoveryPointLifecycleDetails,
    -- | An ARN that uniquely identifies a recovery point.
    recoveryPointArn :: Prelude.Maybe Prelude.Text,
    -- | An ARN that uniquely identifies a resource. The format of the ARN
    -- depends on the resource type.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The type of Amazon Web Services resource saved as a recovery point, such
    -- as an Amazon EBS volume or an Amazon RDS database.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the backup vault where the recovery point was originally
    -- copied from. If the recovery point is restored to the same account, this
    -- value will be null.
    sourceBackupVaultArn :: Prelude.Maybe Prelude.Text,
    -- | A status code specifying the state of the recovery point. Valid values
    -- are as follows:
    --
    -- -   @COMPLETED@
    --
    -- -   @DELETING@
    --
    -- -   @EXPIRED@
    --
    -- -   @PARTIAL@
    status :: Prelude.Maybe Prelude.Text,
    -- | A message explaining the reason of the recovery point deletion failure.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | Specifies the storage class of the recovery point. Valid values are as
    -- follows:
    --
    -- -   @COLD@
    --
    -- -   @DELETED@
    --
    -- -   @WARM@
    storageClass :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsBackupRecoveryPointDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupSizeInBytes', 'awsBackupRecoveryPointDetails_backupSizeInBytes' - The size, in bytes, of a backup.
--
-- 'backupVaultArn', 'awsBackupRecoveryPointDetails_backupVaultArn' - An Amazon Resource Name (ARN) that uniquely identifies a backup vault.
--
-- 'backupVaultName', 'awsBackupRecoveryPointDetails_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the Amazon Web Services
-- account used to create them and the Amazon Web Services Region where
-- they are created. They consist of lowercase letters, numbers, and
-- hyphens.
--
-- 'calculatedLifecycle', 'awsBackupRecoveryPointDetails_calculatedLifecycle' - A @CalculatedLifecycle@ object containing @DeleteAt@ and
-- @MoveToColdStorageAt@ timestamps.
--
-- 'completionDate', 'awsBackupRecoveryPointDetails_completionDate' - The date and time that a job to create a recovery point is completed, in
-- Unix format and UTC. The value of @CompletionDate@ is accurate to
-- milliseconds. For example, the value 1516925490.087 represents Friday,
-- January 26, 2018 12:11:30.087 AM.
--
-- 'createdBy', 'awsBackupRecoveryPointDetails_createdBy' - Contains identifying information about the creation of a recovery point,
-- including the @BackupPlanArn@, @BackupPlanId@, @BackupPlanVersion@, and
-- @BackupRuleId@ of the backup plan that is used to create it.
--
-- 'creationDate', 'awsBackupRecoveryPointDetails_creationDate' - The date and time a recovery point is created, in Unix format and UTC.
-- The value of @CreationDate@ is accurate to milliseconds. For example,
-- the value 1516925490.087 represents Friday, January 26, 2018
-- 12:11:30.087 AM.
--
-- 'encryptionKeyArn', 'awsBackupRecoveryPointDetails_encryptionKeyArn' - The ARN for the server-side encryption key that is used to protect your
-- backups.
--
-- 'iamRoleArn', 'awsBackupRecoveryPointDetails_iamRoleArn' - Specifies the IAM role ARN used to create the target recovery point
--
-- 'isEncrypted', 'awsBackupRecoveryPointDetails_isEncrypted' - A Boolean value that is returned as @TRUE@ if the specified recovery
-- point is encrypted, or @FALSE@ if the recovery point is not encrypted.
--
-- 'lastRestoreTime', 'awsBackupRecoveryPointDetails_lastRestoreTime' - The date and time that a recovery point was last restored, in Unix
-- format and UTC. The value of @LastRestoreTime@ is accurate to
-- milliseconds. For example, the value 1516925490.087 represents Friday,
-- January 26, 2018 12:11:30.087 AM.
--
-- 'lifecycle', 'awsBackupRecoveryPointDetails_lifecycle' - The lifecycle defines when a protected resource is transitioned to cold
-- storage and when it expires. Backup transitions and expires backups
-- automatically according to the lifecycle that you define
--
-- 'recoveryPointArn', 'awsBackupRecoveryPointDetails_recoveryPointArn' - An ARN that uniquely identifies a recovery point.
--
-- 'resourceArn', 'awsBackupRecoveryPointDetails_resourceArn' - An ARN that uniquely identifies a resource. The format of the ARN
-- depends on the resource type.
--
-- 'resourceType', 'awsBackupRecoveryPointDetails_resourceType' - The type of Amazon Web Services resource saved as a recovery point, such
-- as an Amazon EBS volume or an Amazon RDS database.
--
-- 'sourceBackupVaultArn', 'awsBackupRecoveryPointDetails_sourceBackupVaultArn' - The ARN for the backup vault where the recovery point was originally
-- copied from. If the recovery point is restored to the same account, this
-- value will be null.
--
-- 'status', 'awsBackupRecoveryPointDetails_status' - A status code specifying the state of the recovery point. Valid values
-- are as follows:
--
-- -   @COMPLETED@
--
-- -   @DELETING@
--
-- -   @EXPIRED@
--
-- -   @PARTIAL@
--
-- 'statusMessage', 'awsBackupRecoveryPointDetails_statusMessage' - A message explaining the reason of the recovery point deletion failure.
--
-- 'storageClass', 'awsBackupRecoveryPointDetails_storageClass' - Specifies the storage class of the recovery point. Valid values are as
-- follows:
--
-- -   @COLD@
--
-- -   @DELETED@
--
-- -   @WARM@
newAwsBackupRecoveryPointDetails ::
  AwsBackupRecoveryPointDetails
newAwsBackupRecoveryPointDetails =
  AwsBackupRecoveryPointDetails'
    { backupSizeInBytes =
        Prelude.Nothing,
      backupVaultArn = Prelude.Nothing,
      backupVaultName = Prelude.Nothing,
      calculatedLifecycle = Prelude.Nothing,
      completionDate = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      encryptionKeyArn = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      isEncrypted = Prelude.Nothing,
      lastRestoreTime = Prelude.Nothing,
      lifecycle = Prelude.Nothing,
      recoveryPointArn = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      sourceBackupVaultArn = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      storageClass = Prelude.Nothing
    }

-- | The size, in bytes, of a backup.
awsBackupRecoveryPointDetails_backupSizeInBytes :: Lens.Lens' AwsBackupRecoveryPointDetails (Prelude.Maybe Prelude.Integer)
awsBackupRecoveryPointDetails_backupSizeInBytes = Lens.lens (\AwsBackupRecoveryPointDetails' {backupSizeInBytes} -> backupSizeInBytes) (\s@AwsBackupRecoveryPointDetails' {} a -> s {backupSizeInBytes = a} :: AwsBackupRecoveryPointDetails)

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault.
awsBackupRecoveryPointDetails_backupVaultArn :: Lens.Lens' AwsBackupRecoveryPointDetails (Prelude.Maybe Prelude.Text)
awsBackupRecoveryPointDetails_backupVaultArn = Lens.lens (\AwsBackupRecoveryPointDetails' {backupVaultArn} -> backupVaultArn) (\s@AwsBackupRecoveryPointDetails' {} a -> s {backupVaultArn = a} :: AwsBackupRecoveryPointDetails)

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the Amazon Web Services
-- account used to create them and the Amazon Web Services Region where
-- they are created. They consist of lowercase letters, numbers, and
-- hyphens.
awsBackupRecoveryPointDetails_backupVaultName :: Lens.Lens' AwsBackupRecoveryPointDetails (Prelude.Maybe Prelude.Text)
awsBackupRecoveryPointDetails_backupVaultName = Lens.lens (\AwsBackupRecoveryPointDetails' {backupVaultName} -> backupVaultName) (\s@AwsBackupRecoveryPointDetails' {} a -> s {backupVaultName = a} :: AwsBackupRecoveryPointDetails)

-- | A @CalculatedLifecycle@ object containing @DeleteAt@ and
-- @MoveToColdStorageAt@ timestamps.
awsBackupRecoveryPointDetails_calculatedLifecycle :: Lens.Lens' AwsBackupRecoveryPointDetails (Prelude.Maybe AwsBackupRecoveryPointCalculatedLifecycleDetails)
awsBackupRecoveryPointDetails_calculatedLifecycle = Lens.lens (\AwsBackupRecoveryPointDetails' {calculatedLifecycle} -> calculatedLifecycle) (\s@AwsBackupRecoveryPointDetails' {} a -> s {calculatedLifecycle = a} :: AwsBackupRecoveryPointDetails)

-- | The date and time that a job to create a recovery point is completed, in
-- Unix format and UTC. The value of @CompletionDate@ is accurate to
-- milliseconds. For example, the value 1516925490.087 represents Friday,
-- January 26, 2018 12:11:30.087 AM.
awsBackupRecoveryPointDetails_completionDate :: Lens.Lens' AwsBackupRecoveryPointDetails (Prelude.Maybe Prelude.Text)
awsBackupRecoveryPointDetails_completionDate = Lens.lens (\AwsBackupRecoveryPointDetails' {completionDate} -> completionDate) (\s@AwsBackupRecoveryPointDetails' {} a -> s {completionDate = a} :: AwsBackupRecoveryPointDetails)

-- | Contains identifying information about the creation of a recovery point,
-- including the @BackupPlanArn@, @BackupPlanId@, @BackupPlanVersion@, and
-- @BackupRuleId@ of the backup plan that is used to create it.
awsBackupRecoveryPointDetails_createdBy :: Lens.Lens' AwsBackupRecoveryPointDetails (Prelude.Maybe AwsBackupRecoveryPointCreatedByDetails)
awsBackupRecoveryPointDetails_createdBy = Lens.lens (\AwsBackupRecoveryPointDetails' {createdBy} -> createdBy) (\s@AwsBackupRecoveryPointDetails' {} a -> s {createdBy = a} :: AwsBackupRecoveryPointDetails)

-- | The date and time a recovery point is created, in Unix format and UTC.
-- The value of @CreationDate@ is accurate to milliseconds. For example,
-- the value 1516925490.087 represents Friday, January 26, 2018
-- 12:11:30.087 AM.
awsBackupRecoveryPointDetails_creationDate :: Lens.Lens' AwsBackupRecoveryPointDetails (Prelude.Maybe Prelude.Text)
awsBackupRecoveryPointDetails_creationDate = Lens.lens (\AwsBackupRecoveryPointDetails' {creationDate} -> creationDate) (\s@AwsBackupRecoveryPointDetails' {} a -> s {creationDate = a} :: AwsBackupRecoveryPointDetails)

-- | The ARN for the server-side encryption key that is used to protect your
-- backups.
awsBackupRecoveryPointDetails_encryptionKeyArn :: Lens.Lens' AwsBackupRecoveryPointDetails (Prelude.Maybe Prelude.Text)
awsBackupRecoveryPointDetails_encryptionKeyArn = Lens.lens (\AwsBackupRecoveryPointDetails' {encryptionKeyArn} -> encryptionKeyArn) (\s@AwsBackupRecoveryPointDetails' {} a -> s {encryptionKeyArn = a} :: AwsBackupRecoveryPointDetails)

-- | Specifies the IAM role ARN used to create the target recovery point
awsBackupRecoveryPointDetails_iamRoleArn :: Lens.Lens' AwsBackupRecoveryPointDetails (Prelude.Maybe Prelude.Text)
awsBackupRecoveryPointDetails_iamRoleArn = Lens.lens (\AwsBackupRecoveryPointDetails' {iamRoleArn} -> iamRoleArn) (\s@AwsBackupRecoveryPointDetails' {} a -> s {iamRoleArn = a} :: AwsBackupRecoveryPointDetails)

-- | A Boolean value that is returned as @TRUE@ if the specified recovery
-- point is encrypted, or @FALSE@ if the recovery point is not encrypted.
awsBackupRecoveryPointDetails_isEncrypted :: Lens.Lens' AwsBackupRecoveryPointDetails (Prelude.Maybe Prelude.Bool)
awsBackupRecoveryPointDetails_isEncrypted = Lens.lens (\AwsBackupRecoveryPointDetails' {isEncrypted} -> isEncrypted) (\s@AwsBackupRecoveryPointDetails' {} a -> s {isEncrypted = a} :: AwsBackupRecoveryPointDetails)

-- | The date and time that a recovery point was last restored, in Unix
-- format and UTC. The value of @LastRestoreTime@ is accurate to
-- milliseconds. For example, the value 1516925490.087 represents Friday,
-- January 26, 2018 12:11:30.087 AM.
awsBackupRecoveryPointDetails_lastRestoreTime :: Lens.Lens' AwsBackupRecoveryPointDetails (Prelude.Maybe Prelude.Text)
awsBackupRecoveryPointDetails_lastRestoreTime = Lens.lens (\AwsBackupRecoveryPointDetails' {lastRestoreTime} -> lastRestoreTime) (\s@AwsBackupRecoveryPointDetails' {} a -> s {lastRestoreTime = a} :: AwsBackupRecoveryPointDetails)

-- | The lifecycle defines when a protected resource is transitioned to cold
-- storage and when it expires. Backup transitions and expires backups
-- automatically according to the lifecycle that you define
awsBackupRecoveryPointDetails_lifecycle :: Lens.Lens' AwsBackupRecoveryPointDetails (Prelude.Maybe AwsBackupRecoveryPointLifecycleDetails)
awsBackupRecoveryPointDetails_lifecycle = Lens.lens (\AwsBackupRecoveryPointDetails' {lifecycle} -> lifecycle) (\s@AwsBackupRecoveryPointDetails' {} a -> s {lifecycle = a} :: AwsBackupRecoveryPointDetails)

-- | An ARN that uniquely identifies a recovery point.
awsBackupRecoveryPointDetails_recoveryPointArn :: Lens.Lens' AwsBackupRecoveryPointDetails (Prelude.Maybe Prelude.Text)
awsBackupRecoveryPointDetails_recoveryPointArn = Lens.lens (\AwsBackupRecoveryPointDetails' {recoveryPointArn} -> recoveryPointArn) (\s@AwsBackupRecoveryPointDetails' {} a -> s {recoveryPointArn = a} :: AwsBackupRecoveryPointDetails)

-- | An ARN that uniquely identifies a resource. The format of the ARN
-- depends on the resource type.
awsBackupRecoveryPointDetails_resourceArn :: Lens.Lens' AwsBackupRecoveryPointDetails (Prelude.Maybe Prelude.Text)
awsBackupRecoveryPointDetails_resourceArn = Lens.lens (\AwsBackupRecoveryPointDetails' {resourceArn} -> resourceArn) (\s@AwsBackupRecoveryPointDetails' {} a -> s {resourceArn = a} :: AwsBackupRecoveryPointDetails)

-- | The type of Amazon Web Services resource saved as a recovery point, such
-- as an Amazon EBS volume or an Amazon RDS database.
awsBackupRecoveryPointDetails_resourceType :: Lens.Lens' AwsBackupRecoveryPointDetails (Prelude.Maybe Prelude.Text)
awsBackupRecoveryPointDetails_resourceType = Lens.lens (\AwsBackupRecoveryPointDetails' {resourceType} -> resourceType) (\s@AwsBackupRecoveryPointDetails' {} a -> s {resourceType = a} :: AwsBackupRecoveryPointDetails)

-- | The ARN for the backup vault where the recovery point was originally
-- copied from. If the recovery point is restored to the same account, this
-- value will be null.
awsBackupRecoveryPointDetails_sourceBackupVaultArn :: Lens.Lens' AwsBackupRecoveryPointDetails (Prelude.Maybe Prelude.Text)
awsBackupRecoveryPointDetails_sourceBackupVaultArn = Lens.lens (\AwsBackupRecoveryPointDetails' {sourceBackupVaultArn} -> sourceBackupVaultArn) (\s@AwsBackupRecoveryPointDetails' {} a -> s {sourceBackupVaultArn = a} :: AwsBackupRecoveryPointDetails)

-- | A status code specifying the state of the recovery point. Valid values
-- are as follows:
--
-- -   @COMPLETED@
--
-- -   @DELETING@
--
-- -   @EXPIRED@
--
-- -   @PARTIAL@
awsBackupRecoveryPointDetails_status :: Lens.Lens' AwsBackupRecoveryPointDetails (Prelude.Maybe Prelude.Text)
awsBackupRecoveryPointDetails_status = Lens.lens (\AwsBackupRecoveryPointDetails' {status} -> status) (\s@AwsBackupRecoveryPointDetails' {} a -> s {status = a} :: AwsBackupRecoveryPointDetails)

-- | A message explaining the reason of the recovery point deletion failure.
awsBackupRecoveryPointDetails_statusMessage :: Lens.Lens' AwsBackupRecoveryPointDetails (Prelude.Maybe Prelude.Text)
awsBackupRecoveryPointDetails_statusMessage = Lens.lens (\AwsBackupRecoveryPointDetails' {statusMessage} -> statusMessage) (\s@AwsBackupRecoveryPointDetails' {} a -> s {statusMessage = a} :: AwsBackupRecoveryPointDetails)

-- | Specifies the storage class of the recovery point. Valid values are as
-- follows:
--
-- -   @COLD@
--
-- -   @DELETED@
--
-- -   @WARM@
awsBackupRecoveryPointDetails_storageClass :: Lens.Lens' AwsBackupRecoveryPointDetails (Prelude.Maybe Prelude.Text)
awsBackupRecoveryPointDetails_storageClass = Lens.lens (\AwsBackupRecoveryPointDetails' {storageClass} -> storageClass) (\s@AwsBackupRecoveryPointDetails' {} a -> s {storageClass = a} :: AwsBackupRecoveryPointDetails)

instance Data.FromJSON AwsBackupRecoveryPointDetails where
  parseJSON =
    Data.withObject
      "AwsBackupRecoveryPointDetails"
      ( \x ->
          AwsBackupRecoveryPointDetails'
            Prelude.<$> (x Data..:? "BackupSizeInBytes")
            Prelude.<*> (x Data..:? "BackupVaultArn")
            Prelude.<*> (x Data..:? "BackupVaultName")
            Prelude.<*> (x Data..:? "CalculatedLifecycle")
            Prelude.<*> (x Data..:? "CompletionDate")
            Prelude.<*> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "EncryptionKeyArn")
            Prelude.<*> (x Data..:? "IamRoleArn")
            Prelude.<*> (x Data..:? "IsEncrypted")
            Prelude.<*> (x Data..:? "LastRestoreTime")
            Prelude.<*> (x Data..:? "Lifecycle")
            Prelude.<*> (x Data..:? "RecoveryPointArn")
            Prelude.<*> (x Data..:? "ResourceArn")
            Prelude.<*> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "SourceBackupVaultArn")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusMessage")
            Prelude.<*> (x Data..:? "StorageClass")
      )

instance
  Prelude.Hashable
    AwsBackupRecoveryPointDetails
  where
  hashWithSalt _salt AwsBackupRecoveryPointDetails' {..} =
    _salt
      `Prelude.hashWithSalt` backupSizeInBytes
      `Prelude.hashWithSalt` backupVaultArn
      `Prelude.hashWithSalt` backupVaultName
      `Prelude.hashWithSalt` calculatedLifecycle
      `Prelude.hashWithSalt` completionDate
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` encryptionKeyArn
      `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` isEncrypted
      `Prelude.hashWithSalt` lastRestoreTime
      `Prelude.hashWithSalt` lifecycle
      `Prelude.hashWithSalt` recoveryPointArn
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` sourceBackupVaultArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` storageClass

instance Prelude.NFData AwsBackupRecoveryPointDetails where
  rnf AwsBackupRecoveryPointDetails' {..} =
    Prelude.rnf backupSizeInBytes
      `Prelude.seq` Prelude.rnf backupVaultArn
      `Prelude.seq` Prelude.rnf backupVaultName
      `Prelude.seq` Prelude.rnf calculatedLifecycle
      `Prelude.seq` Prelude.rnf completionDate
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf encryptionKeyArn
      `Prelude.seq` Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf isEncrypted
      `Prelude.seq` Prelude.rnf lastRestoreTime
      `Prelude.seq` Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf recoveryPointArn
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf sourceBackupVaultArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf storageClass

instance Data.ToJSON AwsBackupRecoveryPointDetails where
  toJSON AwsBackupRecoveryPointDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BackupSizeInBytes" Data..=)
              Prelude.<$> backupSizeInBytes,
            ("BackupVaultArn" Data..=)
              Prelude.<$> backupVaultArn,
            ("BackupVaultName" Data..=)
              Prelude.<$> backupVaultName,
            ("CalculatedLifecycle" Data..=)
              Prelude.<$> calculatedLifecycle,
            ("CompletionDate" Data..=)
              Prelude.<$> completionDate,
            ("CreatedBy" Data..=) Prelude.<$> createdBy,
            ("CreationDate" Data..=) Prelude.<$> creationDate,
            ("EncryptionKeyArn" Data..=)
              Prelude.<$> encryptionKeyArn,
            ("IamRoleArn" Data..=) Prelude.<$> iamRoleArn,
            ("IsEncrypted" Data..=) Prelude.<$> isEncrypted,
            ("LastRestoreTime" Data..=)
              Prelude.<$> lastRestoreTime,
            ("Lifecycle" Data..=) Prelude.<$> lifecycle,
            ("RecoveryPointArn" Data..=)
              Prelude.<$> recoveryPointArn,
            ("ResourceArn" Data..=) Prelude.<$> resourceArn,
            ("ResourceType" Data..=) Prelude.<$> resourceType,
            ("SourceBackupVaultArn" Data..=)
              Prelude.<$> sourceBackupVaultArn,
            ("Status" Data..=) Prelude.<$> status,
            ("StatusMessage" Data..=) Prelude.<$> statusMessage,
            ("StorageClass" Data..=) Prelude.<$> storageClass
          ]
      )
