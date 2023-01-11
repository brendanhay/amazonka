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
-- Module      : Amazonka.Backup.Types.BackupJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.BackupJob where

import Amazonka.Backup.Types.BackupJobState
import Amazonka.Backup.Types.RecoveryPointCreator
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains detailed information about a backup job.
--
-- /See:/ 'newBackupJob' smart constructor.
data BackupJob = BackupJob'
  { -- | The account ID that owns the backup job.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Uniquely identifies a request to Backup to back up a resource.
    backupJobId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the backup option for a selected resource. This option is only
    -- available for Windows Volume Shadow Copy Service (VSS) backup jobs.
    --
    -- Valid values: Set to @\"WindowsVSS\":\"enabled\"@ to enable the
    -- @WindowsVSS@ backup option and create a Windows VSS backup. Set to
    -- @\"WindowsVSS\":\"disabled\"@ to create a regular backup. If you specify
    -- an invalid option, you get an @InvalidParameterValueException@
    -- exception.
    backupOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The size, in bytes, of a backup.
    backupSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | Represents the type of backup for a backup job.
    backupType :: Prelude.Maybe Prelude.Text,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault;
    -- for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
    backupVaultArn :: Prelude.Maybe Prelude.Text,
    -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Amazon Web Services Region where they are created. They
    -- consist of lowercase letters, numbers, and hyphens.
    backupVaultName :: Prelude.Maybe Prelude.Text,
    -- | The size in bytes transferred to a backup vault at the time that the job
    -- status was queried.
    bytesTransferred :: Prelude.Maybe Prelude.Integer,
    -- | The date and time a job to create a backup job is completed, in Unix
    -- format and Coordinated Universal Time (UTC). The value of
    -- @CompletionDate@ is accurate to milliseconds. For example, the value
    -- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
    completionDate :: Prelude.Maybe Data.POSIX,
    -- | Contains identifying information about the creation of a backup job,
    -- including the @BackupPlanArn@, @BackupPlanId@, @BackupPlanVersion@, and
    -- @BackupRuleId@ of the backup plan used to create it.
    createdBy :: Prelude.Maybe RecoveryPointCreator,
    -- | The date and time a backup job is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The date and time a job to back up resources is expected to be
    -- completed, in Unix format and Coordinated Universal Time (UTC). The
    -- value of @ExpectedCompletionDate@ is accurate to milliseconds. For
    -- example, the value 1516925490.087 represents Friday, January 26, 2018
    -- 12:11:30.087 AM.
    expectedCompletionDate :: Prelude.Maybe Data.POSIX,
    -- | Specifies the IAM role ARN used to create the target recovery point. IAM
    -- roles other than the default role must include either @AWSBackup@ or
    -- @AwsBackup@ in the role name. For example,
    -- @arn:aws:iam::123456789012:role\/AWSBackupRDSAccess@. Role names without
    -- those strings lack permissions to perform backup jobs.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | This is a boolean value indicating this is a parent (composite) backup
    -- job.
    isParent :: Prelude.Maybe Prelude.Bool,
    -- | This uniquely identifies a request to Backup to back up a resource. The
    -- return will be the parent (composite) job ID.
    parentJobId :: Prelude.Maybe Prelude.Text,
    -- | Contains an estimated percentage complete of a job at the time the job
    -- status was queried.
    percentDone :: Prelude.Maybe Prelude.Text,
    -- | An ARN that uniquely identifies a recovery point; for example,
    -- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
    recoveryPointArn :: Prelude.Maybe Prelude.Text,
    -- | An ARN that uniquely identifies a resource. The format of the ARN
    -- depends on the resource type.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The type of Amazon Web Services resource to be backed up; for example,
    -- an Amazon Elastic Block Store (Amazon EBS) volume or an Amazon
    -- Relational Database Service (Amazon RDS) database. For Windows Volume
    -- Shadow Copy Service (VSS) backups, the only supported resource type is
    -- Amazon EC2.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | Specifies the time in Unix format and Coordinated Universal Time (UTC)
    -- when a backup job must be started before it is canceled. The value is
    -- calculated by adding the start window to the scheduled time. So if the
    -- scheduled time were 6:00 PM and the start window is 2 hours, the
    -- @StartBy@ time would be 8:00 PM on the date specified. The value of
    -- @StartBy@ is accurate to milliseconds. For example, the value
    -- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
    startBy :: Prelude.Maybe Data.POSIX,
    -- | The current state of a resource recovery point.
    state :: Prelude.Maybe BackupJobState,
    -- | A detailed message explaining the status of the job to back up a
    -- resource.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackupJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'backupJob_accountId' - The account ID that owns the backup job.
--
-- 'backupJobId', 'backupJob_backupJobId' - Uniquely identifies a request to Backup to back up a resource.
--
-- 'backupOptions', 'backupJob_backupOptions' - Specifies the backup option for a selected resource. This option is only
-- available for Windows Volume Shadow Copy Service (VSS) backup jobs.
--
-- Valid values: Set to @\"WindowsVSS\":\"enabled\"@ to enable the
-- @WindowsVSS@ backup option and create a Windows VSS backup. Set to
-- @\"WindowsVSS\":\"disabled\"@ to create a regular backup. If you specify
-- an invalid option, you get an @InvalidParameterValueException@
-- exception.
--
-- 'backupSizeInBytes', 'backupJob_backupSizeInBytes' - The size, in bytes, of a backup.
--
-- 'backupType', 'backupJob_backupType' - Represents the type of backup for a backup job.
--
-- 'backupVaultArn', 'backupJob_backupVaultArn' - An Amazon Resource Name (ARN) that uniquely identifies a backup vault;
-- for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
--
-- 'backupVaultName', 'backupJob_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
--
-- 'bytesTransferred', 'backupJob_bytesTransferred' - The size in bytes transferred to a backup vault at the time that the job
-- status was queried.
--
-- 'completionDate', 'backupJob_completionDate' - The date and time a job to create a backup job is completed, in Unix
-- format and Coordinated Universal Time (UTC). The value of
-- @CompletionDate@ is accurate to milliseconds. For example, the value
-- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'createdBy', 'backupJob_createdBy' - Contains identifying information about the creation of a backup job,
-- including the @BackupPlanArn@, @BackupPlanId@, @BackupPlanVersion@, and
-- @BackupRuleId@ of the backup plan used to create it.
--
-- 'creationDate', 'backupJob_creationDate' - The date and time a backup job is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'expectedCompletionDate', 'backupJob_expectedCompletionDate' - The date and time a job to back up resources is expected to be
-- completed, in Unix format and Coordinated Universal Time (UTC). The
-- value of @ExpectedCompletionDate@ is accurate to milliseconds. For
-- example, the value 1516925490.087 represents Friday, January 26, 2018
-- 12:11:30.087 AM.
--
-- 'iamRoleArn', 'backupJob_iamRoleArn' - Specifies the IAM role ARN used to create the target recovery point. IAM
-- roles other than the default role must include either @AWSBackup@ or
-- @AwsBackup@ in the role name. For example,
-- @arn:aws:iam::123456789012:role\/AWSBackupRDSAccess@. Role names without
-- those strings lack permissions to perform backup jobs.
--
-- 'isParent', 'backupJob_isParent' - This is a boolean value indicating this is a parent (composite) backup
-- job.
--
-- 'parentJobId', 'backupJob_parentJobId' - This uniquely identifies a request to Backup to back up a resource. The
-- return will be the parent (composite) job ID.
--
-- 'percentDone', 'backupJob_percentDone' - Contains an estimated percentage complete of a job at the time the job
-- status was queried.
--
-- 'recoveryPointArn', 'backupJob_recoveryPointArn' - An ARN that uniquely identifies a recovery point; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
--
-- 'resourceArn', 'backupJob_resourceArn' - An ARN that uniquely identifies a resource. The format of the ARN
-- depends on the resource type.
--
-- 'resourceType', 'backupJob_resourceType' - The type of Amazon Web Services resource to be backed up; for example,
-- an Amazon Elastic Block Store (Amazon EBS) volume or an Amazon
-- Relational Database Service (Amazon RDS) database. For Windows Volume
-- Shadow Copy Service (VSS) backups, the only supported resource type is
-- Amazon EC2.
--
-- 'startBy', 'backupJob_startBy' - Specifies the time in Unix format and Coordinated Universal Time (UTC)
-- when a backup job must be started before it is canceled. The value is
-- calculated by adding the start window to the scheduled time. So if the
-- scheduled time were 6:00 PM and the start window is 2 hours, the
-- @StartBy@ time would be 8:00 PM on the date specified. The value of
-- @StartBy@ is accurate to milliseconds. For example, the value
-- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'state', 'backupJob_state' - The current state of a resource recovery point.
--
-- 'statusMessage', 'backupJob_statusMessage' - A detailed message explaining the status of the job to back up a
-- resource.
newBackupJob ::
  BackupJob
newBackupJob =
  BackupJob'
    { accountId = Prelude.Nothing,
      backupJobId = Prelude.Nothing,
      backupOptions = Prelude.Nothing,
      backupSizeInBytes = Prelude.Nothing,
      backupType = Prelude.Nothing,
      backupVaultArn = Prelude.Nothing,
      backupVaultName = Prelude.Nothing,
      bytesTransferred = Prelude.Nothing,
      completionDate = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      expectedCompletionDate = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      isParent = Prelude.Nothing,
      parentJobId = Prelude.Nothing,
      percentDone = Prelude.Nothing,
      recoveryPointArn = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      startBy = Prelude.Nothing,
      state = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The account ID that owns the backup job.
backupJob_accountId :: Lens.Lens' BackupJob (Prelude.Maybe Prelude.Text)
backupJob_accountId = Lens.lens (\BackupJob' {accountId} -> accountId) (\s@BackupJob' {} a -> s {accountId = a} :: BackupJob)

-- | Uniquely identifies a request to Backup to back up a resource.
backupJob_backupJobId :: Lens.Lens' BackupJob (Prelude.Maybe Prelude.Text)
backupJob_backupJobId = Lens.lens (\BackupJob' {backupJobId} -> backupJobId) (\s@BackupJob' {} a -> s {backupJobId = a} :: BackupJob)

-- | Specifies the backup option for a selected resource. This option is only
-- available for Windows Volume Shadow Copy Service (VSS) backup jobs.
--
-- Valid values: Set to @\"WindowsVSS\":\"enabled\"@ to enable the
-- @WindowsVSS@ backup option and create a Windows VSS backup. Set to
-- @\"WindowsVSS\":\"disabled\"@ to create a regular backup. If you specify
-- an invalid option, you get an @InvalidParameterValueException@
-- exception.
backupJob_backupOptions :: Lens.Lens' BackupJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
backupJob_backupOptions = Lens.lens (\BackupJob' {backupOptions} -> backupOptions) (\s@BackupJob' {} a -> s {backupOptions = a} :: BackupJob) Prelude.. Lens.mapping Lens.coerced

-- | The size, in bytes, of a backup.
backupJob_backupSizeInBytes :: Lens.Lens' BackupJob (Prelude.Maybe Prelude.Integer)
backupJob_backupSizeInBytes = Lens.lens (\BackupJob' {backupSizeInBytes} -> backupSizeInBytes) (\s@BackupJob' {} a -> s {backupSizeInBytes = a} :: BackupJob)

-- | Represents the type of backup for a backup job.
backupJob_backupType :: Lens.Lens' BackupJob (Prelude.Maybe Prelude.Text)
backupJob_backupType = Lens.lens (\BackupJob' {backupType} -> backupType) (\s@BackupJob' {} a -> s {backupType = a} :: BackupJob)

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault;
-- for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
backupJob_backupVaultArn :: Lens.Lens' BackupJob (Prelude.Maybe Prelude.Text)
backupJob_backupVaultArn = Lens.lens (\BackupJob' {backupVaultArn} -> backupVaultArn) (\s@BackupJob' {} a -> s {backupVaultArn = a} :: BackupJob)

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
backupJob_backupVaultName :: Lens.Lens' BackupJob (Prelude.Maybe Prelude.Text)
backupJob_backupVaultName = Lens.lens (\BackupJob' {backupVaultName} -> backupVaultName) (\s@BackupJob' {} a -> s {backupVaultName = a} :: BackupJob)

-- | The size in bytes transferred to a backup vault at the time that the job
-- status was queried.
backupJob_bytesTransferred :: Lens.Lens' BackupJob (Prelude.Maybe Prelude.Integer)
backupJob_bytesTransferred = Lens.lens (\BackupJob' {bytesTransferred} -> bytesTransferred) (\s@BackupJob' {} a -> s {bytesTransferred = a} :: BackupJob)

-- | The date and time a job to create a backup job is completed, in Unix
-- format and Coordinated Universal Time (UTC). The value of
-- @CompletionDate@ is accurate to milliseconds. For example, the value
-- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
backupJob_completionDate :: Lens.Lens' BackupJob (Prelude.Maybe Prelude.UTCTime)
backupJob_completionDate = Lens.lens (\BackupJob' {completionDate} -> completionDate) (\s@BackupJob' {} a -> s {completionDate = a} :: BackupJob) Prelude.. Lens.mapping Data._Time

-- | Contains identifying information about the creation of a backup job,
-- including the @BackupPlanArn@, @BackupPlanId@, @BackupPlanVersion@, and
-- @BackupRuleId@ of the backup plan used to create it.
backupJob_createdBy :: Lens.Lens' BackupJob (Prelude.Maybe RecoveryPointCreator)
backupJob_createdBy = Lens.lens (\BackupJob' {createdBy} -> createdBy) (\s@BackupJob' {} a -> s {createdBy = a} :: BackupJob)

-- | The date and time a backup job is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
backupJob_creationDate :: Lens.Lens' BackupJob (Prelude.Maybe Prelude.UTCTime)
backupJob_creationDate = Lens.lens (\BackupJob' {creationDate} -> creationDate) (\s@BackupJob' {} a -> s {creationDate = a} :: BackupJob) Prelude.. Lens.mapping Data._Time

-- | The date and time a job to back up resources is expected to be
-- completed, in Unix format and Coordinated Universal Time (UTC). The
-- value of @ExpectedCompletionDate@ is accurate to milliseconds. For
-- example, the value 1516925490.087 represents Friday, January 26, 2018
-- 12:11:30.087 AM.
backupJob_expectedCompletionDate :: Lens.Lens' BackupJob (Prelude.Maybe Prelude.UTCTime)
backupJob_expectedCompletionDate = Lens.lens (\BackupJob' {expectedCompletionDate} -> expectedCompletionDate) (\s@BackupJob' {} a -> s {expectedCompletionDate = a} :: BackupJob) Prelude.. Lens.mapping Data._Time

-- | Specifies the IAM role ARN used to create the target recovery point. IAM
-- roles other than the default role must include either @AWSBackup@ or
-- @AwsBackup@ in the role name. For example,
-- @arn:aws:iam::123456789012:role\/AWSBackupRDSAccess@. Role names without
-- those strings lack permissions to perform backup jobs.
backupJob_iamRoleArn :: Lens.Lens' BackupJob (Prelude.Maybe Prelude.Text)
backupJob_iamRoleArn = Lens.lens (\BackupJob' {iamRoleArn} -> iamRoleArn) (\s@BackupJob' {} a -> s {iamRoleArn = a} :: BackupJob)

-- | This is a boolean value indicating this is a parent (composite) backup
-- job.
backupJob_isParent :: Lens.Lens' BackupJob (Prelude.Maybe Prelude.Bool)
backupJob_isParent = Lens.lens (\BackupJob' {isParent} -> isParent) (\s@BackupJob' {} a -> s {isParent = a} :: BackupJob)

-- | This uniquely identifies a request to Backup to back up a resource. The
-- return will be the parent (composite) job ID.
backupJob_parentJobId :: Lens.Lens' BackupJob (Prelude.Maybe Prelude.Text)
backupJob_parentJobId = Lens.lens (\BackupJob' {parentJobId} -> parentJobId) (\s@BackupJob' {} a -> s {parentJobId = a} :: BackupJob)

-- | Contains an estimated percentage complete of a job at the time the job
-- status was queried.
backupJob_percentDone :: Lens.Lens' BackupJob (Prelude.Maybe Prelude.Text)
backupJob_percentDone = Lens.lens (\BackupJob' {percentDone} -> percentDone) (\s@BackupJob' {} a -> s {percentDone = a} :: BackupJob)

-- | An ARN that uniquely identifies a recovery point; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
backupJob_recoveryPointArn :: Lens.Lens' BackupJob (Prelude.Maybe Prelude.Text)
backupJob_recoveryPointArn = Lens.lens (\BackupJob' {recoveryPointArn} -> recoveryPointArn) (\s@BackupJob' {} a -> s {recoveryPointArn = a} :: BackupJob)

-- | An ARN that uniquely identifies a resource. The format of the ARN
-- depends on the resource type.
backupJob_resourceArn :: Lens.Lens' BackupJob (Prelude.Maybe Prelude.Text)
backupJob_resourceArn = Lens.lens (\BackupJob' {resourceArn} -> resourceArn) (\s@BackupJob' {} a -> s {resourceArn = a} :: BackupJob)

-- | The type of Amazon Web Services resource to be backed up; for example,
-- an Amazon Elastic Block Store (Amazon EBS) volume or an Amazon
-- Relational Database Service (Amazon RDS) database. For Windows Volume
-- Shadow Copy Service (VSS) backups, the only supported resource type is
-- Amazon EC2.
backupJob_resourceType :: Lens.Lens' BackupJob (Prelude.Maybe Prelude.Text)
backupJob_resourceType = Lens.lens (\BackupJob' {resourceType} -> resourceType) (\s@BackupJob' {} a -> s {resourceType = a} :: BackupJob)

-- | Specifies the time in Unix format and Coordinated Universal Time (UTC)
-- when a backup job must be started before it is canceled. The value is
-- calculated by adding the start window to the scheduled time. So if the
-- scheduled time were 6:00 PM and the start window is 2 hours, the
-- @StartBy@ time would be 8:00 PM on the date specified. The value of
-- @StartBy@ is accurate to milliseconds. For example, the value
-- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
backupJob_startBy :: Lens.Lens' BackupJob (Prelude.Maybe Prelude.UTCTime)
backupJob_startBy = Lens.lens (\BackupJob' {startBy} -> startBy) (\s@BackupJob' {} a -> s {startBy = a} :: BackupJob) Prelude.. Lens.mapping Data._Time

-- | The current state of a resource recovery point.
backupJob_state :: Lens.Lens' BackupJob (Prelude.Maybe BackupJobState)
backupJob_state = Lens.lens (\BackupJob' {state} -> state) (\s@BackupJob' {} a -> s {state = a} :: BackupJob)

-- | A detailed message explaining the status of the job to back up a
-- resource.
backupJob_statusMessage :: Lens.Lens' BackupJob (Prelude.Maybe Prelude.Text)
backupJob_statusMessage = Lens.lens (\BackupJob' {statusMessage} -> statusMessage) (\s@BackupJob' {} a -> s {statusMessage = a} :: BackupJob)

instance Data.FromJSON BackupJob where
  parseJSON =
    Data.withObject
      "BackupJob"
      ( \x ->
          BackupJob'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "BackupJobId")
            Prelude.<*> (x Data..:? "BackupOptions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "BackupSizeInBytes")
            Prelude.<*> (x Data..:? "BackupType")
            Prelude.<*> (x Data..:? "BackupVaultArn")
            Prelude.<*> (x Data..:? "BackupVaultName")
            Prelude.<*> (x Data..:? "BytesTransferred")
            Prelude.<*> (x Data..:? "CompletionDate")
            Prelude.<*> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "ExpectedCompletionDate")
            Prelude.<*> (x Data..:? "IamRoleArn")
            Prelude.<*> (x Data..:? "IsParent")
            Prelude.<*> (x Data..:? "ParentJobId")
            Prelude.<*> (x Data..:? "PercentDone")
            Prelude.<*> (x Data..:? "RecoveryPointArn")
            Prelude.<*> (x Data..:? "ResourceArn")
            Prelude.<*> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "StartBy")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "StatusMessage")
      )

instance Prelude.Hashable BackupJob where
  hashWithSalt _salt BackupJob' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` backupJobId
      `Prelude.hashWithSalt` backupOptions
      `Prelude.hashWithSalt` backupSizeInBytes
      `Prelude.hashWithSalt` backupType
      `Prelude.hashWithSalt` backupVaultArn
      `Prelude.hashWithSalt` backupVaultName
      `Prelude.hashWithSalt` bytesTransferred
      `Prelude.hashWithSalt` completionDate
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` expectedCompletionDate
      `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` isParent
      `Prelude.hashWithSalt` parentJobId
      `Prelude.hashWithSalt` percentDone
      `Prelude.hashWithSalt` recoveryPointArn
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` startBy
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData BackupJob where
  rnf BackupJob' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf backupJobId
      `Prelude.seq` Prelude.rnf backupOptions
      `Prelude.seq` Prelude.rnf backupSizeInBytes
      `Prelude.seq` Prelude.rnf backupType
      `Prelude.seq` Prelude.rnf backupVaultArn
      `Prelude.seq` Prelude.rnf backupVaultName
      `Prelude.seq` Prelude.rnf bytesTransferred
      `Prelude.seq` Prelude.rnf completionDate
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf expectedCompletionDate
      `Prelude.seq` Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf isParent
      `Prelude.seq` Prelude.rnf parentJobId
      `Prelude.seq` Prelude.rnf percentDone
      `Prelude.seq` Prelude.rnf recoveryPointArn
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf startBy
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf statusMessage
