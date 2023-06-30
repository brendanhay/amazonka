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
-- Module      : Amazonka.Backup.Types.CopyJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.CopyJob where

import Amazonka.Backup.Types.CopyJobState
import Amazonka.Backup.Types.RecoveryPointCreator
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains detailed information about a copy job.
--
-- /See:/ 'newCopyJob' smart constructor.
data CopyJob = CopyJob'
  { -- | The account ID that owns the copy job.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The size, in bytes, of a copy job.
    backupSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | This returns the statistics of the included child (nested) copy jobs.
    childJobsInState :: Prelude.Maybe (Prelude.HashMap CopyJobState Prelude.Integer),
    -- | The date and time a copy job is completed, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CompletionDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    completionDate :: Prelude.Maybe Data.POSIX,
    -- | This is the identifier of a resource within a composite group, such as
    -- nested (child) recovery point belonging to a composite (parent) stack.
    -- The ID is transferred from the
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/resources-section-structure.html#resources-section-structure-syntax logical ID>
    -- within a stack.
    compositeMemberIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Uniquely identifies a copy job.
    copyJobId :: Prelude.Maybe Prelude.Text,
    createdBy :: Prelude.Maybe RecoveryPointCreator,
    -- | The date and time a copy job is created, in Unix format and Coordinated
    -- Universal Time (UTC). The value of @CreationDate@ is accurate to
    -- milliseconds. For example, the value 1516925490.087 represents Friday,
    -- January 26, 2018 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a destination
    -- copy vault; for example,
    -- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
    destinationBackupVaultArn :: Prelude.Maybe Prelude.Text,
    -- | An ARN that uniquely identifies a destination recovery point; for
    -- example,
    -- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
    destinationRecoveryPointArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the IAM role ARN used to copy the target recovery point; for
    -- example, @arn:aws:iam::123456789012:role\/S3Access@.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | This is a boolean value indicating this is a parent (composite) copy
    -- job.
    isParent :: Prelude.Maybe Prelude.Bool,
    -- | This is the number of child (nested) copy jobs.
    numberOfChildJobs :: Prelude.Maybe Prelude.Integer,
    -- | This uniquely identifies a request to Backup to copy a resource. The
    -- return will be the parent (composite) job ID.
    parentJobId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services resource to be copied; for example, an Amazon
    -- Elastic Block Store (Amazon EBS) volume or an Amazon Relational Database
    -- Service (Amazon RDS) database.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The type of Amazon Web Services resource to be copied; for example, an
    -- Amazon Elastic Block Store (Amazon EBS) volume or an Amazon Relational
    -- Database Service (Amazon RDS) database.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a source copy
    -- vault; for example,
    -- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
    sourceBackupVaultArn :: Prelude.Maybe Prelude.Text,
    -- | An ARN that uniquely identifies a source recovery point; for example,
    -- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
    sourceRecoveryPointArn :: Prelude.Maybe Prelude.Text,
    -- | The current state of a copy job.
    state :: Prelude.Maybe CopyJobState,
    -- | A detailed message explaining the status of the job to copy a resource.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'copyJob_accountId' - The account ID that owns the copy job.
--
-- 'backupSizeInBytes', 'copyJob_backupSizeInBytes' - The size, in bytes, of a copy job.
--
-- 'childJobsInState', 'copyJob_childJobsInState' - This returns the statistics of the included child (nested) copy jobs.
--
-- 'completionDate', 'copyJob_completionDate' - The date and time a copy job is completed, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CompletionDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'compositeMemberIdentifier', 'copyJob_compositeMemberIdentifier' - This is the identifier of a resource within a composite group, such as
-- nested (child) recovery point belonging to a composite (parent) stack.
-- The ID is transferred from the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/resources-section-structure.html#resources-section-structure-syntax logical ID>
-- within a stack.
--
-- 'copyJobId', 'copyJob_copyJobId' - Uniquely identifies a copy job.
--
-- 'createdBy', 'copyJob_createdBy' - Undocumented member.
--
-- 'creationDate', 'copyJob_creationDate' - The date and time a copy job is created, in Unix format and Coordinated
-- Universal Time (UTC). The value of @CreationDate@ is accurate to
-- milliseconds. For example, the value 1516925490.087 represents Friday,
-- January 26, 2018 12:11:30.087 AM.
--
-- 'destinationBackupVaultArn', 'copyJob_destinationBackupVaultArn' - An Amazon Resource Name (ARN) that uniquely identifies a destination
-- copy vault; for example,
-- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
--
-- 'destinationRecoveryPointArn', 'copyJob_destinationRecoveryPointArn' - An ARN that uniquely identifies a destination recovery point; for
-- example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
--
-- 'iamRoleArn', 'copyJob_iamRoleArn' - Specifies the IAM role ARN used to copy the target recovery point; for
-- example, @arn:aws:iam::123456789012:role\/S3Access@.
--
-- 'isParent', 'copyJob_isParent' - This is a boolean value indicating this is a parent (composite) copy
-- job.
--
-- 'numberOfChildJobs', 'copyJob_numberOfChildJobs' - This is the number of child (nested) copy jobs.
--
-- 'parentJobId', 'copyJob_parentJobId' - This uniquely identifies a request to Backup to copy a resource. The
-- return will be the parent (composite) job ID.
--
-- 'resourceArn', 'copyJob_resourceArn' - The Amazon Web Services resource to be copied; for example, an Amazon
-- Elastic Block Store (Amazon EBS) volume or an Amazon Relational Database
-- Service (Amazon RDS) database.
--
-- 'resourceType', 'copyJob_resourceType' - The type of Amazon Web Services resource to be copied; for example, an
-- Amazon Elastic Block Store (Amazon EBS) volume or an Amazon Relational
-- Database Service (Amazon RDS) database.
--
-- 'sourceBackupVaultArn', 'copyJob_sourceBackupVaultArn' - An Amazon Resource Name (ARN) that uniquely identifies a source copy
-- vault; for example,
-- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
--
-- 'sourceRecoveryPointArn', 'copyJob_sourceRecoveryPointArn' - An ARN that uniquely identifies a source recovery point; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
--
-- 'state', 'copyJob_state' - The current state of a copy job.
--
-- 'statusMessage', 'copyJob_statusMessage' - A detailed message explaining the status of the job to copy a resource.
newCopyJob ::
  CopyJob
newCopyJob =
  CopyJob'
    { accountId = Prelude.Nothing,
      backupSizeInBytes = Prelude.Nothing,
      childJobsInState = Prelude.Nothing,
      completionDate = Prelude.Nothing,
      compositeMemberIdentifier = Prelude.Nothing,
      copyJobId = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      destinationBackupVaultArn = Prelude.Nothing,
      destinationRecoveryPointArn = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      isParent = Prelude.Nothing,
      numberOfChildJobs = Prelude.Nothing,
      parentJobId = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      sourceBackupVaultArn = Prelude.Nothing,
      sourceRecoveryPointArn = Prelude.Nothing,
      state = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The account ID that owns the copy job.
copyJob_accountId :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Text)
copyJob_accountId = Lens.lens (\CopyJob' {accountId} -> accountId) (\s@CopyJob' {} a -> s {accountId = a} :: CopyJob)

-- | The size, in bytes, of a copy job.
copyJob_backupSizeInBytes :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Integer)
copyJob_backupSizeInBytes = Lens.lens (\CopyJob' {backupSizeInBytes} -> backupSizeInBytes) (\s@CopyJob' {} a -> s {backupSizeInBytes = a} :: CopyJob)

-- | This returns the statistics of the included child (nested) copy jobs.
copyJob_childJobsInState :: Lens.Lens' CopyJob (Prelude.Maybe (Prelude.HashMap CopyJobState Prelude.Integer))
copyJob_childJobsInState = Lens.lens (\CopyJob' {childJobsInState} -> childJobsInState) (\s@CopyJob' {} a -> s {childJobsInState = a} :: CopyJob) Prelude.. Lens.mapping Lens.coerced

-- | The date and time a copy job is completed, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CompletionDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
copyJob_completionDate :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.UTCTime)
copyJob_completionDate = Lens.lens (\CopyJob' {completionDate} -> completionDate) (\s@CopyJob' {} a -> s {completionDate = a} :: CopyJob) Prelude.. Lens.mapping Data._Time

-- | This is the identifier of a resource within a composite group, such as
-- nested (child) recovery point belonging to a composite (parent) stack.
-- The ID is transferred from the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/resources-section-structure.html#resources-section-structure-syntax logical ID>
-- within a stack.
copyJob_compositeMemberIdentifier :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Text)
copyJob_compositeMemberIdentifier = Lens.lens (\CopyJob' {compositeMemberIdentifier} -> compositeMemberIdentifier) (\s@CopyJob' {} a -> s {compositeMemberIdentifier = a} :: CopyJob)

-- | Uniquely identifies a copy job.
copyJob_copyJobId :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Text)
copyJob_copyJobId = Lens.lens (\CopyJob' {copyJobId} -> copyJobId) (\s@CopyJob' {} a -> s {copyJobId = a} :: CopyJob)

-- | Undocumented member.
copyJob_createdBy :: Lens.Lens' CopyJob (Prelude.Maybe RecoveryPointCreator)
copyJob_createdBy = Lens.lens (\CopyJob' {createdBy} -> createdBy) (\s@CopyJob' {} a -> s {createdBy = a} :: CopyJob)

-- | The date and time a copy job is created, in Unix format and Coordinated
-- Universal Time (UTC). The value of @CreationDate@ is accurate to
-- milliseconds. For example, the value 1516925490.087 represents Friday,
-- January 26, 2018 12:11:30.087 AM.
copyJob_creationDate :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.UTCTime)
copyJob_creationDate = Lens.lens (\CopyJob' {creationDate} -> creationDate) (\s@CopyJob' {} a -> s {creationDate = a} :: CopyJob) Prelude.. Lens.mapping Data._Time

-- | An Amazon Resource Name (ARN) that uniquely identifies a destination
-- copy vault; for example,
-- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
copyJob_destinationBackupVaultArn :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Text)
copyJob_destinationBackupVaultArn = Lens.lens (\CopyJob' {destinationBackupVaultArn} -> destinationBackupVaultArn) (\s@CopyJob' {} a -> s {destinationBackupVaultArn = a} :: CopyJob)

-- | An ARN that uniquely identifies a destination recovery point; for
-- example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
copyJob_destinationRecoveryPointArn :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Text)
copyJob_destinationRecoveryPointArn = Lens.lens (\CopyJob' {destinationRecoveryPointArn} -> destinationRecoveryPointArn) (\s@CopyJob' {} a -> s {destinationRecoveryPointArn = a} :: CopyJob)

-- | Specifies the IAM role ARN used to copy the target recovery point; for
-- example, @arn:aws:iam::123456789012:role\/S3Access@.
copyJob_iamRoleArn :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Text)
copyJob_iamRoleArn = Lens.lens (\CopyJob' {iamRoleArn} -> iamRoleArn) (\s@CopyJob' {} a -> s {iamRoleArn = a} :: CopyJob)

-- | This is a boolean value indicating this is a parent (composite) copy
-- job.
copyJob_isParent :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Bool)
copyJob_isParent = Lens.lens (\CopyJob' {isParent} -> isParent) (\s@CopyJob' {} a -> s {isParent = a} :: CopyJob)

-- | This is the number of child (nested) copy jobs.
copyJob_numberOfChildJobs :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Integer)
copyJob_numberOfChildJobs = Lens.lens (\CopyJob' {numberOfChildJobs} -> numberOfChildJobs) (\s@CopyJob' {} a -> s {numberOfChildJobs = a} :: CopyJob)

-- | This uniquely identifies a request to Backup to copy a resource. The
-- return will be the parent (composite) job ID.
copyJob_parentJobId :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Text)
copyJob_parentJobId = Lens.lens (\CopyJob' {parentJobId} -> parentJobId) (\s@CopyJob' {} a -> s {parentJobId = a} :: CopyJob)

-- | The Amazon Web Services resource to be copied; for example, an Amazon
-- Elastic Block Store (Amazon EBS) volume or an Amazon Relational Database
-- Service (Amazon RDS) database.
copyJob_resourceArn :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Text)
copyJob_resourceArn = Lens.lens (\CopyJob' {resourceArn} -> resourceArn) (\s@CopyJob' {} a -> s {resourceArn = a} :: CopyJob)

-- | The type of Amazon Web Services resource to be copied; for example, an
-- Amazon Elastic Block Store (Amazon EBS) volume or an Amazon Relational
-- Database Service (Amazon RDS) database.
copyJob_resourceType :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Text)
copyJob_resourceType = Lens.lens (\CopyJob' {resourceType} -> resourceType) (\s@CopyJob' {} a -> s {resourceType = a} :: CopyJob)

-- | An Amazon Resource Name (ARN) that uniquely identifies a source copy
-- vault; for example,
-- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
copyJob_sourceBackupVaultArn :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Text)
copyJob_sourceBackupVaultArn = Lens.lens (\CopyJob' {sourceBackupVaultArn} -> sourceBackupVaultArn) (\s@CopyJob' {} a -> s {sourceBackupVaultArn = a} :: CopyJob)

-- | An ARN that uniquely identifies a source recovery point; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
copyJob_sourceRecoveryPointArn :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Text)
copyJob_sourceRecoveryPointArn = Lens.lens (\CopyJob' {sourceRecoveryPointArn} -> sourceRecoveryPointArn) (\s@CopyJob' {} a -> s {sourceRecoveryPointArn = a} :: CopyJob)

-- | The current state of a copy job.
copyJob_state :: Lens.Lens' CopyJob (Prelude.Maybe CopyJobState)
copyJob_state = Lens.lens (\CopyJob' {state} -> state) (\s@CopyJob' {} a -> s {state = a} :: CopyJob)

-- | A detailed message explaining the status of the job to copy a resource.
copyJob_statusMessage :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Text)
copyJob_statusMessage = Lens.lens (\CopyJob' {statusMessage} -> statusMessage) (\s@CopyJob' {} a -> s {statusMessage = a} :: CopyJob)

instance Data.FromJSON CopyJob where
  parseJSON =
    Data.withObject
      "CopyJob"
      ( \x ->
          CopyJob'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "BackupSizeInBytes")
            Prelude.<*> ( x
                            Data..:? "ChildJobsInState"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CompletionDate")
            Prelude.<*> (x Data..:? "CompositeMemberIdentifier")
            Prelude.<*> (x Data..:? "CopyJobId")
            Prelude.<*> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "DestinationBackupVaultArn")
            Prelude.<*> (x Data..:? "DestinationRecoveryPointArn")
            Prelude.<*> (x Data..:? "IamRoleArn")
            Prelude.<*> (x Data..:? "IsParent")
            Prelude.<*> (x Data..:? "NumberOfChildJobs")
            Prelude.<*> (x Data..:? "ParentJobId")
            Prelude.<*> (x Data..:? "ResourceArn")
            Prelude.<*> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "SourceBackupVaultArn")
            Prelude.<*> (x Data..:? "SourceRecoveryPointArn")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "StatusMessage")
      )

instance Prelude.Hashable CopyJob where
  hashWithSalt _salt CopyJob' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` backupSizeInBytes
      `Prelude.hashWithSalt` childJobsInState
      `Prelude.hashWithSalt` completionDate
      `Prelude.hashWithSalt` compositeMemberIdentifier
      `Prelude.hashWithSalt` copyJobId
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` destinationBackupVaultArn
      `Prelude.hashWithSalt` destinationRecoveryPointArn
      `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` isParent
      `Prelude.hashWithSalt` numberOfChildJobs
      `Prelude.hashWithSalt` parentJobId
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` sourceBackupVaultArn
      `Prelude.hashWithSalt` sourceRecoveryPointArn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData CopyJob where
  rnf CopyJob' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf backupSizeInBytes
      `Prelude.seq` Prelude.rnf childJobsInState
      `Prelude.seq` Prelude.rnf completionDate
      `Prelude.seq` Prelude.rnf compositeMemberIdentifier
      `Prelude.seq` Prelude.rnf copyJobId
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf destinationBackupVaultArn
      `Prelude.seq` Prelude.rnf destinationRecoveryPointArn
      `Prelude.seq` Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf isParent
      `Prelude.seq` Prelude.rnf numberOfChildJobs
      `Prelude.seq` Prelude.rnf parentJobId
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf sourceBackupVaultArn
      `Prelude.seq` Prelude.rnf sourceRecoveryPointArn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf statusMessage
