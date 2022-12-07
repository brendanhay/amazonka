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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
  { -- | The type of Amazon Web Services resource to be copied; for example, an
    -- Amazon Elastic Block Store (Amazon EBS) volume or an Amazon Relational
    -- Database Service (Amazon RDS) database.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The date and time a copy job is completed, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CompletionDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    completionDate :: Prelude.Maybe Data.POSIX,
    -- | The current state of a copy job.
    state :: Prelude.Maybe CopyJobState,
    -- | The date and time a copy job is created, in Unix format and Coordinated
    -- Universal Time (UTC). The value of @CreationDate@ is accurate to
    -- milliseconds. For example, the value 1516925490.087 represents Friday,
    -- January 26, 2018 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a destination
    -- copy vault; for example,
    -- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
    destinationBackupVaultArn :: Prelude.Maybe Prelude.Text,
    -- | The size, in bytes, of a copy job.
    backupSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | Specifies the IAM role ARN used to copy the target recovery point; for
    -- example, @arn:aws:iam::123456789012:role\/S3Access@.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The account ID that owns the copy job.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | An ARN that uniquely identifies a destination recovery point; for
    -- example,
    -- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
    destinationRecoveryPointArn :: Prelude.Maybe Prelude.Text,
    -- | Uniquely identifies a copy job.
    copyJobId :: Prelude.Maybe Prelude.Text,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a source copy
    -- vault; for example,
    -- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
    sourceBackupVaultArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services resource to be copied; for example, an Amazon
    -- Elastic Block Store (Amazon EBS) volume or an Amazon Relational Database
    -- Service (Amazon RDS) database.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | A detailed message explaining the status of the job to copy a resource.
    statusMessage :: Prelude.Maybe Prelude.Text,
    createdBy :: Prelude.Maybe RecoveryPointCreator,
    -- | An ARN that uniquely identifies a source recovery point; for example,
    -- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
    sourceRecoveryPointArn :: Prelude.Maybe Prelude.Text
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
-- 'resourceType', 'copyJob_resourceType' - The type of Amazon Web Services resource to be copied; for example, an
-- Amazon Elastic Block Store (Amazon EBS) volume or an Amazon Relational
-- Database Service (Amazon RDS) database.
--
-- 'completionDate', 'copyJob_completionDate' - The date and time a copy job is completed, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CompletionDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'state', 'copyJob_state' - The current state of a copy job.
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
-- 'backupSizeInBytes', 'copyJob_backupSizeInBytes' - The size, in bytes, of a copy job.
--
-- 'iamRoleArn', 'copyJob_iamRoleArn' - Specifies the IAM role ARN used to copy the target recovery point; for
-- example, @arn:aws:iam::123456789012:role\/S3Access@.
--
-- 'accountId', 'copyJob_accountId' - The account ID that owns the copy job.
--
-- 'destinationRecoveryPointArn', 'copyJob_destinationRecoveryPointArn' - An ARN that uniquely identifies a destination recovery point; for
-- example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
--
-- 'copyJobId', 'copyJob_copyJobId' - Uniquely identifies a copy job.
--
-- 'sourceBackupVaultArn', 'copyJob_sourceBackupVaultArn' - An Amazon Resource Name (ARN) that uniquely identifies a source copy
-- vault; for example,
-- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
--
-- 'resourceArn', 'copyJob_resourceArn' - The Amazon Web Services resource to be copied; for example, an Amazon
-- Elastic Block Store (Amazon EBS) volume or an Amazon Relational Database
-- Service (Amazon RDS) database.
--
-- 'statusMessage', 'copyJob_statusMessage' - A detailed message explaining the status of the job to copy a resource.
--
-- 'createdBy', 'copyJob_createdBy' - Undocumented member.
--
-- 'sourceRecoveryPointArn', 'copyJob_sourceRecoveryPointArn' - An ARN that uniquely identifies a source recovery point; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
newCopyJob ::
  CopyJob
newCopyJob =
  CopyJob'
    { resourceType = Prelude.Nothing,
      completionDate = Prelude.Nothing,
      state = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      destinationBackupVaultArn = Prelude.Nothing,
      backupSizeInBytes = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      accountId = Prelude.Nothing,
      destinationRecoveryPointArn = Prelude.Nothing,
      copyJobId = Prelude.Nothing,
      sourceBackupVaultArn = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      sourceRecoveryPointArn = Prelude.Nothing
    }

-- | The type of Amazon Web Services resource to be copied; for example, an
-- Amazon Elastic Block Store (Amazon EBS) volume or an Amazon Relational
-- Database Service (Amazon RDS) database.
copyJob_resourceType :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Text)
copyJob_resourceType = Lens.lens (\CopyJob' {resourceType} -> resourceType) (\s@CopyJob' {} a -> s {resourceType = a} :: CopyJob)

-- | The date and time a copy job is completed, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CompletionDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
copyJob_completionDate :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.UTCTime)
copyJob_completionDate = Lens.lens (\CopyJob' {completionDate} -> completionDate) (\s@CopyJob' {} a -> s {completionDate = a} :: CopyJob) Prelude.. Lens.mapping Data._Time

-- | The current state of a copy job.
copyJob_state :: Lens.Lens' CopyJob (Prelude.Maybe CopyJobState)
copyJob_state = Lens.lens (\CopyJob' {state} -> state) (\s@CopyJob' {} a -> s {state = a} :: CopyJob)

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

-- | The size, in bytes, of a copy job.
copyJob_backupSizeInBytes :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Integer)
copyJob_backupSizeInBytes = Lens.lens (\CopyJob' {backupSizeInBytes} -> backupSizeInBytes) (\s@CopyJob' {} a -> s {backupSizeInBytes = a} :: CopyJob)

-- | Specifies the IAM role ARN used to copy the target recovery point; for
-- example, @arn:aws:iam::123456789012:role\/S3Access@.
copyJob_iamRoleArn :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Text)
copyJob_iamRoleArn = Lens.lens (\CopyJob' {iamRoleArn} -> iamRoleArn) (\s@CopyJob' {} a -> s {iamRoleArn = a} :: CopyJob)

-- | The account ID that owns the copy job.
copyJob_accountId :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Text)
copyJob_accountId = Lens.lens (\CopyJob' {accountId} -> accountId) (\s@CopyJob' {} a -> s {accountId = a} :: CopyJob)

-- | An ARN that uniquely identifies a destination recovery point; for
-- example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
copyJob_destinationRecoveryPointArn :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Text)
copyJob_destinationRecoveryPointArn = Lens.lens (\CopyJob' {destinationRecoveryPointArn} -> destinationRecoveryPointArn) (\s@CopyJob' {} a -> s {destinationRecoveryPointArn = a} :: CopyJob)

-- | Uniquely identifies a copy job.
copyJob_copyJobId :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Text)
copyJob_copyJobId = Lens.lens (\CopyJob' {copyJobId} -> copyJobId) (\s@CopyJob' {} a -> s {copyJobId = a} :: CopyJob)

-- | An Amazon Resource Name (ARN) that uniquely identifies a source copy
-- vault; for example,
-- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
copyJob_sourceBackupVaultArn :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Text)
copyJob_sourceBackupVaultArn = Lens.lens (\CopyJob' {sourceBackupVaultArn} -> sourceBackupVaultArn) (\s@CopyJob' {} a -> s {sourceBackupVaultArn = a} :: CopyJob)

-- | The Amazon Web Services resource to be copied; for example, an Amazon
-- Elastic Block Store (Amazon EBS) volume or an Amazon Relational Database
-- Service (Amazon RDS) database.
copyJob_resourceArn :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Text)
copyJob_resourceArn = Lens.lens (\CopyJob' {resourceArn} -> resourceArn) (\s@CopyJob' {} a -> s {resourceArn = a} :: CopyJob)

-- | A detailed message explaining the status of the job to copy a resource.
copyJob_statusMessage :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Text)
copyJob_statusMessage = Lens.lens (\CopyJob' {statusMessage} -> statusMessage) (\s@CopyJob' {} a -> s {statusMessage = a} :: CopyJob)

-- | Undocumented member.
copyJob_createdBy :: Lens.Lens' CopyJob (Prelude.Maybe RecoveryPointCreator)
copyJob_createdBy = Lens.lens (\CopyJob' {createdBy} -> createdBy) (\s@CopyJob' {} a -> s {createdBy = a} :: CopyJob)

-- | An ARN that uniquely identifies a source recovery point; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
copyJob_sourceRecoveryPointArn :: Lens.Lens' CopyJob (Prelude.Maybe Prelude.Text)
copyJob_sourceRecoveryPointArn = Lens.lens (\CopyJob' {sourceRecoveryPointArn} -> sourceRecoveryPointArn) (\s@CopyJob' {} a -> s {sourceRecoveryPointArn = a} :: CopyJob)

instance Data.FromJSON CopyJob where
  parseJSON =
    Data.withObject
      "CopyJob"
      ( \x ->
          CopyJob'
            Prelude.<$> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "CompletionDate")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "DestinationBackupVaultArn")
            Prelude.<*> (x Data..:? "BackupSizeInBytes")
            Prelude.<*> (x Data..:? "IamRoleArn")
            Prelude.<*> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "DestinationRecoveryPointArn")
            Prelude.<*> (x Data..:? "CopyJobId")
            Prelude.<*> (x Data..:? "SourceBackupVaultArn")
            Prelude.<*> (x Data..:? "ResourceArn")
            Prelude.<*> (x Data..:? "StatusMessage")
            Prelude.<*> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..:? "SourceRecoveryPointArn")
      )

instance Prelude.Hashable CopyJob where
  hashWithSalt _salt CopyJob' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` completionDate
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` destinationBackupVaultArn
      `Prelude.hashWithSalt` backupSizeInBytes
      `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` destinationRecoveryPointArn
      `Prelude.hashWithSalt` copyJobId
      `Prelude.hashWithSalt` sourceBackupVaultArn
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` sourceRecoveryPointArn

instance Prelude.NFData CopyJob where
  rnf CopyJob' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf completionDate
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf destinationBackupVaultArn
      `Prelude.seq` Prelude.rnf backupSizeInBytes
      `Prelude.seq` Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf destinationRecoveryPointArn
      `Prelude.seq` Prelude.rnf copyJobId
      `Prelude.seq` Prelude.rnf sourceBackupVaultArn
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf sourceRecoveryPointArn
