{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Backup.DescribeRecoveryPoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata associated with a recovery point, including ID, status,
-- encryption, and lifecycle.
module Amazonka.Backup.DescribeRecoveryPoint
  ( -- * Creating a Request
    DescribeRecoveryPoint (..),
    newDescribeRecoveryPoint,

    -- * Request Lenses
    describeRecoveryPoint_backupVaultName,
    describeRecoveryPoint_recoveryPointArn,

    -- * Destructuring the Response
    DescribeRecoveryPointResponse (..),
    newDescribeRecoveryPointResponse,

    -- * Response Lenses
    describeRecoveryPointResponse_encryptionKeyArn,
    describeRecoveryPointResponse_resourceType,
    describeRecoveryPointResponse_lifecycle,
    describeRecoveryPointResponse_recoveryPointArn,
    describeRecoveryPointResponse_completionDate,
    describeRecoveryPointResponse_backupVaultName,
    describeRecoveryPointResponse_creationDate,
    describeRecoveryPointResponse_backupSizeInBytes,
    describeRecoveryPointResponse_status,
    describeRecoveryPointResponse_backupVaultArn,
    describeRecoveryPointResponse_isEncrypted,
    describeRecoveryPointResponse_iamRoleArn,
    describeRecoveryPointResponse_sourceBackupVaultArn,
    describeRecoveryPointResponse_resourceArn,
    describeRecoveryPointResponse_storageClass,
    describeRecoveryPointResponse_statusMessage,
    describeRecoveryPointResponse_createdBy,
    describeRecoveryPointResponse_lastRestoreTime,
    describeRecoveryPointResponse_calculatedLifecycle,
    describeRecoveryPointResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRecoveryPoint' smart constructor.
data DescribeRecoveryPoint = DescribeRecoveryPoint'
  { -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Amazon Web Services Region where they are created. They
    -- consist of lowercase letters, numbers, and hyphens.
    backupVaultName :: Prelude.Text,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a recovery point;
    -- for example,
    -- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
    recoveryPointArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRecoveryPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupVaultName', 'describeRecoveryPoint_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
--
-- 'recoveryPointArn', 'describeRecoveryPoint_recoveryPointArn' - An Amazon Resource Name (ARN) that uniquely identifies a recovery point;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
newDescribeRecoveryPoint ::
  -- | 'backupVaultName'
  Prelude.Text ->
  -- | 'recoveryPointArn'
  Prelude.Text ->
  DescribeRecoveryPoint
newDescribeRecoveryPoint
  pBackupVaultName_
  pRecoveryPointArn_ =
    DescribeRecoveryPoint'
      { backupVaultName =
          pBackupVaultName_,
        recoveryPointArn = pRecoveryPointArn_
      }

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
describeRecoveryPoint_backupVaultName :: Lens.Lens' DescribeRecoveryPoint Prelude.Text
describeRecoveryPoint_backupVaultName = Lens.lens (\DescribeRecoveryPoint' {backupVaultName} -> backupVaultName) (\s@DescribeRecoveryPoint' {} a -> s {backupVaultName = a} :: DescribeRecoveryPoint)

-- | An Amazon Resource Name (ARN) that uniquely identifies a recovery point;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
describeRecoveryPoint_recoveryPointArn :: Lens.Lens' DescribeRecoveryPoint Prelude.Text
describeRecoveryPoint_recoveryPointArn = Lens.lens (\DescribeRecoveryPoint' {recoveryPointArn} -> recoveryPointArn) (\s@DescribeRecoveryPoint' {} a -> s {recoveryPointArn = a} :: DescribeRecoveryPoint)

instance Core.AWSRequest DescribeRecoveryPoint where
  type
    AWSResponse DescribeRecoveryPoint =
      DescribeRecoveryPointResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRecoveryPointResponse'
            Prelude.<$> (x Data..?> "EncryptionKeyArn")
            Prelude.<*> (x Data..?> "ResourceType")
            Prelude.<*> (x Data..?> "Lifecycle")
            Prelude.<*> (x Data..?> "RecoveryPointArn")
            Prelude.<*> (x Data..?> "CompletionDate")
            Prelude.<*> (x Data..?> "BackupVaultName")
            Prelude.<*> (x Data..?> "CreationDate")
            Prelude.<*> (x Data..?> "BackupSizeInBytes")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "BackupVaultArn")
            Prelude.<*> (x Data..?> "IsEncrypted")
            Prelude.<*> (x Data..?> "IamRoleArn")
            Prelude.<*> (x Data..?> "SourceBackupVaultArn")
            Prelude.<*> (x Data..?> "ResourceArn")
            Prelude.<*> (x Data..?> "StorageClass")
            Prelude.<*> (x Data..?> "StatusMessage")
            Prelude.<*> (x Data..?> "CreatedBy")
            Prelude.<*> (x Data..?> "LastRestoreTime")
            Prelude.<*> (x Data..?> "CalculatedLifecycle")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRecoveryPoint where
  hashWithSalt _salt DescribeRecoveryPoint' {..} =
    _salt `Prelude.hashWithSalt` backupVaultName
      `Prelude.hashWithSalt` recoveryPointArn

instance Prelude.NFData DescribeRecoveryPoint where
  rnf DescribeRecoveryPoint' {..} =
    Prelude.rnf backupVaultName
      `Prelude.seq` Prelude.rnf recoveryPointArn

instance Data.ToHeaders DescribeRecoveryPoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeRecoveryPoint where
  toPath DescribeRecoveryPoint' {..} =
    Prelude.mconcat
      [ "/backup-vaults/",
        Data.toBS backupVaultName,
        "/recovery-points/",
        Data.toBS recoveryPointArn
      ]

instance Data.ToQuery DescribeRecoveryPoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRecoveryPointResponse' smart constructor.
data DescribeRecoveryPointResponse = DescribeRecoveryPointResponse'
  { -- | The server-side encryption key used to protect your backups; for
    -- example,
    -- @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
    encryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The type of Amazon Web Services resource to save as a recovery point;
    -- for example, an Amazon Elastic Block Store (Amazon EBS) volume or an
    -- Amazon Relational Database Service (Amazon RDS) database.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The lifecycle defines when a protected resource is transitioned to cold
    -- storage and when it expires. Backup transitions and expires backups
    -- automatically according to the lifecycle that you define.
    --
    -- Backups that are transitioned to cold storage must be stored in cold
    -- storage for a minimum of 90 days. Therefore, the “retention” setting
    -- must be 90 days greater than the “transition to cold after days”
    -- setting. The “transition to cold after days” setting cannot be changed
    -- after a backup has been transitioned to cold.
    --
    -- Resource types that are able to be transitioned to cold storage are
    -- listed in the \"Lifecycle to cold storage\" section of the
    -- <https://docs.aws.amazon.com/aws-backup/latest/devguide/whatisbackup.html#features-by-resource Feature availability by resource>
    -- table. Backup ignores this expression for other resource types.
    lifecycle :: Prelude.Maybe Lifecycle,
    -- | An ARN that uniquely identifies a recovery point; for example,
    -- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
    recoveryPointArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that a job to create a recovery point is completed, in
    -- Unix format and Coordinated Universal Time (UTC). The value of
    -- @CompletionDate@ is accurate to milliseconds. For example, the value
    -- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
    completionDate :: Prelude.Maybe Data.POSIX,
    -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Region where they are created. They consist of lowercase
    -- letters, numbers, and hyphens.
    backupVaultName :: Prelude.Maybe Prelude.Text,
    -- | The date and time that a recovery point is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The size, in bytes, of a backup.
    backupSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | A status code specifying the state of the recovery point.
    --
    -- @PARTIAL@ status indicates Backup could not create the recovery point
    -- before the backup window closed. To increase your backup plan window
    -- using the API, see
    -- <https://docs.aws.amazon.com/aws-backup/latest/devguide/API_UpdateBackupPlan.html UpdateBackupPlan>.
    -- You can also increase your backup plan window using the Console by
    -- choosing and editing your backup plan.
    --
    -- @EXPIRED@ status indicates that the recovery point has exceeded its
    -- retention period, but Backup lacks permission or is otherwise unable to
    -- delete it. To manually delete these recovery points, see
    -- <https://docs.aws.amazon.com/aws-backup/latest/devguide/gs-cleanup-resources.html#cleanup-backups Step 3: Delete the recovery points>
    -- in the /Clean up resources/ section of /Getting started/.
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
    -- | An Amazon Resource Name (ARN) that uniquely identifies the source vault
    -- where the resource was originally backed up in; for example,
    -- @arn:aws:backup:us-east-1:123456789012:vault:BackupVault@. If the
    -- recovery is restored to the same Amazon Web Services account or Region,
    -- this value will be @null@.
    sourceBackupVaultArn :: Prelude.Maybe Prelude.Text,
    -- | An ARN that uniquely identifies a saved resource. The format of the ARN
    -- depends on the resource type.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the storage class of the recovery point. Valid values are
    -- @WARM@ or @COLD@.
    storageClass :: Prelude.Maybe StorageClass,
    -- | A status message explaining the reason for the recovery point deletion
    -- failure.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | Contains identifying information about the creation of a recovery point,
    -- including the @BackupPlanArn@, @BackupPlanId@, @BackupPlanVersion@, and
    -- @BackupRuleId@ of the backup plan used to create it.
    createdBy :: Prelude.Maybe RecoveryPointCreator,
    -- | The date and time that a recovery point was last restored, in Unix
    -- format and Coordinated Universal Time (UTC). The value of
    -- @LastRestoreTime@ is accurate to milliseconds. For example, the value
    -- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
    lastRestoreTime :: Prelude.Maybe Data.POSIX,
    -- | A @CalculatedLifecycle@ object containing @DeleteAt@ and
    -- @MoveToColdStorageAt@ timestamps.
    calculatedLifecycle :: Prelude.Maybe CalculatedLifecycle,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRecoveryPointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionKeyArn', 'describeRecoveryPointResponse_encryptionKeyArn' - The server-side encryption key used to protect your backups; for
-- example,
-- @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
--
-- 'resourceType', 'describeRecoveryPointResponse_resourceType' - The type of Amazon Web Services resource to save as a recovery point;
-- for example, an Amazon Elastic Block Store (Amazon EBS) volume or an
-- Amazon Relational Database Service (Amazon RDS) database.
--
-- 'lifecycle', 'describeRecoveryPointResponse_lifecycle' - The lifecycle defines when a protected resource is transitioned to cold
-- storage and when it expires. Backup transitions and expires backups
-- automatically according to the lifecycle that you define.
--
-- Backups that are transitioned to cold storage must be stored in cold
-- storage for a minimum of 90 days. Therefore, the “retention” setting
-- must be 90 days greater than the “transition to cold after days”
-- setting. The “transition to cold after days” setting cannot be changed
-- after a backup has been transitioned to cold.
--
-- Resource types that are able to be transitioned to cold storage are
-- listed in the \"Lifecycle to cold storage\" section of the
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/whatisbackup.html#features-by-resource Feature availability by resource>
-- table. Backup ignores this expression for other resource types.
--
-- 'recoveryPointArn', 'describeRecoveryPointResponse_recoveryPointArn' - An ARN that uniquely identifies a recovery point; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
--
-- 'completionDate', 'describeRecoveryPointResponse_completionDate' - The date and time that a job to create a recovery point is completed, in
-- Unix format and Coordinated Universal Time (UTC). The value of
-- @CompletionDate@ is accurate to milliseconds. For example, the value
-- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'backupVaultName', 'describeRecoveryPointResponse_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Region where they are created. They consist of lowercase
-- letters, numbers, and hyphens.
--
-- 'creationDate', 'describeRecoveryPointResponse_creationDate' - The date and time that a recovery point is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'backupSizeInBytes', 'describeRecoveryPointResponse_backupSizeInBytes' - The size, in bytes, of a backup.
--
-- 'status', 'describeRecoveryPointResponse_status' - A status code specifying the state of the recovery point.
--
-- @PARTIAL@ status indicates Backup could not create the recovery point
-- before the backup window closed. To increase your backup plan window
-- using the API, see
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/API_UpdateBackupPlan.html UpdateBackupPlan>.
-- You can also increase your backup plan window using the Console by
-- choosing and editing your backup plan.
--
-- @EXPIRED@ status indicates that the recovery point has exceeded its
-- retention period, but Backup lacks permission or is otherwise unable to
-- delete it. To manually delete these recovery points, see
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/gs-cleanup-resources.html#cleanup-backups Step 3: Delete the recovery points>
-- in the /Clean up resources/ section of /Getting started/.
--
-- 'backupVaultArn', 'describeRecoveryPointResponse_backupVaultArn' - An ARN that uniquely identifies a backup vault; for example,
-- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
--
-- 'isEncrypted', 'describeRecoveryPointResponse_isEncrypted' - A Boolean value that is returned as @TRUE@ if the specified recovery
-- point is encrypted, or @FALSE@ if the recovery point is not encrypted.
--
-- 'iamRoleArn', 'describeRecoveryPointResponse_iamRoleArn' - Specifies the IAM role ARN used to create the target recovery point; for
-- example, @arn:aws:iam::123456789012:role\/S3Access@.
--
-- 'sourceBackupVaultArn', 'describeRecoveryPointResponse_sourceBackupVaultArn' - An Amazon Resource Name (ARN) that uniquely identifies the source vault
-- where the resource was originally backed up in; for example,
-- @arn:aws:backup:us-east-1:123456789012:vault:BackupVault@. If the
-- recovery is restored to the same Amazon Web Services account or Region,
-- this value will be @null@.
--
-- 'resourceArn', 'describeRecoveryPointResponse_resourceArn' - An ARN that uniquely identifies a saved resource. The format of the ARN
-- depends on the resource type.
--
-- 'storageClass', 'describeRecoveryPointResponse_storageClass' - Specifies the storage class of the recovery point. Valid values are
-- @WARM@ or @COLD@.
--
-- 'statusMessage', 'describeRecoveryPointResponse_statusMessage' - A status message explaining the reason for the recovery point deletion
-- failure.
--
-- 'createdBy', 'describeRecoveryPointResponse_createdBy' - Contains identifying information about the creation of a recovery point,
-- including the @BackupPlanArn@, @BackupPlanId@, @BackupPlanVersion@, and
-- @BackupRuleId@ of the backup plan used to create it.
--
-- 'lastRestoreTime', 'describeRecoveryPointResponse_lastRestoreTime' - The date and time that a recovery point was last restored, in Unix
-- format and Coordinated Universal Time (UTC). The value of
-- @LastRestoreTime@ is accurate to milliseconds. For example, the value
-- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'calculatedLifecycle', 'describeRecoveryPointResponse_calculatedLifecycle' - A @CalculatedLifecycle@ object containing @DeleteAt@ and
-- @MoveToColdStorageAt@ timestamps.
--
-- 'httpStatus', 'describeRecoveryPointResponse_httpStatus' - The response's http status code.
newDescribeRecoveryPointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRecoveryPointResponse
newDescribeRecoveryPointResponse pHttpStatus_ =
  DescribeRecoveryPointResponse'
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
      storageClass = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      lastRestoreTime = Prelude.Nothing,
      calculatedLifecycle = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The server-side encryption key used to protect your backups; for
-- example,
-- @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
describeRecoveryPointResponse_encryptionKeyArn :: Lens.Lens' DescribeRecoveryPointResponse (Prelude.Maybe Prelude.Text)
describeRecoveryPointResponse_encryptionKeyArn = Lens.lens (\DescribeRecoveryPointResponse' {encryptionKeyArn} -> encryptionKeyArn) (\s@DescribeRecoveryPointResponse' {} a -> s {encryptionKeyArn = a} :: DescribeRecoveryPointResponse)

-- | The type of Amazon Web Services resource to save as a recovery point;
-- for example, an Amazon Elastic Block Store (Amazon EBS) volume or an
-- Amazon Relational Database Service (Amazon RDS) database.
describeRecoveryPointResponse_resourceType :: Lens.Lens' DescribeRecoveryPointResponse (Prelude.Maybe Prelude.Text)
describeRecoveryPointResponse_resourceType = Lens.lens (\DescribeRecoveryPointResponse' {resourceType} -> resourceType) (\s@DescribeRecoveryPointResponse' {} a -> s {resourceType = a} :: DescribeRecoveryPointResponse)

-- | The lifecycle defines when a protected resource is transitioned to cold
-- storage and when it expires. Backup transitions and expires backups
-- automatically according to the lifecycle that you define.
--
-- Backups that are transitioned to cold storage must be stored in cold
-- storage for a minimum of 90 days. Therefore, the “retention” setting
-- must be 90 days greater than the “transition to cold after days”
-- setting. The “transition to cold after days” setting cannot be changed
-- after a backup has been transitioned to cold.
--
-- Resource types that are able to be transitioned to cold storage are
-- listed in the \"Lifecycle to cold storage\" section of the
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/whatisbackup.html#features-by-resource Feature availability by resource>
-- table. Backup ignores this expression for other resource types.
describeRecoveryPointResponse_lifecycle :: Lens.Lens' DescribeRecoveryPointResponse (Prelude.Maybe Lifecycle)
describeRecoveryPointResponse_lifecycle = Lens.lens (\DescribeRecoveryPointResponse' {lifecycle} -> lifecycle) (\s@DescribeRecoveryPointResponse' {} a -> s {lifecycle = a} :: DescribeRecoveryPointResponse)

-- | An ARN that uniquely identifies a recovery point; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
describeRecoveryPointResponse_recoveryPointArn :: Lens.Lens' DescribeRecoveryPointResponse (Prelude.Maybe Prelude.Text)
describeRecoveryPointResponse_recoveryPointArn = Lens.lens (\DescribeRecoveryPointResponse' {recoveryPointArn} -> recoveryPointArn) (\s@DescribeRecoveryPointResponse' {} a -> s {recoveryPointArn = a} :: DescribeRecoveryPointResponse)

-- | The date and time that a job to create a recovery point is completed, in
-- Unix format and Coordinated Universal Time (UTC). The value of
-- @CompletionDate@ is accurate to milliseconds. For example, the value
-- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
describeRecoveryPointResponse_completionDate :: Lens.Lens' DescribeRecoveryPointResponse (Prelude.Maybe Prelude.UTCTime)
describeRecoveryPointResponse_completionDate = Lens.lens (\DescribeRecoveryPointResponse' {completionDate} -> completionDate) (\s@DescribeRecoveryPointResponse' {} a -> s {completionDate = a} :: DescribeRecoveryPointResponse) Prelude.. Lens.mapping Data._Time

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Region where they are created. They consist of lowercase
-- letters, numbers, and hyphens.
describeRecoveryPointResponse_backupVaultName :: Lens.Lens' DescribeRecoveryPointResponse (Prelude.Maybe Prelude.Text)
describeRecoveryPointResponse_backupVaultName = Lens.lens (\DescribeRecoveryPointResponse' {backupVaultName} -> backupVaultName) (\s@DescribeRecoveryPointResponse' {} a -> s {backupVaultName = a} :: DescribeRecoveryPointResponse)

-- | The date and time that a recovery point is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
describeRecoveryPointResponse_creationDate :: Lens.Lens' DescribeRecoveryPointResponse (Prelude.Maybe Prelude.UTCTime)
describeRecoveryPointResponse_creationDate = Lens.lens (\DescribeRecoveryPointResponse' {creationDate} -> creationDate) (\s@DescribeRecoveryPointResponse' {} a -> s {creationDate = a} :: DescribeRecoveryPointResponse) Prelude.. Lens.mapping Data._Time

-- | The size, in bytes, of a backup.
describeRecoveryPointResponse_backupSizeInBytes :: Lens.Lens' DescribeRecoveryPointResponse (Prelude.Maybe Prelude.Integer)
describeRecoveryPointResponse_backupSizeInBytes = Lens.lens (\DescribeRecoveryPointResponse' {backupSizeInBytes} -> backupSizeInBytes) (\s@DescribeRecoveryPointResponse' {} a -> s {backupSizeInBytes = a} :: DescribeRecoveryPointResponse)

-- | A status code specifying the state of the recovery point.
--
-- @PARTIAL@ status indicates Backup could not create the recovery point
-- before the backup window closed. To increase your backup plan window
-- using the API, see
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/API_UpdateBackupPlan.html UpdateBackupPlan>.
-- You can also increase your backup plan window using the Console by
-- choosing and editing your backup plan.
--
-- @EXPIRED@ status indicates that the recovery point has exceeded its
-- retention period, but Backup lacks permission or is otherwise unable to
-- delete it. To manually delete these recovery points, see
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/gs-cleanup-resources.html#cleanup-backups Step 3: Delete the recovery points>
-- in the /Clean up resources/ section of /Getting started/.
describeRecoveryPointResponse_status :: Lens.Lens' DescribeRecoveryPointResponse (Prelude.Maybe RecoveryPointStatus)
describeRecoveryPointResponse_status = Lens.lens (\DescribeRecoveryPointResponse' {status} -> status) (\s@DescribeRecoveryPointResponse' {} a -> s {status = a} :: DescribeRecoveryPointResponse)

-- | An ARN that uniquely identifies a backup vault; for example,
-- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
describeRecoveryPointResponse_backupVaultArn :: Lens.Lens' DescribeRecoveryPointResponse (Prelude.Maybe Prelude.Text)
describeRecoveryPointResponse_backupVaultArn = Lens.lens (\DescribeRecoveryPointResponse' {backupVaultArn} -> backupVaultArn) (\s@DescribeRecoveryPointResponse' {} a -> s {backupVaultArn = a} :: DescribeRecoveryPointResponse)

-- | A Boolean value that is returned as @TRUE@ if the specified recovery
-- point is encrypted, or @FALSE@ if the recovery point is not encrypted.
describeRecoveryPointResponse_isEncrypted :: Lens.Lens' DescribeRecoveryPointResponse (Prelude.Maybe Prelude.Bool)
describeRecoveryPointResponse_isEncrypted = Lens.lens (\DescribeRecoveryPointResponse' {isEncrypted} -> isEncrypted) (\s@DescribeRecoveryPointResponse' {} a -> s {isEncrypted = a} :: DescribeRecoveryPointResponse)

-- | Specifies the IAM role ARN used to create the target recovery point; for
-- example, @arn:aws:iam::123456789012:role\/S3Access@.
describeRecoveryPointResponse_iamRoleArn :: Lens.Lens' DescribeRecoveryPointResponse (Prelude.Maybe Prelude.Text)
describeRecoveryPointResponse_iamRoleArn = Lens.lens (\DescribeRecoveryPointResponse' {iamRoleArn} -> iamRoleArn) (\s@DescribeRecoveryPointResponse' {} a -> s {iamRoleArn = a} :: DescribeRecoveryPointResponse)

-- | An Amazon Resource Name (ARN) that uniquely identifies the source vault
-- where the resource was originally backed up in; for example,
-- @arn:aws:backup:us-east-1:123456789012:vault:BackupVault@. If the
-- recovery is restored to the same Amazon Web Services account or Region,
-- this value will be @null@.
describeRecoveryPointResponse_sourceBackupVaultArn :: Lens.Lens' DescribeRecoveryPointResponse (Prelude.Maybe Prelude.Text)
describeRecoveryPointResponse_sourceBackupVaultArn = Lens.lens (\DescribeRecoveryPointResponse' {sourceBackupVaultArn} -> sourceBackupVaultArn) (\s@DescribeRecoveryPointResponse' {} a -> s {sourceBackupVaultArn = a} :: DescribeRecoveryPointResponse)

-- | An ARN that uniquely identifies a saved resource. The format of the ARN
-- depends on the resource type.
describeRecoveryPointResponse_resourceArn :: Lens.Lens' DescribeRecoveryPointResponse (Prelude.Maybe Prelude.Text)
describeRecoveryPointResponse_resourceArn = Lens.lens (\DescribeRecoveryPointResponse' {resourceArn} -> resourceArn) (\s@DescribeRecoveryPointResponse' {} a -> s {resourceArn = a} :: DescribeRecoveryPointResponse)

-- | Specifies the storage class of the recovery point. Valid values are
-- @WARM@ or @COLD@.
describeRecoveryPointResponse_storageClass :: Lens.Lens' DescribeRecoveryPointResponse (Prelude.Maybe StorageClass)
describeRecoveryPointResponse_storageClass = Lens.lens (\DescribeRecoveryPointResponse' {storageClass} -> storageClass) (\s@DescribeRecoveryPointResponse' {} a -> s {storageClass = a} :: DescribeRecoveryPointResponse)

-- | A status message explaining the reason for the recovery point deletion
-- failure.
describeRecoveryPointResponse_statusMessage :: Lens.Lens' DescribeRecoveryPointResponse (Prelude.Maybe Prelude.Text)
describeRecoveryPointResponse_statusMessage = Lens.lens (\DescribeRecoveryPointResponse' {statusMessage} -> statusMessage) (\s@DescribeRecoveryPointResponse' {} a -> s {statusMessage = a} :: DescribeRecoveryPointResponse)

-- | Contains identifying information about the creation of a recovery point,
-- including the @BackupPlanArn@, @BackupPlanId@, @BackupPlanVersion@, and
-- @BackupRuleId@ of the backup plan used to create it.
describeRecoveryPointResponse_createdBy :: Lens.Lens' DescribeRecoveryPointResponse (Prelude.Maybe RecoveryPointCreator)
describeRecoveryPointResponse_createdBy = Lens.lens (\DescribeRecoveryPointResponse' {createdBy} -> createdBy) (\s@DescribeRecoveryPointResponse' {} a -> s {createdBy = a} :: DescribeRecoveryPointResponse)

-- | The date and time that a recovery point was last restored, in Unix
-- format and Coordinated Universal Time (UTC). The value of
-- @LastRestoreTime@ is accurate to milliseconds. For example, the value
-- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
describeRecoveryPointResponse_lastRestoreTime :: Lens.Lens' DescribeRecoveryPointResponse (Prelude.Maybe Prelude.UTCTime)
describeRecoveryPointResponse_lastRestoreTime = Lens.lens (\DescribeRecoveryPointResponse' {lastRestoreTime} -> lastRestoreTime) (\s@DescribeRecoveryPointResponse' {} a -> s {lastRestoreTime = a} :: DescribeRecoveryPointResponse) Prelude.. Lens.mapping Data._Time

-- | A @CalculatedLifecycle@ object containing @DeleteAt@ and
-- @MoveToColdStorageAt@ timestamps.
describeRecoveryPointResponse_calculatedLifecycle :: Lens.Lens' DescribeRecoveryPointResponse (Prelude.Maybe CalculatedLifecycle)
describeRecoveryPointResponse_calculatedLifecycle = Lens.lens (\DescribeRecoveryPointResponse' {calculatedLifecycle} -> calculatedLifecycle) (\s@DescribeRecoveryPointResponse' {} a -> s {calculatedLifecycle = a} :: DescribeRecoveryPointResponse)

-- | The response's http status code.
describeRecoveryPointResponse_httpStatus :: Lens.Lens' DescribeRecoveryPointResponse Prelude.Int
describeRecoveryPointResponse_httpStatus = Lens.lens (\DescribeRecoveryPointResponse' {httpStatus} -> httpStatus) (\s@DescribeRecoveryPointResponse' {} a -> s {httpStatus = a} :: DescribeRecoveryPointResponse)

instance Prelude.NFData DescribeRecoveryPointResponse where
  rnf DescribeRecoveryPointResponse' {..} =
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
      `Prelude.seq` Prelude.rnf storageClass
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf lastRestoreTime
      `Prelude.seq` Prelude.rnf calculatedLifecycle
      `Prelude.seq` Prelude.rnf httpStatus
