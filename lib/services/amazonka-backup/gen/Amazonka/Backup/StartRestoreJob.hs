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
-- Module      : Amazonka.Backup.StartRestoreJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Recovers the saved resource identified by an Amazon Resource Name (ARN).
module Amazonka.Backup.StartRestoreJob
  ( -- * Creating a Request
    StartRestoreJob (..),
    newStartRestoreJob,

    -- * Request Lenses
    startRestoreJob_iamRoleArn,
    startRestoreJob_idempotencyToken,
    startRestoreJob_resourceType,
    startRestoreJob_recoveryPointArn,
    startRestoreJob_metadata,

    -- * Destructuring the Response
    StartRestoreJobResponse (..),
    newStartRestoreJobResponse,

    -- * Response Lenses
    startRestoreJobResponse_restoreJobId,
    startRestoreJobResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartRestoreJob' smart constructor.
data StartRestoreJob = StartRestoreJob'
  { -- | The Amazon Resource Name (ARN) of the IAM role that Backup uses to
    -- create the target resource; for example:
    -- @arn:aws:iam::123456789012:role\/S3Access@.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A customer-chosen string that you can use to distinguish between
    -- otherwise identical calls to @StartRestoreJob@. Retrying a successful
    -- request with the same idempotency token results in a success message
    -- with no action taken.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    -- | Starts a job to restore a recovery point for one of the following
    -- resources:
    --
    -- -   @Aurora@ for Amazon Aurora
    --
    -- -   @DocumentDB@ for Amazon DocumentDB (with MongoDB compatibility)
    --
    -- -   @DynamoDB@ for Amazon DynamoDB
    --
    -- -   @EBS@ for Amazon Elastic Block Store
    --
    -- -   @EC2@ for Amazon Elastic Compute Cloud
    --
    -- -   @EFS@ for Amazon Elastic File System
    --
    -- -   @FSx@ for Amazon FSx
    --
    -- -   @Neptune@ for Amazon Neptune
    --
    -- -   @RDS@ for Amazon Relational Database Service
    --
    -- -   @Storage Gateway@ for Storage Gateway
    --
    -- -   @S3@ for Amazon S3
    --
    -- -   @VirtualMachine@ for virtual machines
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | An ARN that uniquely identifies a recovery point; for example,
    -- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
    recoveryPointArn :: Prelude.Text,
    -- | A set of metadata key-value pairs. Contains information, such as a
    -- resource name, required to restore a recovery point.
    --
    -- You can get configuration metadata about a resource at the time it was
    -- backed up by calling @GetRecoveryPointRestoreMetadata@. However, values
    -- in addition to those provided by @GetRecoveryPointRestoreMetadata@ might
    -- be required to restore a resource. For example, you might need to
    -- provide a new resource name if the original already exists.
    --
    -- You need to specify specific metadata to restore an Amazon Elastic File
    -- System (Amazon EFS) instance:
    --
    -- -   @file-system-id@: The ID of the Amazon EFS file system that is
    --     backed up by Backup. Returned in @GetRecoveryPointRestoreMetadata@.
    --
    -- -   @Encrypted@: A Boolean value that, if true, specifies that the file
    --     system is encrypted. If @KmsKeyId@ is specified, @Encrypted@ must be
    --     set to @true@.
    --
    -- -   @KmsKeyId@: Specifies the Amazon Web Services KMS key that is used
    --     to encrypt the restored file system. You can specify a key from
    --     another Amazon Web Services account provided that key it is properly
    --     shared with your account via Amazon Web Services KMS.
    --
    -- -   @PerformanceMode@: Specifies the throughput mode of the file system.
    --
    -- -   @CreationToken@: A user-supplied value that ensures the uniqueness
    --     (idempotency) of the request.
    --
    -- -   @newFileSystem@: A Boolean value that, if true, specifies that the
    --     recovery point is restored to a new Amazon EFS file system.
    --
    -- -   @ItemsToRestore@: An array of one to five strings where each string
    --     is a file path. Use @ItemsToRestore@ to restore specific files or
    --     directories rather than the entire file system. This parameter is
    --     optional. For example, @\"itemsToRestore\":\"[\\\"\/my.test\\\"]\"@.
    metadata :: Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartRestoreJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamRoleArn', 'startRestoreJob_iamRoleArn' - The Amazon Resource Name (ARN) of the IAM role that Backup uses to
-- create the target resource; for example:
-- @arn:aws:iam::123456789012:role\/S3Access@.
--
-- 'idempotencyToken', 'startRestoreJob_idempotencyToken' - A customer-chosen string that you can use to distinguish between
-- otherwise identical calls to @StartRestoreJob@. Retrying a successful
-- request with the same idempotency token results in a success message
-- with no action taken.
--
-- 'resourceType', 'startRestoreJob_resourceType' - Starts a job to restore a recovery point for one of the following
-- resources:
--
-- -   @Aurora@ for Amazon Aurora
--
-- -   @DocumentDB@ for Amazon DocumentDB (with MongoDB compatibility)
--
-- -   @DynamoDB@ for Amazon DynamoDB
--
-- -   @EBS@ for Amazon Elastic Block Store
--
-- -   @EC2@ for Amazon Elastic Compute Cloud
--
-- -   @EFS@ for Amazon Elastic File System
--
-- -   @FSx@ for Amazon FSx
--
-- -   @Neptune@ for Amazon Neptune
--
-- -   @RDS@ for Amazon Relational Database Service
--
-- -   @Storage Gateway@ for Storage Gateway
--
-- -   @S3@ for Amazon S3
--
-- -   @VirtualMachine@ for virtual machines
--
-- 'recoveryPointArn', 'startRestoreJob_recoveryPointArn' - An ARN that uniquely identifies a recovery point; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
--
-- 'metadata', 'startRestoreJob_metadata' - A set of metadata key-value pairs. Contains information, such as a
-- resource name, required to restore a recovery point.
--
-- You can get configuration metadata about a resource at the time it was
-- backed up by calling @GetRecoveryPointRestoreMetadata@. However, values
-- in addition to those provided by @GetRecoveryPointRestoreMetadata@ might
-- be required to restore a resource. For example, you might need to
-- provide a new resource name if the original already exists.
--
-- You need to specify specific metadata to restore an Amazon Elastic File
-- System (Amazon EFS) instance:
--
-- -   @file-system-id@: The ID of the Amazon EFS file system that is
--     backed up by Backup. Returned in @GetRecoveryPointRestoreMetadata@.
--
-- -   @Encrypted@: A Boolean value that, if true, specifies that the file
--     system is encrypted. If @KmsKeyId@ is specified, @Encrypted@ must be
--     set to @true@.
--
-- -   @KmsKeyId@: Specifies the Amazon Web Services KMS key that is used
--     to encrypt the restored file system. You can specify a key from
--     another Amazon Web Services account provided that key it is properly
--     shared with your account via Amazon Web Services KMS.
--
-- -   @PerformanceMode@: Specifies the throughput mode of the file system.
--
-- -   @CreationToken@: A user-supplied value that ensures the uniqueness
--     (idempotency) of the request.
--
-- -   @newFileSystem@: A Boolean value that, if true, specifies that the
--     recovery point is restored to a new Amazon EFS file system.
--
-- -   @ItemsToRestore@: An array of one to five strings where each string
--     is a file path. Use @ItemsToRestore@ to restore specific files or
--     directories rather than the entire file system. This parameter is
--     optional. For example, @\"itemsToRestore\":\"[\\\"\/my.test\\\"]\"@.
newStartRestoreJob ::
  -- | 'recoveryPointArn'
  Prelude.Text ->
  StartRestoreJob
newStartRestoreJob pRecoveryPointArn_ =
  StartRestoreJob'
    { iamRoleArn = Prelude.Nothing,
      idempotencyToken = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      recoveryPointArn = pRecoveryPointArn_,
      metadata = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the IAM role that Backup uses to
-- create the target resource; for example:
-- @arn:aws:iam::123456789012:role\/S3Access@.
startRestoreJob_iamRoleArn :: Lens.Lens' StartRestoreJob (Prelude.Maybe Prelude.Text)
startRestoreJob_iamRoleArn = Lens.lens (\StartRestoreJob' {iamRoleArn} -> iamRoleArn) (\s@StartRestoreJob' {} a -> s {iamRoleArn = a} :: StartRestoreJob)

-- | A customer-chosen string that you can use to distinguish between
-- otherwise identical calls to @StartRestoreJob@. Retrying a successful
-- request with the same idempotency token results in a success message
-- with no action taken.
startRestoreJob_idempotencyToken :: Lens.Lens' StartRestoreJob (Prelude.Maybe Prelude.Text)
startRestoreJob_idempotencyToken = Lens.lens (\StartRestoreJob' {idempotencyToken} -> idempotencyToken) (\s@StartRestoreJob' {} a -> s {idempotencyToken = a} :: StartRestoreJob)

-- | Starts a job to restore a recovery point for one of the following
-- resources:
--
-- -   @Aurora@ for Amazon Aurora
--
-- -   @DocumentDB@ for Amazon DocumentDB (with MongoDB compatibility)
--
-- -   @DynamoDB@ for Amazon DynamoDB
--
-- -   @EBS@ for Amazon Elastic Block Store
--
-- -   @EC2@ for Amazon Elastic Compute Cloud
--
-- -   @EFS@ for Amazon Elastic File System
--
-- -   @FSx@ for Amazon FSx
--
-- -   @Neptune@ for Amazon Neptune
--
-- -   @RDS@ for Amazon Relational Database Service
--
-- -   @Storage Gateway@ for Storage Gateway
--
-- -   @S3@ for Amazon S3
--
-- -   @VirtualMachine@ for virtual machines
startRestoreJob_resourceType :: Lens.Lens' StartRestoreJob (Prelude.Maybe Prelude.Text)
startRestoreJob_resourceType = Lens.lens (\StartRestoreJob' {resourceType} -> resourceType) (\s@StartRestoreJob' {} a -> s {resourceType = a} :: StartRestoreJob)

-- | An ARN that uniquely identifies a recovery point; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
startRestoreJob_recoveryPointArn :: Lens.Lens' StartRestoreJob Prelude.Text
startRestoreJob_recoveryPointArn = Lens.lens (\StartRestoreJob' {recoveryPointArn} -> recoveryPointArn) (\s@StartRestoreJob' {} a -> s {recoveryPointArn = a} :: StartRestoreJob)

-- | A set of metadata key-value pairs. Contains information, such as a
-- resource name, required to restore a recovery point.
--
-- You can get configuration metadata about a resource at the time it was
-- backed up by calling @GetRecoveryPointRestoreMetadata@. However, values
-- in addition to those provided by @GetRecoveryPointRestoreMetadata@ might
-- be required to restore a resource. For example, you might need to
-- provide a new resource name if the original already exists.
--
-- You need to specify specific metadata to restore an Amazon Elastic File
-- System (Amazon EFS) instance:
--
-- -   @file-system-id@: The ID of the Amazon EFS file system that is
--     backed up by Backup. Returned in @GetRecoveryPointRestoreMetadata@.
--
-- -   @Encrypted@: A Boolean value that, if true, specifies that the file
--     system is encrypted. If @KmsKeyId@ is specified, @Encrypted@ must be
--     set to @true@.
--
-- -   @KmsKeyId@: Specifies the Amazon Web Services KMS key that is used
--     to encrypt the restored file system. You can specify a key from
--     another Amazon Web Services account provided that key it is properly
--     shared with your account via Amazon Web Services KMS.
--
-- -   @PerformanceMode@: Specifies the throughput mode of the file system.
--
-- -   @CreationToken@: A user-supplied value that ensures the uniqueness
--     (idempotency) of the request.
--
-- -   @newFileSystem@: A Boolean value that, if true, specifies that the
--     recovery point is restored to a new Amazon EFS file system.
--
-- -   @ItemsToRestore@: An array of one to five strings where each string
--     is a file path. Use @ItemsToRestore@ to restore specific files or
--     directories rather than the entire file system. This parameter is
--     optional. For example, @\"itemsToRestore\":\"[\\\"\/my.test\\\"]\"@.
startRestoreJob_metadata :: Lens.Lens' StartRestoreJob (Prelude.HashMap Prelude.Text Prelude.Text)
startRestoreJob_metadata = Lens.lens (\StartRestoreJob' {metadata} -> metadata) (\s@StartRestoreJob' {} a -> s {metadata = a} :: StartRestoreJob) Prelude.. Data._Sensitive Prelude.. Lens.coerced

instance Core.AWSRequest StartRestoreJob where
  type
    AWSResponse StartRestoreJob =
      StartRestoreJobResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartRestoreJobResponse'
            Prelude.<$> (x Data..?> "RestoreJobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartRestoreJob where
  hashWithSalt _salt StartRestoreJob' {..} =
    _salt
      `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` idempotencyToken
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` recoveryPointArn
      `Prelude.hashWithSalt` metadata

instance Prelude.NFData StartRestoreJob where
  rnf StartRestoreJob' {..} =
    Prelude.rnf iamRoleArn `Prelude.seq`
      Prelude.rnf idempotencyToken `Prelude.seq`
        Prelude.rnf resourceType `Prelude.seq`
          Prelude.rnf recoveryPointArn `Prelude.seq`
            Prelude.rnf metadata

instance Data.ToHeaders StartRestoreJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartRestoreJob where
  toJSON StartRestoreJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IamRoleArn" Data..=) Prelude.<$> iamRoleArn,
            ("IdempotencyToken" Data..=)
              Prelude.<$> idempotencyToken,
            ("ResourceType" Data..=) Prelude.<$> resourceType,
            Prelude.Just
              ("RecoveryPointArn" Data..= recoveryPointArn),
            Prelude.Just ("Metadata" Data..= metadata)
          ]
      )

instance Data.ToPath StartRestoreJob where
  toPath = Prelude.const "/restore-jobs"

instance Data.ToQuery StartRestoreJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartRestoreJobResponse' smart constructor.
data StartRestoreJobResponse = StartRestoreJobResponse'
  { -- | Uniquely identifies the job that restores a recovery point.
    restoreJobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartRestoreJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restoreJobId', 'startRestoreJobResponse_restoreJobId' - Uniquely identifies the job that restores a recovery point.
--
-- 'httpStatus', 'startRestoreJobResponse_httpStatus' - The response's http status code.
newStartRestoreJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartRestoreJobResponse
newStartRestoreJobResponse pHttpStatus_ =
  StartRestoreJobResponse'
    { restoreJobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Uniquely identifies the job that restores a recovery point.
startRestoreJobResponse_restoreJobId :: Lens.Lens' StartRestoreJobResponse (Prelude.Maybe Prelude.Text)
startRestoreJobResponse_restoreJobId = Lens.lens (\StartRestoreJobResponse' {restoreJobId} -> restoreJobId) (\s@StartRestoreJobResponse' {} a -> s {restoreJobId = a} :: StartRestoreJobResponse)

-- | The response's http status code.
startRestoreJobResponse_httpStatus :: Lens.Lens' StartRestoreJobResponse Prelude.Int
startRestoreJobResponse_httpStatus = Lens.lens (\StartRestoreJobResponse' {httpStatus} -> httpStatus) (\s@StartRestoreJobResponse' {} a -> s {httpStatus = a} :: StartRestoreJobResponse)

instance Prelude.NFData StartRestoreJobResponse where
  rnf StartRestoreJobResponse' {..} =
    Prelude.rnf restoreJobId `Prelude.seq`
      Prelude.rnf httpStatus
