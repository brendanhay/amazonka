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
-- Module      : Amazonka.Backup.StartCopyJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a job to create a one-time copy of the specified resource.
--
-- Does not support continuous backups.
module Amazonka.Backup.StartCopyJob
  ( -- * Creating a Request
    StartCopyJob (..),
    newStartCopyJob,

    -- * Request Lenses
    startCopyJob_idempotencyToken,
    startCopyJob_lifecycle,
    startCopyJob_recoveryPointArn,
    startCopyJob_sourceBackupVaultName,
    startCopyJob_destinationBackupVaultArn,
    startCopyJob_iamRoleArn,

    -- * Destructuring the Response
    StartCopyJobResponse (..),
    newStartCopyJobResponse,

    -- * Response Lenses
    startCopyJobResponse_copyJobId,
    startCopyJobResponse_creationDate,
    startCopyJobResponse_isParent,
    startCopyJobResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartCopyJob' smart constructor.
data StartCopyJob = StartCopyJob'
  { -- | A customer-chosen string that you can use to distinguish between
    -- otherwise identical calls to @StartCopyJob@. Retrying a successful
    -- request with the same idempotency token results in a success message
    -- with no action taken.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    lifecycle :: Prelude.Maybe Lifecycle,
    -- | An ARN that uniquely identifies a recovery point to use for the copy
    -- job; for example,
    -- arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45.
    recoveryPointArn :: Prelude.Text,
    -- | The name of a logical source container where backups are stored. Backup
    -- vaults are identified by names that are unique to the account used to
    -- create them and the Amazon Web Services Region where they are created.
    -- They consist of lowercase letters, numbers, and hyphens.
    sourceBackupVaultName :: Prelude.Text,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a destination
    -- backup vault to copy to; for example,
    -- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
    destinationBackupVaultArn :: Prelude.Text,
    -- | Specifies the IAM role ARN used to copy the target recovery point; for
    -- example, @arn:aws:iam::123456789012:role\/S3Access@.
    iamRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartCopyJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'idempotencyToken', 'startCopyJob_idempotencyToken' - A customer-chosen string that you can use to distinguish between
-- otherwise identical calls to @StartCopyJob@. Retrying a successful
-- request with the same idempotency token results in a success message
-- with no action taken.
--
-- 'lifecycle', 'startCopyJob_lifecycle' - Undocumented member.
--
-- 'recoveryPointArn', 'startCopyJob_recoveryPointArn' - An ARN that uniquely identifies a recovery point to use for the copy
-- job; for example,
-- arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45.
--
-- 'sourceBackupVaultName', 'startCopyJob_sourceBackupVaultName' - The name of a logical source container where backups are stored. Backup
-- vaults are identified by names that are unique to the account used to
-- create them and the Amazon Web Services Region where they are created.
-- They consist of lowercase letters, numbers, and hyphens.
--
-- 'destinationBackupVaultArn', 'startCopyJob_destinationBackupVaultArn' - An Amazon Resource Name (ARN) that uniquely identifies a destination
-- backup vault to copy to; for example,
-- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
--
-- 'iamRoleArn', 'startCopyJob_iamRoleArn' - Specifies the IAM role ARN used to copy the target recovery point; for
-- example, @arn:aws:iam::123456789012:role\/S3Access@.
newStartCopyJob ::
  -- | 'recoveryPointArn'
  Prelude.Text ->
  -- | 'sourceBackupVaultName'
  Prelude.Text ->
  -- | 'destinationBackupVaultArn'
  Prelude.Text ->
  -- | 'iamRoleArn'
  Prelude.Text ->
  StartCopyJob
newStartCopyJob
  pRecoveryPointArn_
  pSourceBackupVaultName_
  pDestinationBackupVaultArn_
  pIamRoleArn_ =
    StartCopyJob'
      { idempotencyToken = Prelude.Nothing,
        lifecycle = Prelude.Nothing,
        recoveryPointArn = pRecoveryPointArn_,
        sourceBackupVaultName = pSourceBackupVaultName_,
        destinationBackupVaultArn =
          pDestinationBackupVaultArn_,
        iamRoleArn = pIamRoleArn_
      }

-- | A customer-chosen string that you can use to distinguish between
-- otherwise identical calls to @StartCopyJob@. Retrying a successful
-- request with the same idempotency token results in a success message
-- with no action taken.
startCopyJob_idempotencyToken :: Lens.Lens' StartCopyJob (Prelude.Maybe Prelude.Text)
startCopyJob_idempotencyToken = Lens.lens (\StartCopyJob' {idempotencyToken} -> idempotencyToken) (\s@StartCopyJob' {} a -> s {idempotencyToken = a} :: StartCopyJob)

-- | Undocumented member.
startCopyJob_lifecycle :: Lens.Lens' StartCopyJob (Prelude.Maybe Lifecycle)
startCopyJob_lifecycle = Lens.lens (\StartCopyJob' {lifecycle} -> lifecycle) (\s@StartCopyJob' {} a -> s {lifecycle = a} :: StartCopyJob)

-- | An ARN that uniquely identifies a recovery point to use for the copy
-- job; for example,
-- arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45.
startCopyJob_recoveryPointArn :: Lens.Lens' StartCopyJob Prelude.Text
startCopyJob_recoveryPointArn = Lens.lens (\StartCopyJob' {recoveryPointArn} -> recoveryPointArn) (\s@StartCopyJob' {} a -> s {recoveryPointArn = a} :: StartCopyJob)

-- | The name of a logical source container where backups are stored. Backup
-- vaults are identified by names that are unique to the account used to
-- create them and the Amazon Web Services Region where they are created.
-- They consist of lowercase letters, numbers, and hyphens.
startCopyJob_sourceBackupVaultName :: Lens.Lens' StartCopyJob Prelude.Text
startCopyJob_sourceBackupVaultName = Lens.lens (\StartCopyJob' {sourceBackupVaultName} -> sourceBackupVaultName) (\s@StartCopyJob' {} a -> s {sourceBackupVaultName = a} :: StartCopyJob)

-- | An Amazon Resource Name (ARN) that uniquely identifies a destination
-- backup vault to copy to; for example,
-- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
startCopyJob_destinationBackupVaultArn :: Lens.Lens' StartCopyJob Prelude.Text
startCopyJob_destinationBackupVaultArn = Lens.lens (\StartCopyJob' {destinationBackupVaultArn} -> destinationBackupVaultArn) (\s@StartCopyJob' {} a -> s {destinationBackupVaultArn = a} :: StartCopyJob)

-- | Specifies the IAM role ARN used to copy the target recovery point; for
-- example, @arn:aws:iam::123456789012:role\/S3Access@.
startCopyJob_iamRoleArn :: Lens.Lens' StartCopyJob Prelude.Text
startCopyJob_iamRoleArn = Lens.lens (\StartCopyJob' {iamRoleArn} -> iamRoleArn) (\s@StartCopyJob' {} a -> s {iamRoleArn = a} :: StartCopyJob)

instance Core.AWSRequest StartCopyJob where
  type AWSResponse StartCopyJob = StartCopyJobResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartCopyJobResponse'
            Prelude.<$> (x Data..?> "CopyJobId")
            Prelude.<*> (x Data..?> "CreationDate")
            Prelude.<*> (x Data..?> "IsParent")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartCopyJob where
  hashWithSalt _salt StartCopyJob' {..} =
    _salt
      `Prelude.hashWithSalt` idempotencyToken
      `Prelude.hashWithSalt` lifecycle
      `Prelude.hashWithSalt` recoveryPointArn
      `Prelude.hashWithSalt` sourceBackupVaultName
      `Prelude.hashWithSalt` destinationBackupVaultArn
      `Prelude.hashWithSalt` iamRoleArn

instance Prelude.NFData StartCopyJob where
  rnf StartCopyJob' {..} =
    Prelude.rnf idempotencyToken
      `Prelude.seq` Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf recoveryPointArn
      `Prelude.seq` Prelude.rnf sourceBackupVaultName
      `Prelude.seq` Prelude.rnf destinationBackupVaultArn
      `Prelude.seq` Prelude.rnf iamRoleArn

instance Data.ToHeaders StartCopyJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartCopyJob where
  toJSON StartCopyJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IdempotencyToken" Data..=)
              Prelude.<$> idempotencyToken,
            ("Lifecycle" Data..=) Prelude.<$> lifecycle,
            Prelude.Just
              ("RecoveryPointArn" Data..= recoveryPointArn),
            Prelude.Just
              ( "SourceBackupVaultName"
                  Data..= sourceBackupVaultName
              ),
            Prelude.Just
              ( "DestinationBackupVaultArn"
                  Data..= destinationBackupVaultArn
              ),
            Prelude.Just ("IamRoleArn" Data..= iamRoleArn)
          ]
      )

instance Data.ToPath StartCopyJob where
  toPath = Prelude.const "/copy-jobs"

instance Data.ToQuery StartCopyJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartCopyJobResponse' smart constructor.
data StartCopyJobResponse = StartCopyJobResponse'
  { -- | Uniquely identifies a copy job.
    copyJobId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that a copy job is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | This is a returned boolean value indicating this is a parent (composite)
    -- copy job.
    isParent :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartCopyJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'copyJobId', 'startCopyJobResponse_copyJobId' - Uniquely identifies a copy job.
--
-- 'creationDate', 'startCopyJobResponse_creationDate' - The date and time that a copy job is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'isParent', 'startCopyJobResponse_isParent' - This is a returned boolean value indicating this is a parent (composite)
-- copy job.
--
-- 'httpStatus', 'startCopyJobResponse_httpStatus' - The response's http status code.
newStartCopyJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartCopyJobResponse
newStartCopyJobResponse pHttpStatus_ =
  StartCopyJobResponse'
    { copyJobId = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      isParent = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Uniquely identifies a copy job.
startCopyJobResponse_copyJobId :: Lens.Lens' StartCopyJobResponse (Prelude.Maybe Prelude.Text)
startCopyJobResponse_copyJobId = Lens.lens (\StartCopyJobResponse' {copyJobId} -> copyJobId) (\s@StartCopyJobResponse' {} a -> s {copyJobId = a} :: StartCopyJobResponse)

-- | The date and time that a copy job is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
startCopyJobResponse_creationDate :: Lens.Lens' StartCopyJobResponse (Prelude.Maybe Prelude.UTCTime)
startCopyJobResponse_creationDate = Lens.lens (\StartCopyJobResponse' {creationDate} -> creationDate) (\s@StartCopyJobResponse' {} a -> s {creationDate = a} :: StartCopyJobResponse) Prelude.. Lens.mapping Data._Time

-- | This is a returned boolean value indicating this is a parent (composite)
-- copy job.
startCopyJobResponse_isParent :: Lens.Lens' StartCopyJobResponse (Prelude.Maybe Prelude.Bool)
startCopyJobResponse_isParent = Lens.lens (\StartCopyJobResponse' {isParent} -> isParent) (\s@StartCopyJobResponse' {} a -> s {isParent = a} :: StartCopyJobResponse)

-- | The response's http status code.
startCopyJobResponse_httpStatus :: Lens.Lens' StartCopyJobResponse Prelude.Int
startCopyJobResponse_httpStatus = Lens.lens (\StartCopyJobResponse' {httpStatus} -> httpStatus) (\s@StartCopyJobResponse' {} a -> s {httpStatus = a} :: StartCopyJobResponse)

instance Prelude.NFData StartCopyJobResponse where
  rnf StartCopyJobResponse' {..} =
    Prelude.rnf copyJobId
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf isParent
      `Prelude.seq` Prelude.rnf httpStatus
