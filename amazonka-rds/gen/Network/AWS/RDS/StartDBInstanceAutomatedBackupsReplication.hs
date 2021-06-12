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
-- Module      : Network.AWS.RDS.StartDBInstanceAutomatedBackupsReplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables replication of automated backups to a different AWS Region.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_ReplicateBackups.html Replicating Automated Backups to Another AWS Region>
-- in the /Amazon RDS User Guide./
module Network.AWS.RDS.StartDBInstanceAutomatedBackupsReplication
  ( -- * Creating a Request
    StartDBInstanceAutomatedBackupsReplication (..),
    newStartDBInstanceAutomatedBackupsReplication,

    -- * Request Lenses
    startDBInstanceAutomatedBackupsReplication_backupRetentionPeriod,
    startDBInstanceAutomatedBackupsReplication_kmsKeyId,
    startDBInstanceAutomatedBackupsReplication_preSignedUrl,
    startDBInstanceAutomatedBackupsReplication_sourceDBInstanceArn,

    -- * Destructuring the Response
    StartDBInstanceAutomatedBackupsReplicationResponse (..),
    newStartDBInstanceAutomatedBackupsReplicationResponse,

    -- * Response Lenses
    startDBInstanceAutomatedBackupsReplicationResponse_dbInstanceAutomatedBackup,
    startDBInstanceAutomatedBackupsReplicationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartDBInstanceAutomatedBackupsReplication' smart constructor.
data StartDBInstanceAutomatedBackupsReplication = StartDBInstanceAutomatedBackupsReplication'
  { -- | The retention period for the replicated automated backups.
    backupRetentionPeriod :: Core.Maybe Core.Int,
    -- | The AWS KMS key identifier for encryption of the replicated automated
    -- backups. The KMS key ID is the Amazon Resource Name (ARN) for the KMS
    -- encryption key in the destination AWS Region, for example,
    -- @arn:aws:kms:us-east-1:123456789012:key\/AKIAIOSFODNN7EXAMPLE@.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | A URL that contains a Signature Version 4 signed request for the
    -- StartDBInstanceAutomatedBackupsReplication action to be called in the
    -- AWS Region of the source DB instance. The presigned URL must be a valid
    -- request for the StartDBInstanceAutomatedBackupsReplication API action
    -- that can be executed in the AWS Region that contains the source DB
    -- instance.
    preSignedUrl :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the source DB instance for the
    -- replicated automated backups, for example,
    -- @arn:aws:rds:us-west-2:123456789012:db:mydatabase@.
    sourceDBInstanceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartDBInstanceAutomatedBackupsReplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupRetentionPeriod', 'startDBInstanceAutomatedBackupsReplication_backupRetentionPeriod' - The retention period for the replicated automated backups.
--
-- 'kmsKeyId', 'startDBInstanceAutomatedBackupsReplication_kmsKeyId' - The AWS KMS key identifier for encryption of the replicated automated
-- backups. The KMS key ID is the Amazon Resource Name (ARN) for the KMS
-- encryption key in the destination AWS Region, for example,
-- @arn:aws:kms:us-east-1:123456789012:key\/AKIAIOSFODNN7EXAMPLE@.
--
-- 'preSignedUrl', 'startDBInstanceAutomatedBackupsReplication_preSignedUrl' - A URL that contains a Signature Version 4 signed request for the
-- StartDBInstanceAutomatedBackupsReplication action to be called in the
-- AWS Region of the source DB instance. The presigned URL must be a valid
-- request for the StartDBInstanceAutomatedBackupsReplication API action
-- that can be executed in the AWS Region that contains the source DB
-- instance.
--
-- 'sourceDBInstanceArn', 'startDBInstanceAutomatedBackupsReplication_sourceDBInstanceArn' - The Amazon Resource Name (ARN) of the source DB instance for the
-- replicated automated backups, for example,
-- @arn:aws:rds:us-west-2:123456789012:db:mydatabase@.
newStartDBInstanceAutomatedBackupsReplication ::
  -- | 'sourceDBInstanceArn'
  Core.Text ->
  StartDBInstanceAutomatedBackupsReplication
newStartDBInstanceAutomatedBackupsReplication
  pSourceDBInstanceArn_ =
    StartDBInstanceAutomatedBackupsReplication'
      { backupRetentionPeriod =
          Core.Nothing,
        kmsKeyId = Core.Nothing,
        preSignedUrl = Core.Nothing,
        sourceDBInstanceArn =
          pSourceDBInstanceArn_
      }

-- | The retention period for the replicated automated backups.
startDBInstanceAutomatedBackupsReplication_backupRetentionPeriod :: Lens.Lens' StartDBInstanceAutomatedBackupsReplication (Core.Maybe Core.Int)
startDBInstanceAutomatedBackupsReplication_backupRetentionPeriod = Lens.lens (\StartDBInstanceAutomatedBackupsReplication' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@StartDBInstanceAutomatedBackupsReplication' {} a -> s {backupRetentionPeriod = a} :: StartDBInstanceAutomatedBackupsReplication)

-- | The AWS KMS key identifier for encryption of the replicated automated
-- backups. The KMS key ID is the Amazon Resource Name (ARN) for the KMS
-- encryption key in the destination AWS Region, for example,
-- @arn:aws:kms:us-east-1:123456789012:key\/AKIAIOSFODNN7EXAMPLE@.
startDBInstanceAutomatedBackupsReplication_kmsKeyId :: Lens.Lens' StartDBInstanceAutomatedBackupsReplication (Core.Maybe Core.Text)
startDBInstanceAutomatedBackupsReplication_kmsKeyId = Lens.lens (\StartDBInstanceAutomatedBackupsReplication' {kmsKeyId} -> kmsKeyId) (\s@StartDBInstanceAutomatedBackupsReplication' {} a -> s {kmsKeyId = a} :: StartDBInstanceAutomatedBackupsReplication)

-- | A URL that contains a Signature Version 4 signed request for the
-- StartDBInstanceAutomatedBackupsReplication action to be called in the
-- AWS Region of the source DB instance. The presigned URL must be a valid
-- request for the StartDBInstanceAutomatedBackupsReplication API action
-- that can be executed in the AWS Region that contains the source DB
-- instance.
startDBInstanceAutomatedBackupsReplication_preSignedUrl :: Lens.Lens' StartDBInstanceAutomatedBackupsReplication (Core.Maybe Core.Text)
startDBInstanceAutomatedBackupsReplication_preSignedUrl = Lens.lens (\StartDBInstanceAutomatedBackupsReplication' {preSignedUrl} -> preSignedUrl) (\s@StartDBInstanceAutomatedBackupsReplication' {} a -> s {preSignedUrl = a} :: StartDBInstanceAutomatedBackupsReplication)

-- | The Amazon Resource Name (ARN) of the source DB instance for the
-- replicated automated backups, for example,
-- @arn:aws:rds:us-west-2:123456789012:db:mydatabase@.
startDBInstanceAutomatedBackupsReplication_sourceDBInstanceArn :: Lens.Lens' StartDBInstanceAutomatedBackupsReplication Core.Text
startDBInstanceAutomatedBackupsReplication_sourceDBInstanceArn = Lens.lens (\StartDBInstanceAutomatedBackupsReplication' {sourceDBInstanceArn} -> sourceDBInstanceArn) (\s@StartDBInstanceAutomatedBackupsReplication' {} a -> s {sourceDBInstanceArn = a} :: StartDBInstanceAutomatedBackupsReplication)

instance
  Core.AWSRequest
    StartDBInstanceAutomatedBackupsReplication
  where
  type
    AWSResponse
      StartDBInstanceAutomatedBackupsReplication =
      StartDBInstanceAutomatedBackupsReplicationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "StartDBInstanceAutomatedBackupsReplicationResult"
      ( \s h x ->
          StartDBInstanceAutomatedBackupsReplicationResponse'
            Core.<$> (x Core..@? "DBInstanceAutomatedBackup")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    StartDBInstanceAutomatedBackupsReplication

instance
  Core.NFData
    StartDBInstanceAutomatedBackupsReplication

instance
  Core.ToHeaders
    StartDBInstanceAutomatedBackupsReplication
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    StartDBInstanceAutomatedBackupsReplication
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    StartDBInstanceAutomatedBackupsReplication
  where
  toQuery
    StartDBInstanceAutomatedBackupsReplication' {..} =
      Core.mconcat
        [ "Action"
            Core.=: ( "StartDBInstanceAutomatedBackupsReplication" ::
                        Core.ByteString
                    ),
          "Version" Core.=: ("2014-10-31" :: Core.ByteString),
          "BackupRetentionPeriod"
            Core.=: backupRetentionPeriod,
          "KmsKeyId" Core.=: kmsKeyId,
          "PreSignedUrl" Core.=: preSignedUrl,
          "SourceDBInstanceArn" Core.=: sourceDBInstanceArn
        ]

-- | /See:/ 'newStartDBInstanceAutomatedBackupsReplicationResponse' smart constructor.
data StartDBInstanceAutomatedBackupsReplicationResponse = StartDBInstanceAutomatedBackupsReplicationResponse'
  { dbInstanceAutomatedBackup :: Core.Maybe DBInstanceAutomatedBackup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartDBInstanceAutomatedBackupsReplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstanceAutomatedBackup', 'startDBInstanceAutomatedBackupsReplicationResponse_dbInstanceAutomatedBackup' - Undocumented member.
--
-- 'httpStatus', 'startDBInstanceAutomatedBackupsReplicationResponse_httpStatus' - The response's http status code.
newStartDBInstanceAutomatedBackupsReplicationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartDBInstanceAutomatedBackupsReplicationResponse
newStartDBInstanceAutomatedBackupsReplicationResponse
  pHttpStatus_ =
    StartDBInstanceAutomatedBackupsReplicationResponse'
      { dbInstanceAutomatedBackup =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Undocumented member.
startDBInstanceAutomatedBackupsReplicationResponse_dbInstanceAutomatedBackup :: Lens.Lens' StartDBInstanceAutomatedBackupsReplicationResponse (Core.Maybe DBInstanceAutomatedBackup)
startDBInstanceAutomatedBackupsReplicationResponse_dbInstanceAutomatedBackup = Lens.lens (\StartDBInstanceAutomatedBackupsReplicationResponse' {dbInstanceAutomatedBackup} -> dbInstanceAutomatedBackup) (\s@StartDBInstanceAutomatedBackupsReplicationResponse' {} a -> s {dbInstanceAutomatedBackup = a} :: StartDBInstanceAutomatedBackupsReplicationResponse)

-- | The response's http status code.
startDBInstanceAutomatedBackupsReplicationResponse_httpStatus :: Lens.Lens' StartDBInstanceAutomatedBackupsReplicationResponse Core.Int
startDBInstanceAutomatedBackupsReplicationResponse_httpStatus = Lens.lens (\StartDBInstanceAutomatedBackupsReplicationResponse' {httpStatus} -> httpStatus) (\s@StartDBInstanceAutomatedBackupsReplicationResponse' {} a -> s {httpStatus = a} :: StartDBInstanceAutomatedBackupsReplicationResponse)

instance
  Core.NFData
    StartDBInstanceAutomatedBackupsReplicationResponse
