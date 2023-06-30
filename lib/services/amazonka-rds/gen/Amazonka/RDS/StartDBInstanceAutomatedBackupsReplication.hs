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
-- Module      : Amazonka.RDS.StartDBInstanceAutomatedBackupsReplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables replication of automated backups to a different Amazon Web
-- Services Region.
--
-- This command doesn\'t apply to RDS Custom.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_ReplicateBackups.html Replicating Automated Backups to Another Amazon Web Services Region>
-- in the /Amazon RDS User Guide./
module Amazonka.RDS.StartDBInstanceAutomatedBackupsReplication
  ( -- * Creating a Request
    StartDBInstanceAutomatedBackupsReplication (..),
    newStartDBInstanceAutomatedBackupsReplication,

    -- * Request Lenses
    startDBInstanceAutomatedBackupsReplication_backupRetentionPeriod,
    startDBInstanceAutomatedBackupsReplication_destinationRegion,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartDBInstanceAutomatedBackupsReplication' smart constructor.
data StartDBInstanceAutomatedBackupsReplication = StartDBInstanceAutomatedBackupsReplication'
  { -- | The retention period for the replicated automated backups.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | Pseudo-parameter used when populating the @PreSignedUrl@ of a
    -- cross-region @StartDBInstanceAutomatedBackupsReplication@ request. To
    -- replicate from region @SRC@ to region @DST@, send a request to region
    -- @DST@. In that request, pass a @PreSignedUrl@ for region @SRC@ with
    -- @DestinationRegion@ set to region @DST@.
    destinationRegion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services KMS key identifier for encryption of the
    -- replicated automated backups. The KMS key ID is the Amazon Resource Name
    -- (ARN) for the KMS encryption key in the destination Amazon Web Services
    -- Region, for example,
    -- @arn:aws:kms:us-east-1:123456789012:key\/AKIAIOSFODNN7EXAMPLE@.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | In an Amazon Web Services GovCloud (US) Region, an URL that contains a
    -- Signature Version 4 signed request for the
    -- @StartDBInstanceAutomatedBackupsReplication@ operation to call in the
    -- Amazon Web Services Region of the source DB instance. The presigned URL
    -- must be a valid request for the
    -- @StartDBInstanceAutomatedBackupsReplication@ API operation that can run
    -- in the Amazon Web Services Region that contains the source DB instance.
    --
    -- This setting applies only to Amazon Web Services GovCloud (US) Regions.
    -- It\'s ignored in other Amazon Web Services Regions.
    --
    -- To learn how to generate a Signature Version 4 signed request, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (Amazon Web Services Signature Version 4)>
    -- and
    -- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
    --
    -- If you are using an Amazon Web Services SDK tool or the CLI, you can
    -- specify @SourceRegion@ (or @--source-region@ for the CLI) instead of
    -- specifying @PreSignedUrl@ manually. Specifying @SourceRegion@
    -- autogenerates a presigned URL that is a valid request for the operation
    -- that can run in the source Amazon Web Services Region.
    preSignedUrl :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the source DB instance for the
    -- replicated automated backups, for example,
    -- @arn:aws:rds:us-west-2:123456789012:db:mydatabase@.
    sourceDBInstanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'destinationRegion', 'startDBInstanceAutomatedBackupsReplication_destinationRegion' - Pseudo-parameter used when populating the @PreSignedUrl@ of a
-- cross-region @StartDBInstanceAutomatedBackupsReplication@ request. To
-- replicate from region @SRC@ to region @DST@, send a request to region
-- @DST@. In that request, pass a @PreSignedUrl@ for region @SRC@ with
-- @DestinationRegion@ set to region @DST@.
--
-- 'kmsKeyId', 'startDBInstanceAutomatedBackupsReplication_kmsKeyId' - The Amazon Web Services KMS key identifier for encryption of the
-- replicated automated backups. The KMS key ID is the Amazon Resource Name
-- (ARN) for the KMS encryption key in the destination Amazon Web Services
-- Region, for example,
-- @arn:aws:kms:us-east-1:123456789012:key\/AKIAIOSFODNN7EXAMPLE@.
--
-- 'preSignedUrl', 'startDBInstanceAutomatedBackupsReplication_preSignedUrl' - In an Amazon Web Services GovCloud (US) Region, an URL that contains a
-- Signature Version 4 signed request for the
-- @StartDBInstanceAutomatedBackupsReplication@ operation to call in the
-- Amazon Web Services Region of the source DB instance. The presigned URL
-- must be a valid request for the
-- @StartDBInstanceAutomatedBackupsReplication@ API operation that can run
-- in the Amazon Web Services Region that contains the source DB instance.
--
-- This setting applies only to Amazon Web Services GovCloud (US) Regions.
-- It\'s ignored in other Amazon Web Services Regions.
--
-- To learn how to generate a Signature Version 4 signed request, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (Amazon Web Services Signature Version 4)>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
--
-- If you are using an Amazon Web Services SDK tool or the CLI, you can
-- specify @SourceRegion@ (or @--source-region@ for the CLI) instead of
-- specifying @PreSignedUrl@ manually. Specifying @SourceRegion@
-- autogenerates a presigned URL that is a valid request for the operation
-- that can run in the source Amazon Web Services Region.
--
-- 'sourceDBInstanceArn', 'startDBInstanceAutomatedBackupsReplication_sourceDBInstanceArn' - The Amazon Resource Name (ARN) of the source DB instance for the
-- replicated automated backups, for example,
-- @arn:aws:rds:us-west-2:123456789012:db:mydatabase@.
newStartDBInstanceAutomatedBackupsReplication ::
  -- | 'sourceDBInstanceArn'
  Prelude.Text ->
  StartDBInstanceAutomatedBackupsReplication
newStartDBInstanceAutomatedBackupsReplication
  pSourceDBInstanceArn_ =
    StartDBInstanceAutomatedBackupsReplication'
      { backupRetentionPeriod =
          Prelude.Nothing,
        destinationRegion =
          Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        preSignedUrl = Prelude.Nothing,
        sourceDBInstanceArn =
          pSourceDBInstanceArn_
      }

-- | The retention period for the replicated automated backups.
startDBInstanceAutomatedBackupsReplication_backupRetentionPeriod :: Lens.Lens' StartDBInstanceAutomatedBackupsReplication (Prelude.Maybe Prelude.Int)
startDBInstanceAutomatedBackupsReplication_backupRetentionPeriod = Lens.lens (\StartDBInstanceAutomatedBackupsReplication' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@StartDBInstanceAutomatedBackupsReplication' {} a -> s {backupRetentionPeriod = a} :: StartDBInstanceAutomatedBackupsReplication)

-- | Pseudo-parameter used when populating the @PreSignedUrl@ of a
-- cross-region @StartDBInstanceAutomatedBackupsReplication@ request. To
-- replicate from region @SRC@ to region @DST@, send a request to region
-- @DST@. In that request, pass a @PreSignedUrl@ for region @SRC@ with
-- @DestinationRegion@ set to region @DST@.
startDBInstanceAutomatedBackupsReplication_destinationRegion :: Lens.Lens' StartDBInstanceAutomatedBackupsReplication (Prelude.Maybe Prelude.Text)
startDBInstanceAutomatedBackupsReplication_destinationRegion = Lens.lens (\StartDBInstanceAutomatedBackupsReplication' {destinationRegion} -> destinationRegion) (\s@StartDBInstanceAutomatedBackupsReplication' {} a -> s {destinationRegion = a} :: StartDBInstanceAutomatedBackupsReplication)

-- | The Amazon Web Services KMS key identifier for encryption of the
-- replicated automated backups. The KMS key ID is the Amazon Resource Name
-- (ARN) for the KMS encryption key in the destination Amazon Web Services
-- Region, for example,
-- @arn:aws:kms:us-east-1:123456789012:key\/AKIAIOSFODNN7EXAMPLE@.
startDBInstanceAutomatedBackupsReplication_kmsKeyId :: Lens.Lens' StartDBInstanceAutomatedBackupsReplication (Prelude.Maybe Prelude.Text)
startDBInstanceAutomatedBackupsReplication_kmsKeyId = Lens.lens (\StartDBInstanceAutomatedBackupsReplication' {kmsKeyId} -> kmsKeyId) (\s@StartDBInstanceAutomatedBackupsReplication' {} a -> s {kmsKeyId = a} :: StartDBInstanceAutomatedBackupsReplication)

-- | In an Amazon Web Services GovCloud (US) Region, an URL that contains a
-- Signature Version 4 signed request for the
-- @StartDBInstanceAutomatedBackupsReplication@ operation to call in the
-- Amazon Web Services Region of the source DB instance. The presigned URL
-- must be a valid request for the
-- @StartDBInstanceAutomatedBackupsReplication@ API operation that can run
-- in the Amazon Web Services Region that contains the source DB instance.
--
-- This setting applies only to Amazon Web Services GovCloud (US) Regions.
-- It\'s ignored in other Amazon Web Services Regions.
--
-- To learn how to generate a Signature Version 4 signed request, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (Amazon Web Services Signature Version 4)>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
--
-- If you are using an Amazon Web Services SDK tool or the CLI, you can
-- specify @SourceRegion@ (or @--source-region@ for the CLI) instead of
-- specifying @PreSignedUrl@ manually. Specifying @SourceRegion@
-- autogenerates a presigned URL that is a valid request for the operation
-- that can run in the source Amazon Web Services Region.
startDBInstanceAutomatedBackupsReplication_preSignedUrl :: Lens.Lens' StartDBInstanceAutomatedBackupsReplication (Prelude.Maybe Prelude.Text)
startDBInstanceAutomatedBackupsReplication_preSignedUrl = Lens.lens (\StartDBInstanceAutomatedBackupsReplication' {preSignedUrl} -> preSignedUrl) (\s@StartDBInstanceAutomatedBackupsReplication' {} a -> s {preSignedUrl = a} :: StartDBInstanceAutomatedBackupsReplication)

-- | The Amazon Resource Name (ARN) of the source DB instance for the
-- replicated automated backups, for example,
-- @arn:aws:rds:us-west-2:123456789012:db:mydatabase@.
startDBInstanceAutomatedBackupsReplication_sourceDBInstanceArn :: Lens.Lens' StartDBInstanceAutomatedBackupsReplication Prelude.Text
startDBInstanceAutomatedBackupsReplication_sourceDBInstanceArn = Lens.lens (\StartDBInstanceAutomatedBackupsReplication' {sourceDBInstanceArn} -> sourceDBInstanceArn) (\s@StartDBInstanceAutomatedBackupsReplication' {} a -> s {sourceDBInstanceArn = a} :: StartDBInstanceAutomatedBackupsReplication)

instance
  Core.AWSRequest
    StartDBInstanceAutomatedBackupsReplication
  where
  type
    AWSResponse
      StartDBInstanceAutomatedBackupsReplication =
      StartDBInstanceAutomatedBackupsReplicationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "StartDBInstanceAutomatedBackupsReplicationResult"
      ( \s h x ->
          StartDBInstanceAutomatedBackupsReplicationResponse'
            Prelude.<$> (x Data..@? "DBInstanceAutomatedBackup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartDBInstanceAutomatedBackupsReplication
  where
  hashWithSalt
    _salt
    StartDBInstanceAutomatedBackupsReplication' {..} =
      _salt
        `Prelude.hashWithSalt` backupRetentionPeriod
        `Prelude.hashWithSalt` destinationRegion
        `Prelude.hashWithSalt` kmsKeyId
        `Prelude.hashWithSalt` preSignedUrl
        `Prelude.hashWithSalt` sourceDBInstanceArn

instance
  Prelude.NFData
    StartDBInstanceAutomatedBackupsReplication
  where
  rnf StartDBInstanceAutomatedBackupsReplication' {..} =
    Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf destinationRegion
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf preSignedUrl
      `Prelude.seq` Prelude.rnf sourceDBInstanceArn

instance
  Data.ToHeaders
    StartDBInstanceAutomatedBackupsReplication
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    StartDBInstanceAutomatedBackupsReplication
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    StartDBInstanceAutomatedBackupsReplication
  where
  toQuery
    StartDBInstanceAutomatedBackupsReplication' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "StartDBInstanceAutomatedBackupsReplication" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2014-10-31" :: Prelude.ByteString),
          "BackupRetentionPeriod"
            Data.=: backupRetentionPeriod,
          "DestinationRegion" Data.=: destinationRegion,
          "KmsKeyId" Data.=: kmsKeyId,
          "PreSignedUrl" Data.=: preSignedUrl,
          "SourceDBInstanceArn" Data.=: sourceDBInstanceArn
        ]

-- | /See:/ 'newStartDBInstanceAutomatedBackupsReplicationResponse' smart constructor.
data StartDBInstanceAutomatedBackupsReplicationResponse = StartDBInstanceAutomatedBackupsReplicationResponse'
  { dbInstanceAutomatedBackup :: Prelude.Maybe DBInstanceAutomatedBackup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StartDBInstanceAutomatedBackupsReplicationResponse
newStartDBInstanceAutomatedBackupsReplicationResponse
  pHttpStatus_ =
    StartDBInstanceAutomatedBackupsReplicationResponse'
      { dbInstanceAutomatedBackup =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Undocumented member.
startDBInstanceAutomatedBackupsReplicationResponse_dbInstanceAutomatedBackup :: Lens.Lens' StartDBInstanceAutomatedBackupsReplicationResponse (Prelude.Maybe DBInstanceAutomatedBackup)
startDBInstanceAutomatedBackupsReplicationResponse_dbInstanceAutomatedBackup = Lens.lens (\StartDBInstanceAutomatedBackupsReplicationResponse' {dbInstanceAutomatedBackup} -> dbInstanceAutomatedBackup) (\s@StartDBInstanceAutomatedBackupsReplicationResponse' {} a -> s {dbInstanceAutomatedBackup = a} :: StartDBInstanceAutomatedBackupsReplicationResponse)

-- | The response's http status code.
startDBInstanceAutomatedBackupsReplicationResponse_httpStatus :: Lens.Lens' StartDBInstanceAutomatedBackupsReplicationResponse Prelude.Int
startDBInstanceAutomatedBackupsReplicationResponse_httpStatus = Lens.lens (\StartDBInstanceAutomatedBackupsReplicationResponse' {httpStatus} -> httpStatus) (\s@StartDBInstanceAutomatedBackupsReplicationResponse' {} a -> s {httpStatus = a} :: StartDBInstanceAutomatedBackupsReplicationResponse)

instance
  Prelude.NFData
    StartDBInstanceAutomatedBackupsReplicationResponse
  where
  rnf
    StartDBInstanceAutomatedBackupsReplicationResponse' {..} =
      Prelude.rnf dbInstanceAutomatedBackup
        `Prelude.seq` Prelude.rnf httpStatus
