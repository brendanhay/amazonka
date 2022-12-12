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
-- Module      : Amazonka.RDS.StartExportTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an export of a snapshot to Amazon S3. The provided IAM role must
-- have access to the S3 bucket.
--
-- This command doesn\'t apply to RDS Custom.
module Amazonka.RDS.StartExportTask
  ( -- * Creating a Request
    StartExportTask (..),
    newStartExportTask,

    -- * Request Lenses
    startExportTask_exportOnly,
    startExportTask_s3Prefix,
    startExportTask_exportTaskIdentifier,
    startExportTask_sourceArn,
    startExportTask_s3BucketName,
    startExportTask_iamRoleArn,
    startExportTask_kmsKeyId,

    -- * Destructuring the Response
    ExportTask (..),
    newExportTask,

    -- * Response Lenses
    exportTask_exportOnly,
    exportTask_exportTaskIdentifier,
    exportTask_failureCause,
    exportTask_iamRoleArn,
    exportTask_kmsKeyId,
    exportTask_percentProgress,
    exportTask_s3Bucket,
    exportTask_s3Prefix,
    exportTask_snapshotTime,
    exportTask_sourceArn,
    exportTask_sourceType,
    exportTask_status,
    exportTask_taskEndTime,
    exportTask_taskStartTime,
    exportTask_totalExtractedDataInGB,
    exportTask_warningMessage,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartExportTask' smart constructor.
data StartExportTask = StartExportTask'
  { -- | The data to be exported from the snapshot. If this parameter is not
    -- provided, all the snapshot data is exported. Valid values are the
    -- following:
    --
    -- -   @database@ - Export all the data from a specified database.
    --
    -- -   @database.table@ /table-name/ - Export a table of the snapshot. This
    --     format is valid only for RDS for MySQL, RDS for MariaDB, and Aurora
    --     MySQL.
    --
    -- -   @database.schema@ /schema-name/ - Export a database schema of the
    --     snapshot. This format is valid only for RDS for PostgreSQL and
    --     Aurora PostgreSQL.
    --
    -- -   @database.schema.table@ /table-name/ - Export a table of the
    --     database schema. This format is valid only for RDS for PostgreSQL
    --     and Aurora PostgreSQL.
    exportOnly :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon S3 bucket prefix to use as the file name and path of the
    -- exported snapshot.
    s3Prefix :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the snapshot export task. This ID isn\'t an
    -- identifier for the Amazon S3 bucket where the snapshot is to be exported
    -- to.
    exportTaskIdentifier :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the snapshot to export to Amazon S3.
    sourceArn :: Prelude.Text,
    -- | The name of the Amazon S3 bucket to export the snapshot to.
    s3BucketName :: Prelude.Text,
    -- | The name of the IAM role to use for writing to the Amazon S3 bucket when
    -- exporting a snapshot.
    iamRoleArn :: Prelude.Text,
    -- | The ID of the Amazon Web Services KMS key to use to encrypt the snapshot
    -- exported to Amazon S3. The Amazon Web Services KMS key identifier is the
    -- key ARN, key ID, alias ARN, or alias name for the KMS key. The caller of
    -- this operation must be authorized to execute the following operations.
    -- These can be set in the Amazon Web Services KMS key policy:
    --
    -- -   GrantOperation.Encrypt
    --
    -- -   GrantOperation.Decrypt
    --
    -- -   GrantOperation.GenerateDataKey
    --
    -- -   GrantOperation.GenerateDataKeyWithoutPlaintext
    --
    -- -   GrantOperation.ReEncryptFrom
    --
    -- -   GrantOperation.ReEncryptTo
    --
    -- -   GrantOperation.CreateGrant
    --
    -- -   GrantOperation.DescribeKey
    --
    -- -   GrantOperation.RetireGrant
    kmsKeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartExportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportOnly', 'startExportTask_exportOnly' - The data to be exported from the snapshot. If this parameter is not
-- provided, all the snapshot data is exported. Valid values are the
-- following:
--
-- -   @database@ - Export all the data from a specified database.
--
-- -   @database.table@ /table-name/ - Export a table of the snapshot. This
--     format is valid only for RDS for MySQL, RDS for MariaDB, and Aurora
--     MySQL.
--
-- -   @database.schema@ /schema-name/ - Export a database schema of the
--     snapshot. This format is valid only for RDS for PostgreSQL and
--     Aurora PostgreSQL.
--
-- -   @database.schema.table@ /table-name/ - Export a table of the
--     database schema. This format is valid only for RDS for PostgreSQL
--     and Aurora PostgreSQL.
--
-- 's3Prefix', 'startExportTask_s3Prefix' - The Amazon S3 bucket prefix to use as the file name and path of the
-- exported snapshot.
--
-- 'exportTaskIdentifier', 'startExportTask_exportTaskIdentifier' - A unique identifier for the snapshot export task. This ID isn\'t an
-- identifier for the Amazon S3 bucket where the snapshot is to be exported
-- to.
--
-- 'sourceArn', 'startExportTask_sourceArn' - The Amazon Resource Name (ARN) of the snapshot to export to Amazon S3.
--
-- 's3BucketName', 'startExportTask_s3BucketName' - The name of the Amazon S3 bucket to export the snapshot to.
--
-- 'iamRoleArn', 'startExportTask_iamRoleArn' - The name of the IAM role to use for writing to the Amazon S3 bucket when
-- exporting a snapshot.
--
-- 'kmsKeyId', 'startExportTask_kmsKeyId' - The ID of the Amazon Web Services KMS key to use to encrypt the snapshot
-- exported to Amazon S3. The Amazon Web Services KMS key identifier is the
-- key ARN, key ID, alias ARN, or alias name for the KMS key. The caller of
-- this operation must be authorized to execute the following operations.
-- These can be set in the Amazon Web Services KMS key policy:
--
-- -   GrantOperation.Encrypt
--
-- -   GrantOperation.Decrypt
--
-- -   GrantOperation.GenerateDataKey
--
-- -   GrantOperation.GenerateDataKeyWithoutPlaintext
--
-- -   GrantOperation.ReEncryptFrom
--
-- -   GrantOperation.ReEncryptTo
--
-- -   GrantOperation.CreateGrant
--
-- -   GrantOperation.DescribeKey
--
-- -   GrantOperation.RetireGrant
newStartExportTask ::
  -- | 'exportTaskIdentifier'
  Prelude.Text ->
  -- | 'sourceArn'
  Prelude.Text ->
  -- | 's3BucketName'
  Prelude.Text ->
  -- | 'iamRoleArn'
  Prelude.Text ->
  -- | 'kmsKeyId'
  Prelude.Text ->
  StartExportTask
newStartExportTask
  pExportTaskIdentifier_
  pSourceArn_
  pS3BucketName_
  pIamRoleArn_
  pKmsKeyId_ =
    StartExportTask'
      { exportOnly = Prelude.Nothing,
        s3Prefix = Prelude.Nothing,
        exportTaskIdentifier = pExportTaskIdentifier_,
        sourceArn = pSourceArn_,
        s3BucketName = pS3BucketName_,
        iamRoleArn = pIamRoleArn_,
        kmsKeyId = pKmsKeyId_
      }

-- | The data to be exported from the snapshot. If this parameter is not
-- provided, all the snapshot data is exported. Valid values are the
-- following:
--
-- -   @database@ - Export all the data from a specified database.
--
-- -   @database.table@ /table-name/ - Export a table of the snapshot. This
--     format is valid only for RDS for MySQL, RDS for MariaDB, and Aurora
--     MySQL.
--
-- -   @database.schema@ /schema-name/ - Export a database schema of the
--     snapshot. This format is valid only for RDS for PostgreSQL and
--     Aurora PostgreSQL.
--
-- -   @database.schema.table@ /table-name/ - Export a table of the
--     database schema. This format is valid only for RDS for PostgreSQL
--     and Aurora PostgreSQL.
startExportTask_exportOnly :: Lens.Lens' StartExportTask (Prelude.Maybe [Prelude.Text])
startExportTask_exportOnly = Lens.lens (\StartExportTask' {exportOnly} -> exportOnly) (\s@StartExportTask' {} a -> s {exportOnly = a} :: StartExportTask) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon S3 bucket prefix to use as the file name and path of the
-- exported snapshot.
startExportTask_s3Prefix :: Lens.Lens' StartExportTask (Prelude.Maybe Prelude.Text)
startExportTask_s3Prefix = Lens.lens (\StartExportTask' {s3Prefix} -> s3Prefix) (\s@StartExportTask' {} a -> s {s3Prefix = a} :: StartExportTask)

-- | A unique identifier for the snapshot export task. This ID isn\'t an
-- identifier for the Amazon S3 bucket where the snapshot is to be exported
-- to.
startExportTask_exportTaskIdentifier :: Lens.Lens' StartExportTask Prelude.Text
startExportTask_exportTaskIdentifier = Lens.lens (\StartExportTask' {exportTaskIdentifier} -> exportTaskIdentifier) (\s@StartExportTask' {} a -> s {exportTaskIdentifier = a} :: StartExportTask)

-- | The Amazon Resource Name (ARN) of the snapshot to export to Amazon S3.
startExportTask_sourceArn :: Lens.Lens' StartExportTask Prelude.Text
startExportTask_sourceArn = Lens.lens (\StartExportTask' {sourceArn} -> sourceArn) (\s@StartExportTask' {} a -> s {sourceArn = a} :: StartExportTask)

-- | The name of the Amazon S3 bucket to export the snapshot to.
startExportTask_s3BucketName :: Lens.Lens' StartExportTask Prelude.Text
startExportTask_s3BucketName = Lens.lens (\StartExportTask' {s3BucketName} -> s3BucketName) (\s@StartExportTask' {} a -> s {s3BucketName = a} :: StartExportTask)

-- | The name of the IAM role to use for writing to the Amazon S3 bucket when
-- exporting a snapshot.
startExportTask_iamRoleArn :: Lens.Lens' StartExportTask Prelude.Text
startExportTask_iamRoleArn = Lens.lens (\StartExportTask' {iamRoleArn} -> iamRoleArn) (\s@StartExportTask' {} a -> s {iamRoleArn = a} :: StartExportTask)

-- | The ID of the Amazon Web Services KMS key to use to encrypt the snapshot
-- exported to Amazon S3. The Amazon Web Services KMS key identifier is the
-- key ARN, key ID, alias ARN, or alias name for the KMS key. The caller of
-- this operation must be authorized to execute the following operations.
-- These can be set in the Amazon Web Services KMS key policy:
--
-- -   GrantOperation.Encrypt
--
-- -   GrantOperation.Decrypt
--
-- -   GrantOperation.GenerateDataKey
--
-- -   GrantOperation.GenerateDataKeyWithoutPlaintext
--
-- -   GrantOperation.ReEncryptFrom
--
-- -   GrantOperation.ReEncryptTo
--
-- -   GrantOperation.CreateGrant
--
-- -   GrantOperation.DescribeKey
--
-- -   GrantOperation.RetireGrant
startExportTask_kmsKeyId :: Lens.Lens' StartExportTask Prelude.Text
startExportTask_kmsKeyId = Lens.lens (\StartExportTask' {kmsKeyId} -> kmsKeyId) (\s@StartExportTask' {} a -> s {kmsKeyId = a} :: StartExportTask)

instance Core.AWSRequest StartExportTask where
  type AWSResponse StartExportTask = ExportTask
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "StartExportTaskResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable StartExportTask where
  hashWithSalt _salt StartExportTask' {..} =
    _salt `Prelude.hashWithSalt` exportOnly
      `Prelude.hashWithSalt` s3Prefix
      `Prelude.hashWithSalt` exportTaskIdentifier
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` s3BucketName
      `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` kmsKeyId

instance Prelude.NFData StartExportTask where
  rnf StartExportTask' {..} =
    Prelude.rnf exportOnly
      `Prelude.seq` Prelude.rnf s3Prefix
      `Prelude.seq` Prelude.rnf exportTaskIdentifier
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf kmsKeyId

instance Data.ToHeaders StartExportTask where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath StartExportTask where
  toPath = Prelude.const "/"

instance Data.ToQuery StartExportTask where
  toQuery StartExportTask' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("StartExportTask" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "ExportOnly"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> exportOnly),
        "S3Prefix" Data.=: s3Prefix,
        "ExportTaskIdentifier" Data.=: exportTaskIdentifier,
        "SourceArn" Data.=: sourceArn,
        "S3BucketName" Data.=: s3BucketName,
        "IamRoleArn" Data.=: iamRoleArn,
        "KmsKeyId" Data.=: kmsKeyId
      ]
