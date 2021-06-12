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
-- Module      : Network.AWS.RDS.StartExportTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an export of a snapshot to Amazon S3. The provided IAM role must
-- have access to the S3 bucket.
module Network.AWS.RDS.StartExportTask
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
    exportTask_taskEndTime,
    exportTask_iamRoleArn,
    exportTask_status,
    exportTask_totalExtractedDataInGB,
    exportTask_warningMessage,
    exportTask_snapshotTime,
    exportTask_s3Bucket,
    exportTask_exportOnly,
    exportTask_kmsKeyId,
    exportTask_failureCause,
    exportTask_percentProgress,
    exportTask_sourceArn,
    exportTask_s3Prefix,
    exportTask_taskStartTime,
    exportTask_exportTaskIdentifier,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
    exportOnly :: Core.Maybe [Core.Text],
    -- | The Amazon S3 bucket prefix to use as the file name and path of the
    -- exported snapshot.
    s3Prefix :: Core.Maybe Core.Text,
    -- | A unique identifier for the snapshot export task. This ID isn\'t an
    -- identifier for the Amazon S3 bucket where the snapshot is to be exported
    -- to.
    exportTaskIdentifier :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the snapshot to export to Amazon S3.
    sourceArn :: Core.Text,
    -- | The name of the Amazon S3 bucket to export the snapshot to.
    s3BucketName :: Core.Text,
    -- | The name of the IAM role to use for writing to the Amazon S3 bucket when
    -- exporting a snapshot.
    iamRoleArn :: Core.Text,
    -- | The ID of the AWS KMS customer master key (CMK) to use to encrypt the
    -- snapshot exported to Amazon S3. The AWS KMS key identifier is the key
    -- ARN, key ID, alias ARN, or alias name for the AWS KMS customer master
    -- key (CMK). The caller of this operation must be authorized to execute
    -- the following operations. These can be set in the AWS KMS key policy:
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
    kmsKeyId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'kmsKeyId', 'startExportTask_kmsKeyId' - The ID of the AWS KMS customer master key (CMK) to use to encrypt the
-- snapshot exported to Amazon S3. The AWS KMS key identifier is the key
-- ARN, key ID, alias ARN, or alias name for the AWS KMS customer master
-- key (CMK). The caller of this operation must be authorized to execute
-- the following operations. These can be set in the AWS KMS key policy:
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
  Core.Text ->
  -- | 'sourceArn'
  Core.Text ->
  -- | 's3BucketName'
  Core.Text ->
  -- | 'iamRoleArn'
  Core.Text ->
  -- | 'kmsKeyId'
  Core.Text ->
  StartExportTask
newStartExportTask
  pExportTaskIdentifier_
  pSourceArn_
  pS3BucketName_
  pIamRoleArn_
  pKmsKeyId_ =
    StartExportTask'
      { exportOnly = Core.Nothing,
        s3Prefix = Core.Nothing,
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
startExportTask_exportOnly :: Lens.Lens' StartExportTask (Core.Maybe [Core.Text])
startExportTask_exportOnly = Lens.lens (\StartExportTask' {exportOnly} -> exportOnly) (\s@StartExportTask' {} a -> s {exportOnly = a} :: StartExportTask) Core.. Lens.mapping Lens._Coerce

-- | The Amazon S3 bucket prefix to use as the file name and path of the
-- exported snapshot.
startExportTask_s3Prefix :: Lens.Lens' StartExportTask (Core.Maybe Core.Text)
startExportTask_s3Prefix = Lens.lens (\StartExportTask' {s3Prefix} -> s3Prefix) (\s@StartExportTask' {} a -> s {s3Prefix = a} :: StartExportTask)

-- | A unique identifier for the snapshot export task. This ID isn\'t an
-- identifier for the Amazon S3 bucket where the snapshot is to be exported
-- to.
startExportTask_exportTaskIdentifier :: Lens.Lens' StartExportTask Core.Text
startExportTask_exportTaskIdentifier = Lens.lens (\StartExportTask' {exportTaskIdentifier} -> exportTaskIdentifier) (\s@StartExportTask' {} a -> s {exportTaskIdentifier = a} :: StartExportTask)

-- | The Amazon Resource Name (ARN) of the snapshot to export to Amazon S3.
startExportTask_sourceArn :: Lens.Lens' StartExportTask Core.Text
startExportTask_sourceArn = Lens.lens (\StartExportTask' {sourceArn} -> sourceArn) (\s@StartExportTask' {} a -> s {sourceArn = a} :: StartExportTask)

-- | The name of the Amazon S3 bucket to export the snapshot to.
startExportTask_s3BucketName :: Lens.Lens' StartExportTask Core.Text
startExportTask_s3BucketName = Lens.lens (\StartExportTask' {s3BucketName} -> s3BucketName) (\s@StartExportTask' {} a -> s {s3BucketName = a} :: StartExportTask)

-- | The name of the IAM role to use for writing to the Amazon S3 bucket when
-- exporting a snapshot.
startExportTask_iamRoleArn :: Lens.Lens' StartExportTask Core.Text
startExportTask_iamRoleArn = Lens.lens (\StartExportTask' {iamRoleArn} -> iamRoleArn) (\s@StartExportTask' {} a -> s {iamRoleArn = a} :: StartExportTask)

-- | The ID of the AWS KMS customer master key (CMK) to use to encrypt the
-- snapshot exported to Amazon S3. The AWS KMS key identifier is the key
-- ARN, key ID, alias ARN, or alias name for the AWS KMS customer master
-- key (CMK). The caller of this operation must be authorized to execute
-- the following operations. These can be set in the AWS KMS key policy:
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
startExportTask_kmsKeyId :: Lens.Lens' StartExportTask Core.Text
startExportTask_kmsKeyId = Lens.lens (\StartExportTask' {kmsKeyId} -> kmsKeyId) (\s@StartExportTask' {} a -> s {kmsKeyId = a} :: StartExportTask)

instance Core.AWSRequest StartExportTask where
  type AWSResponse StartExportTask = ExportTask
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "StartExportTaskResult"
      (\s h x -> Core.parseXML x)

instance Core.Hashable StartExportTask

instance Core.NFData StartExportTask

instance Core.ToHeaders StartExportTask where
  toHeaders = Core.const Core.mempty

instance Core.ToPath StartExportTask where
  toPath = Core.const "/"

instance Core.ToQuery StartExportTask where
  toQuery StartExportTask' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("StartExportTask" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "ExportOnly"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> exportOnly),
        "S3Prefix" Core.=: s3Prefix,
        "ExportTaskIdentifier" Core.=: exportTaskIdentifier,
        "SourceArn" Core.=: sourceArn,
        "S3BucketName" Core.=: s3BucketName,
        "IamRoleArn" Core.=: iamRoleArn,
        "KmsKeyId" Core.=: kmsKeyId
      ]
