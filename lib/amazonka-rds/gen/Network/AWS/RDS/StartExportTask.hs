{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.StartExportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an export of a snapshot to Amazon S3. The provided IAM role must have access to the S3 bucket.
module Network.AWS.RDS.StartExportTask
  ( -- * Creating a request
    StartExportTask (..),
    mkStartExportTask,

    -- ** Request lenses
    setIAMRoleARN,
    setSourceARN,
    setExportOnly,
    setKMSKeyId,
    setExportTaskIdentifier,
    setS3Prefix,
    setS3BucketName,

    -- * Destructuring the response
    ExportTask (..),
    mkExportTask,

    -- ** Response lenses
    etTotalExtractedDataInGB,
    etStatus,
    etIAMRoleARN,
    etSourceARN,
    etExportOnly,
    etTaskStartTime,
    etWarningMessage,
    etSnapshotTime,
    etKMSKeyId,
    etTaskEndTime,
    etExportTaskIdentifier,
    etS3Prefix,
    etPercentProgress,
    etS3Bucket,
    etFailureCause,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartExportTask' smart constructor.
data StartExportTask = StartExportTask'
  { -- | The name of the IAM role to use for writing to the Amazon S3 bucket when exporting a snapshot.
    iamRoleARN :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the snapshot to export to Amazon S3.
    sourceARN :: Lude.Text,
    -- | The data to be exported from the snapshot. If this parameter is not provided, all the snapshot data is exported. Valid values are the following:
    --
    --
    --     * @database@ - Export all the data from a specified database.
    --
    --
    --     * @database.table@ /table-name/ - Export a table of the snapshot. This format is valid only for RDS for MySQL, RDS for MariaDB, and Aurora MySQL.
    --
    --
    --     * @database.schema@ /schema-name/ - Export a database schema of the snapshot. This format is valid only for RDS for PostgreSQL and Aurora PostgreSQL.
    --
    --
    --     * @database.schema.table@ /table-name/ - Export a table of the database schema. This format is valid only for RDS for PostgreSQL and Aurora PostgreSQL.
    exportOnly :: Lude.Maybe [Lude.Text],
    -- | The ID of the AWS KMS key to use to encrypt the snapshot exported to Amazon S3. The KMS key ID is the Amazon Resource Name (ARN), the KMS key identifier, or the KMS key alias for the KMS encryption key. The caller of this operation must be authorized to execute the following operations. These can be set in the KMS key policy:
    --
    --
    --     * GrantOperation.Encrypt
    --
    --
    --     * GrantOperation.Decrypt
    --
    --
    --     * GrantOperation.GenerateDataKey
    --
    --
    --     * GrantOperation.GenerateDataKeyWithoutPlaintext
    --
    --
    --     * GrantOperation.ReEncryptFrom
    --
    --
    --     * GrantOperation.ReEncryptTo
    --
    --
    --     * GrantOperation.CreateGrant
    --
    --
    --     * GrantOperation.DescribeKey
    --
    --
    --     * GrantOperation.RetireGrant
    kmsKeyId :: Lude.Text,
    -- | A unique identifier for the snapshot export task. This ID isn't an identifier for the Amazon S3 bucket where the snapshot is to be exported to.
    exportTaskIdentifier :: Lude.Text,
    -- | The Amazon S3 bucket prefix to use as the file name and path of the exported snapshot.
    s3Prefix :: Lude.Maybe Lude.Text,
    -- | The name of the Amazon S3 bucket to export the snapshot to.
    s3BucketName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartExportTask' with the minimum fields required to make a request.
--
-- * 'iamRoleARN' - The name of the IAM role to use for writing to the Amazon S3 bucket when exporting a snapshot.
-- * 'sourceARN' - The Amazon Resource Name (ARN) of the snapshot to export to Amazon S3.
-- * 'exportOnly' - The data to be exported from the snapshot. If this parameter is not provided, all the snapshot data is exported. Valid values are the following:
--
--
--     * @database@ - Export all the data from a specified database.
--
--
--     * @database.table@ /table-name/ - Export a table of the snapshot. This format is valid only for RDS for MySQL, RDS for MariaDB, and Aurora MySQL.
--
--
--     * @database.schema@ /schema-name/ - Export a database schema of the snapshot. This format is valid only for RDS for PostgreSQL and Aurora PostgreSQL.
--
--
--     * @database.schema.table@ /table-name/ - Export a table of the database schema. This format is valid only for RDS for PostgreSQL and Aurora PostgreSQL.
--
--
-- * 'kmsKeyId' - The ID of the AWS KMS key to use to encrypt the snapshot exported to Amazon S3. The KMS key ID is the Amazon Resource Name (ARN), the KMS key identifier, or the KMS key alias for the KMS encryption key. The caller of this operation must be authorized to execute the following operations. These can be set in the KMS key policy:
--
--
--     * GrantOperation.Encrypt
--
--
--     * GrantOperation.Decrypt
--
--
--     * GrantOperation.GenerateDataKey
--
--
--     * GrantOperation.GenerateDataKeyWithoutPlaintext
--
--
--     * GrantOperation.ReEncryptFrom
--
--
--     * GrantOperation.ReEncryptTo
--
--
--     * GrantOperation.CreateGrant
--
--
--     * GrantOperation.DescribeKey
--
--
--     * GrantOperation.RetireGrant
--
--
-- * 'exportTaskIdentifier' - A unique identifier for the snapshot export task. This ID isn't an identifier for the Amazon S3 bucket where the snapshot is to be exported to.
-- * 's3Prefix' - The Amazon S3 bucket prefix to use as the file name and path of the exported snapshot.
-- * 's3BucketName' - The name of the Amazon S3 bucket to export the snapshot to.
mkStartExportTask ::
  -- | 'iamRoleARN'
  Lude.Text ->
  -- | 'sourceARN'
  Lude.Text ->
  -- | 'kmsKeyId'
  Lude.Text ->
  -- | 'exportTaskIdentifier'
  Lude.Text ->
  -- | 's3BucketName'
  Lude.Text ->
  StartExportTask
mkStartExportTask
  pIAMRoleARN_
  pSourceARN_
  pKMSKeyId_
  pExportTaskIdentifier_
  pS3BucketName_ =
    StartExportTask'
      { iamRoleARN = pIAMRoleARN_,
        sourceARN = pSourceARN_,
        exportOnly = Lude.Nothing,
        kmsKeyId = pKMSKeyId_,
        exportTaskIdentifier = pExportTaskIdentifier_,
        s3Prefix = Lude.Nothing,
        s3BucketName = pS3BucketName_
      }

-- | The name of the IAM role to use for writing to the Amazon S3 bucket when exporting a snapshot.
--
-- /Note:/ Consider using 'iamRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setIAMRoleARN :: Lens.Lens' StartExportTask Lude.Text
setIAMRoleARN = Lens.lens (iamRoleARN :: StartExportTask -> Lude.Text) (\s a -> s {iamRoleARN = a} :: StartExportTask)
{-# DEPRECATED setIAMRoleARN "Use generic-lens or generic-optics with 'iamRoleARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the snapshot to export to Amazon S3.
--
-- /Note:/ Consider using 'sourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setSourceARN :: Lens.Lens' StartExportTask Lude.Text
setSourceARN = Lens.lens (sourceARN :: StartExportTask -> Lude.Text) (\s a -> s {sourceARN = a} :: StartExportTask)
{-# DEPRECATED setSourceARN "Use generic-lens or generic-optics with 'sourceARN' instead." #-}

-- | The data to be exported from the snapshot. If this parameter is not provided, all the snapshot data is exported. Valid values are the following:
--
--
--     * @database@ - Export all the data from a specified database.
--
--
--     * @database.table@ /table-name/ - Export a table of the snapshot. This format is valid only for RDS for MySQL, RDS for MariaDB, and Aurora MySQL.
--
--
--     * @database.schema@ /schema-name/ - Export a database schema of the snapshot. This format is valid only for RDS for PostgreSQL and Aurora PostgreSQL.
--
--
--     * @database.schema.table@ /table-name/ - Export a table of the database schema. This format is valid only for RDS for PostgreSQL and Aurora PostgreSQL.
--
--
--
-- /Note:/ Consider using 'exportOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setExportOnly :: Lens.Lens' StartExportTask (Lude.Maybe [Lude.Text])
setExportOnly = Lens.lens (exportOnly :: StartExportTask -> Lude.Maybe [Lude.Text]) (\s a -> s {exportOnly = a} :: StartExportTask)
{-# DEPRECATED setExportOnly "Use generic-lens or generic-optics with 'exportOnly' instead." #-}

-- | The ID of the AWS KMS key to use to encrypt the snapshot exported to Amazon S3. The KMS key ID is the Amazon Resource Name (ARN), the KMS key identifier, or the KMS key alias for the KMS encryption key. The caller of this operation must be authorized to execute the following operations. These can be set in the KMS key policy:
--
--
--     * GrantOperation.Encrypt
--
--
--     * GrantOperation.Decrypt
--
--
--     * GrantOperation.GenerateDataKey
--
--
--     * GrantOperation.GenerateDataKeyWithoutPlaintext
--
--
--     * GrantOperation.ReEncryptFrom
--
--
--     * GrantOperation.ReEncryptTo
--
--
--     * GrantOperation.CreateGrant
--
--
--     * GrantOperation.DescribeKey
--
--
--     * GrantOperation.RetireGrant
--
--
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setKMSKeyId :: Lens.Lens' StartExportTask Lude.Text
setKMSKeyId = Lens.lens (kmsKeyId :: StartExportTask -> Lude.Text) (\s a -> s {kmsKeyId = a} :: StartExportTask)
{-# DEPRECATED setKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | A unique identifier for the snapshot export task. This ID isn't an identifier for the Amazon S3 bucket where the snapshot is to be exported to.
--
-- /Note:/ Consider using 'exportTaskIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setExportTaskIdentifier :: Lens.Lens' StartExportTask Lude.Text
setExportTaskIdentifier = Lens.lens (exportTaskIdentifier :: StartExportTask -> Lude.Text) (\s a -> s {exportTaskIdentifier = a} :: StartExportTask)
{-# DEPRECATED setExportTaskIdentifier "Use generic-lens or generic-optics with 'exportTaskIdentifier' instead." #-}

-- | The Amazon S3 bucket prefix to use as the file name and path of the exported snapshot.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setS3Prefix :: Lens.Lens' StartExportTask (Lude.Maybe Lude.Text)
setS3Prefix = Lens.lens (s3Prefix :: StartExportTask -> Lude.Maybe Lude.Text) (\s a -> s {s3Prefix = a} :: StartExportTask)
{-# DEPRECATED setS3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead." #-}

-- | The name of the Amazon S3 bucket to export the snapshot to.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setS3BucketName :: Lens.Lens' StartExportTask Lude.Text
setS3BucketName = Lens.lens (s3BucketName :: StartExportTask -> Lude.Text) (\s a -> s {s3BucketName = a} :: StartExportTask)
{-# DEPRECATED setS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

instance Lude.AWSRequest StartExportTask where
  type Rs StartExportTask = ExportTask
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "StartExportTaskResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders StartExportTask where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath StartExportTask where
  toPath = Lude.const "/"

instance Lude.ToQuery StartExportTask where
  toQuery StartExportTask' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("StartExportTask" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "IamRoleArn" Lude.=: iamRoleARN,
        "SourceArn" Lude.=: sourceARN,
        "ExportOnly"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> exportOnly),
        "KmsKeyId" Lude.=: kmsKeyId,
        "ExportTaskIdentifier" Lude.=: exportTaskIdentifier,
        "S3Prefix" Lude.=: s3Prefix,
        "S3BucketName" Lude.=: s3BucketName
      ]
