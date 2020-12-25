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
    setExportTaskIdentifier,
    setSourceArn,
    setS3BucketName,
    setIamRoleArn,
    setKmsKeyId,
    setExportOnly,
    setS3Prefix,

    -- * Destructuring the response
    Types.ExportTask (..),
    Types.mkExportTask,

    -- ** Response lenses
    Types.etExportOnly,
    Types.etExportTaskIdentifier,
    Types.etFailureCause,
    Types.etIamRoleArn,
    Types.etKmsKeyId,
    Types.etPercentProgress,
    Types.etS3Bucket,
    Types.etS3Prefix,
    Types.etSnapshotTime,
    Types.etSourceArn,
    Types.etStatus,
    Types.etTaskEndTime,
    Types.etTaskStartTime,
    Types.etTotalExtractedDataInGB,
    Types.etWarningMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartExportTask' smart constructor.
data StartExportTask = StartExportTask'
  { -- | A unique identifier for the snapshot export task. This ID isn't an identifier for the Amazon S3 bucket where the snapshot is to be exported to.
    exportTaskIdentifier :: Types.String,
    -- | The Amazon Resource Name (ARN) of the snapshot to export to Amazon S3.
    sourceArn :: Types.String,
    -- | The name of the Amazon S3 bucket to export the snapshot to.
    s3BucketName :: Types.String,
    -- | The name of the IAM role to use for writing to the Amazon S3 bucket when exporting a snapshot.
    iamRoleArn :: Types.String,
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
    kmsKeyId :: Types.String,
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
    exportOnly :: Core.Maybe [Types.String],
    -- | The Amazon S3 bucket prefix to use as the file name and path of the exported snapshot.
    s3Prefix :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartExportTask' value with any optional fields omitted.
mkStartExportTask ::
  -- | 'exportTaskIdentifier'
  Types.String ->
  -- | 'sourceArn'
  Types.String ->
  -- | 's3BucketName'
  Types.String ->
  -- | 'iamRoleArn'
  Types.String ->
  -- | 'kmsKeyId'
  Types.String ->
  StartExportTask
mkStartExportTask
  exportTaskIdentifier
  sourceArn
  s3BucketName
  iamRoleArn
  kmsKeyId =
    StartExportTask'
      { exportTaskIdentifier,
        sourceArn,
        s3BucketName,
        iamRoleArn,
        kmsKeyId,
        exportOnly = Core.Nothing,
        s3Prefix = Core.Nothing
      }

-- | A unique identifier for the snapshot export task. This ID isn't an identifier for the Amazon S3 bucket where the snapshot is to be exported to.
--
-- /Note:/ Consider using 'exportTaskIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setExportTaskIdentifier :: Lens.Lens' StartExportTask Types.String
setExportTaskIdentifier = Lens.field @"exportTaskIdentifier"
{-# DEPRECATED setExportTaskIdentifier "Use generic-lens or generic-optics with 'exportTaskIdentifier' instead." #-}

-- | The Amazon Resource Name (ARN) of the snapshot to export to Amazon S3.
--
-- /Note:/ Consider using 'sourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setSourceArn :: Lens.Lens' StartExportTask Types.String
setSourceArn = Lens.field @"sourceArn"
{-# DEPRECATED setSourceArn "Use generic-lens or generic-optics with 'sourceArn' instead." #-}

-- | The name of the Amazon S3 bucket to export the snapshot to.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setS3BucketName :: Lens.Lens' StartExportTask Types.String
setS3BucketName = Lens.field @"s3BucketName"
{-# DEPRECATED setS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | The name of the IAM role to use for writing to the Amazon S3 bucket when exporting a snapshot.
--
-- /Note:/ Consider using 'iamRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setIamRoleArn :: Lens.Lens' StartExportTask Types.String
setIamRoleArn = Lens.field @"iamRoleArn"
{-# DEPRECATED setIamRoleArn "Use generic-lens or generic-optics with 'iamRoleArn' instead." #-}

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
setKmsKeyId :: Lens.Lens' StartExportTask Types.String
setKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED setKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

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
setExportOnly :: Lens.Lens' StartExportTask (Core.Maybe [Types.String])
setExportOnly = Lens.field @"exportOnly"
{-# DEPRECATED setExportOnly "Use generic-lens or generic-optics with 'exportOnly' instead." #-}

-- | The Amazon S3 bucket prefix to use as the file name and path of the exported snapshot.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setS3Prefix :: Lens.Lens' StartExportTask (Core.Maybe Types.String)
setS3Prefix = Lens.field @"s3Prefix"
{-# DEPRECATED setS3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead." #-}

instance Core.AWSRequest StartExportTask where
  type Rs StartExportTask = Types.ExportTask
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "StartExportTask")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "ExportTaskIdentifier" exportTaskIdentifier)
                Core.<> (Core.toQueryValue "SourceArn" sourceArn)
                Core.<> (Core.toQueryValue "S3BucketName" s3BucketName)
                Core.<> (Core.toQueryValue "IamRoleArn" iamRoleArn)
                Core.<> (Core.toQueryValue "KmsKeyId" kmsKeyId)
                Core.<> ( Core.toQueryValue
                            "ExportOnly"
                            (Core.toQueryList "member" Core.<$> exportOnly)
                        )
                Core.<> (Core.toQueryValue "S3Prefix" Core.<$> s3Prefix)
            )
      }
  response =
    Response.receiveXMLWrapper
      "StartExportTaskResult"
      (\s h x -> Core.parseXML x)
