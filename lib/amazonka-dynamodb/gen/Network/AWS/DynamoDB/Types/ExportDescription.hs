{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ExportDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ExportDescription
  ( ExportDescription (..),

    -- * Smart constructor
    mkExportDescription,

    -- * Lenses
    edBilledSizeBytes,
    edClientToken,
    edEndTime,
    edExportArn,
    edExportFormat,
    edExportManifest,
    edExportStatus,
    edExportTime,
    edFailureCode,
    edFailureMessage,
    edItemCount,
    edS3Bucket,
    edS3BucketOwner,
    edS3Prefix,
    edS3SseAlgorithm,
    edS3SseKmsKeyId,
    edStartTime,
    edTableArn,
    edTableId,
  )
where

import qualified Network.AWS.DynamoDB.Types.ClientToken as Types
import qualified Network.AWS.DynamoDB.Types.ExportArn as Types
import qualified Network.AWS.DynamoDB.Types.ExportFormat as Types
import qualified Network.AWS.DynamoDB.Types.ExportManifest as Types
import qualified Network.AWS.DynamoDB.Types.ExportStatus as Types
import qualified Network.AWS.DynamoDB.Types.FailureCode as Types
import qualified Network.AWS.DynamoDB.Types.FailureMessage as Types
import qualified Network.AWS.DynamoDB.Types.S3Bucket as Types
import qualified Network.AWS.DynamoDB.Types.S3BucketOwner as Types
import qualified Network.AWS.DynamoDB.Types.S3Prefix as Types
import qualified Network.AWS.DynamoDB.Types.S3SseAlgorithm as Types
import qualified Network.AWS.DynamoDB.Types.S3SseKmsKeyId as Types
import qualified Network.AWS.DynamoDB.Types.TableArn as Types
import qualified Network.AWS.DynamoDB.Types.TableId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the properties of the exported table.
--
-- /See:/ 'mkExportDescription' smart constructor.
data ExportDescription = ExportDescription'
  { -- | The billable size of the table export.
    billedSizeBytes :: Core.Maybe Core.Natural,
    -- | The client token that was provided for the export task. A client token makes calls to @ExportTableToPointInTimeInput@ idempotent, meaning that multiple identical calls have the same effect as one single call.
    clientToken :: Core.Maybe Types.ClientToken,
    -- | The time at which the export task completed.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The Amazon Resource Name (ARN) of the table export.
    exportArn :: Core.Maybe Types.ExportArn,
    -- | The format of the exported data. Valid values for @ExportFormat@ are @DYNAMODB_JSON@ or @ION@ .
    exportFormat :: Core.Maybe Types.ExportFormat,
    -- | The name of the manifest file for the export task.
    exportManifest :: Core.Maybe Types.ExportManifest,
    -- | Export can be in one of the following states: IN_PROGRESS, COMPLETED, or FAILED.
    exportStatus :: Core.Maybe Types.ExportStatus,
    -- | Point in time from which table data was exported.
    exportTime :: Core.Maybe Core.NominalDiffTime,
    -- | Status code for the result of the failed export.
    failureCode :: Core.Maybe Types.FailureCode,
    -- | Export failure reason description.
    failureMessage :: Core.Maybe Types.FailureMessage,
    -- | The number of items exported.
    itemCount :: Core.Maybe Core.Natural,
    -- | The name of the Amazon S3 bucket containing the export.
    s3Bucket :: Core.Maybe Types.S3Bucket,
    -- | The ID of the AWS account that owns the bucket containing the export.
    s3BucketOwner :: Core.Maybe Types.S3BucketOwner,
    -- | The Amazon S3 bucket prefix used as the file name and path of the exported snapshot.
    s3Prefix :: Core.Maybe Types.S3Prefix,
    -- | Type of encryption used on the bucket where export data is stored. Valid values for @S3SseAlgorithm@ are:
    --
    --
    --     * @AES256@ - server-side encryption with Amazon S3 managed keys
    --
    --
    --     * @KMS@ - server-side encryption with AWS KMS managed keys
    s3SseAlgorithm :: Core.Maybe Types.S3SseAlgorithm,
    -- | The ID of the AWS KMS managed key used to encrypt the S3 bucket where export data is stored (if applicable).
    s3SseKmsKeyId :: Core.Maybe Types.S3SseKmsKeyId,
    -- | The time at which the export task began.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | The Amazon Resource Name (ARN) of the table that was exported.
    tableArn :: Core.Maybe Types.TableArn,
    -- | Unique ID of the table that was exported.
    tableId :: Core.Maybe Types.TableId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ExportDescription' value with any optional fields omitted.
mkExportDescription ::
  ExportDescription
mkExportDescription =
  ExportDescription'
    { billedSizeBytes = Core.Nothing,
      clientToken = Core.Nothing,
      endTime = Core.Nothing,
      exportArn = Core.Nothing,
      exportFormat = Core.Nothing,
      exportManifest = Core.Nothing,
      exportStatus = Core.Nothing,
      exportTime = Core.Nothing,
      failureCode = Core.Nothing,
      failureMessage = Core.Nothing,
      itemCount = Core.Nothing,
      s3Bucket = Core.Nothing,
      s3BucketOwner = Core.Nothing,
      s3Prefix = Core.Nothing,
      s3SseAlgorithm = Core.Nothing,
      s3SseKmsKeyId = Core.Nothing,
      startTime = Core.Nothing,
      tableArn = Core.Nothing,
      tableId = Core.Nothing
    }

-- | The billable size of the table export.
--
-- /Note:/ Consider using 'billedSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edBilledSizeBytes :: Lens.Lens' ExportDescription (Core.Maybe Core.Natural)
edBilledSizeBytes = Lens.field @"billedSizeBytes"
{-# DEPRECATED edBilledSizeBytes "Use generic-lens or generic-optics with 'billedSizeBytes' instead." #-}

-- | The client token that was provided for the export task. A client token makes calls to @ExportTableToPointInTimeInput@ idempotent, meaning that multiple identical calls have the same effect as one single call.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edClientToken :: Lens.Lens' ExportDescription (Core.Maybe Types.ClientToken)
edClientToken = Lens.field @"clientToken"
{-# DEPRECATED edClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The time at which the export task completed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edEndTime :: Lens.Lens' ExportDescription (Core.Maybe Core.NominalDiffTime)
edEndTime = Lens.field @"endTime"
{-# DEPRECATED edEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the table export.
--
-- /Note:/ Consider using 'exportArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edExportArn :: Lens.Lens' ExportDescription (Core.Maybe Types.ExportArn)
edExportArn = Lens.field @"exportArn"
{-# DEPRECATED edExportArn "Use generic-lens or generic-optics with 'exportArn' instead." #-}

-- | The format of the exported data. Valid values for @ExportFormat@ are @DYNAMODB_JSON@ or @ION@ .
--
-- /Note:/ Consider using 'exportFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edExportFormat :: Lens.Lens' ExportDescription (Core.Maybe Types.ExportFormat)
edExportFormat = Lens.field @"exportFormat"
{-# DEPRECATED edExportFormat "Use generic-lens or generic-optics with 'exportFormat' instead." #-}

-- | The name of the manifest file for the export task.
--
-- /Note:/ Consider using 'exportManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edExportManifest :: Lens.Lens' ExportDescription (Core.Maybe Types.ExportManifest)
edExportManifest = Lens.field @"exportManifest"
{-# DEPRECATED edExportManifest "Use generic-lens or generic-optics with 'exportManifest' instead." #-}

-- | Export can be in one of the following states: IN_PROGRESS, COMPLETED, or FAILED.
--
-- /Note:/ Consider using 'exportStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edExportStatus :: Lens.Lens' ExportDescription (Core.Maybe Types.ExportStatus)
edExportStatus = Lens.field @"exportStatus"
{-# DEPRECATED edExportStatus "Use generic-lens or generic-optics with 'exportStatus' instead." #-}

-- | Point in time from which table data was exported.
--
-- /Note:/ Consider using 'exportTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edExportTime :: Lens.Lens' ExportDescription (Core.Maybe Core.NominalDiffTime)
edExportTime = Lens.field @"exportTime"
{-# DEPRECATED edExportTime "Use generic-lens or generic-optics with 'exportTime' instead." #-}

-- | Status code for the result of the failed export.
--
-- /Note:/ Consider using 'failureCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edFailureCode :: Lens.Lens' ExportDescription (Core.Maybe Types.FailureCode)
edFailureCode = Lens.field @"failureCode"
{-# DEPRECATED edFailureCode "Use generic-lens or generic-optics with 'failureCode' instead." #-}

-- | Export failure reason description.
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edFailureMessage :: Lens.Lens' ExportDescription (Core.Maybe Types.FailureMessage)
edFailureMessage = Lens.field @"failureMessage"
{-# DEPRECATED edFailureMessage "Use generic-lens or generic-optics with 'failureMessage' instead." #-}

-- | The number of items exported.
--
-- /Note:/ Consider using 'itemCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edItemCount :: Lens.Lens' ExportDescription (Core.Maybe Core.Natural)
edItemCount = Lens.field @"itemCount"
{-# DEPRECATED edItemCount "Use generic-lens or generic-optics with 'itemCount' instead." #-}

-- | The name of the Amazon S3 bucket containing the export.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edS3Bucket :: Lens.Lens' ExportDescription (Core.Maybe Types.S3Bucket)
edS3Bucket = Lens.field @"s3Bucket"
{-# DEPRECATED edS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

-- | The ID of the AWS account that owns the bucket containing the export.
--
-- /Note:/ Consider using 's3BucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edS3BucketOwner :: Lens.Lens' ExportDescription (Core.Maybe Types.S3BucketOwner)
edS3BucketOwner = Lens.field @"s3BucketOwner"
{-# DEPRECATED edS3BucketOwner "Use generic-lens or generic-optics with 's3BucketOwner' instead." #-}

-- | The Amazon S3 bucket prefix used as the file name and path of the exported snapshot.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edS3Prefix :: Lens.Lens' ExportDescription (Core.Maybe Types.S3Prefix)
edS3Prefix = Lens.field @"s3Prefix"
{-# DEPRECATED edS3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead." #-}

-- | Type of encryption used on the bucket where export data is stored. Valid values for @S3SseAlgorithm@ are:
--
--
--     * @AES256@ - server-side encryption with Amazon S3 managed keys
--
--
--     * @KMS@ - server-side encryption with AWS KMS managed keys
--
--
--
-- /Note:/ Consider using 's3SseAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edS3SseAlgorithm :: Lens.Lens' ExportDescription (Core.Maybe Types.S3SseAlgorithm)
edS3SseAlgorithm = Lens.field @"s3SseAlgorithm"
{-# DEPRECATED edS3SseAlgorithm "Use generic-lens or generic-optics with 's3SseAlgorithm' instead." #-}

-- | The ID of the AWS KMS managed key used to encrypt the S3 bucket where export data is stored (if applicable).
--
-- /Note:/ Consider using 's3SseKmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edS3SseKmsKeyId :: Lens.Lens' ExportDescription (Core.Maybe Types.S3SseKmsKeyId)
edS3SseKmsKeyId = Lens.field @"s3SseKmsKeyId"
{-# DEPRECATED edS3SseKmsKeyId "Use generic-lens or generic-optics with 's3SseKmsKeyId' instead." #-}

-- | The time at which the export task began.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edStartTime :: Lens.Lens' ExportDescription (Core.Maybe Core.NominalDiffTime)
edStartTime = Lens.field @"startTime"
{-# DEPRECATED edStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the table that was exported.
--
-- /Note:/ Consider using 'tableArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edTableArn :: Lens.Lens' ExportDescription (Core.Maybe Types.TableArn)
edTableArn = Lens.field @"tableArn"
{-# DEPRECATED edTableArn "Use generic-lens or generic-optics with 'tableArn' instead." #-}

-- | Unique ID of the table that was exported.
--
-- /Note:/ Consider using 'tableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edTableId :: Lens.Lens' ExportDescription (Core.Maybe Types.TableId)
edTableId = Lens.field @"tableId"
{-# DEPRECATED edTableId "Use generic-lens or generic-optics with 'tableId' instead." #-}

instance Core.FromJSON ExportDescription where
  parseJSON =
    Core.withObject "ExportDescription" Core.$
      \x ->
        ExportDescription'
          Core.<$> (x Core..:? "BilledSizeBytes")
          Core.<*> (x Core..:? "ClientToken")
          Core.<*> (x Core..:? "EndTime")
          Core.<*> (x Core..:? "ExportArn")
          Core.<*> (x Core..:? "ExportFormat")
          Core.<*> (x Core..:? "ExportManifest")
          Core.<*> (x Core..:? "ExportStatus")
          Core.<*> (x Core..:? "ExportTime")
          Core.<*> (x Core..:? "FailureCode")
          Core.<*> (x Core..:? "FailureMessage")
          Core.<*> (x Core..:? "ItemCount")
          Core.<*> (x Core..:? "S3Bucket")
          Core.<*> (x Core..:? "S3BucketOwner")
          Core.<*> (x Core..:? "S3Prefix")
          Core.<*> (x Core..:? "S3SseAlgorithm")
          Core.<*> (x Core..:? "S3SseKmsKeyId")
          Core.<*> (x Core..:? "StartTime")
          Core.<*> (x Core..:? "TableArn")
          Core.<*> (x Core..:? "TableId")
