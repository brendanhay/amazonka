{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ExportDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ExportDescription
  ( ExportDescription (..)
  -- * Smart constructor
  , mkExportDescription
  -- * Lenses
  , edBilledSizeBytes
  , edClientToken
  , edEndTime
  , edExportArn
  , edExportFormat
  , edExportManifest
  , edExportStatus
  , edExportTime
  , edFailureCode
  , edFailureMessage
  , edItemCount
  , edS3Bucket
  , edS3BucketOwner
  , edS3Prefix
  , edS3SseAlgorithm
  , edS3SseKmsKeyId
  , edStartTime
  , edTableArn
  , edTableId
  ) where

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
  { billedSizeBytes :: Core.Maybe Core.Natural
    -- ^ The billable size of the table export.
  , clientToken :: Core.Maybe Types.ClientToken
    -- ^ The client token that was provided for the export task. A client token makes calls to @ExportTableToPointInTimeInput@ idempotent, meaning that multiple identical calls have the same effect as one single call.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time at which the export task completed.
  , exportArn :: Core.Maybe Types.ExportArn
    -- ^ The Amazon Resource Name (ARN) of the table export.
  , exportFormat :: Core.Maybe Types.ExportFormat
    -- ^ The format of the exported data. Valid values for @ExportFormat@ are @DYNAMODB_JSON@ or @ION@ .
  , exportManifest :: Core.Maybe Types.ExportManifest
    -- ^ The name of the manifest file for the export task.
  , exportStatus :: Core.Maybe Types.ExportStatus
    -- ^ Export can be in one of the following states: IN_PROGRESS, COMPLETED, or FAILED.
  , exportTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Point in time from which table data was exported.
  , failureCode :: Core.Maybe Types.FailureCode
    -- ^ Status code for the result of the failed export.
  , failureMessage :: Core.Maybe Types.FailureMessage
    -- ^ Export failure reason description.
  , itemCount :: Core.Maybe Core.Natural
    -- ^ The number of items exported.
  , s3Bucket :: Core.Maybe Types.S3Bucket
    -- ^ The name of the Amazon S3 bucket containing the export.
  , s3BucketOwner :: Core.Maybe Types.S3BucketOwner
    -- ^ The ID of the AWS account that owns the bucket containing the export.
  , s3Prefix :: Core.Maybe Types.S3Prefix
    -- ^ The Amazon S3 bucket prefix used as the file name and path of the exported snapshot.
  , s3SseAlgorithm :: Core.Maybe Types.S3SseAlgorithm
    -- ^ Type of encryption used on the bucket where export data is stored. Valid values for @S3SseAlgorithm@ are:
--
--
--     * @AES256@ - server-side encryption with Amazon S3 managed keys
--
--
--     * @KMS@ - server-side encryption with AWS KMS managed keys
--
--
  , s3SseKmsKeyId :: Core.Maybe Types.S3SseKmsKeyId
    -- ^ The ID of the AWS KMS managed key used to encrypt the S3 bucket where export data is stored (if applicable).
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time at which the export task began.
  , tableArn :: Core.Maybe Types.TableArn
    -- ^ The Amazon Resource Name (ARN) of the table that was exported.
  , tableId :: Core.Maybe Types.TableId
    -- ^ Unique ID of the table that was exported.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ExportDescription' value with any optional fields omitted.
mkExportDescription
    :: ExportDescription
mkExportDescription
  = ExportDescription'{billedSizeBytes = Core.Nothing,
                       clientToken = Core.Nothing, endTime = Core.Nothing,
                       exportArn = Core.Nothing, exportFormat = Core.Nothing,
                       exportManifest = Core.Nothing, exportStatus = Core.Nothing,
                       exportTime = Core.Nothing, failureCode = Core.Nothing,
                       failureMessage = Core.Nothing, itemCount = Core.Nothing,
                       s3Bucket = Core.Nothing, s3BucketOwner = Core.Nothing,
                       s3Prefix = Core.Nothing, s3SseAlgorithm = Core.Nothing,
                       s3SseKmsKeyId = Core.Nothing, startTime = Core.Nothing,
                       tableArn = Core.Nothing, tableId = Core.Nothing}

-- | The billable size of the table export.
--
-- /Note:/ Consider using 'billedSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edBilledSizeBytes :: Lens.Lens' ExportDescription (Core.Maybe Core.Natural)
edBilledSizeBytes = Lens.field @"billedSizeBytes"
{-# INLINEABLE edBilledSizeBytes #-}
{-# DEPRECATED billedSizeBytes "Use generic-lens or generic-optics with 'billedSizeBytes' instead"  #-}

-- | The client token that was provided for the export task. A client token makes calls to @ExportTableToPointInTimeInput@ idempotent, meaning that multiple identical calls have the same effect as one single call.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edClientToken :: Lens.Lens' ExportDescription (Core.Maybe Types.ClientToken)
edClientToken = Lens.field @"clientToken"
{-# INLINEABLE edClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The time at which the export task completed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edEndTime :: Lens.Lens' ExportDescription (Core.Maybe Core.NominalDiffTime)
edEndTime = Lens.field @"endTime"
{-# INLINEABLE edEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The Amazon Resource Name (ARN) of the table export.
--
-- /Note:/ Consider using 'exportArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edExportArn :: Lens.Lens' ExportDescription (Core.Maybe Types.ExportArn)
edExportArn = Lens.field @"exportArn"
{-# INLINEABLE edExportArn #-}
{-# DEPRECATED exportArn "Use generic-lens or generic-optics with 'exportArn' instead"  #-}

-- | The format of the exported data. Valid values for @ExportFormat@ are @DYNAMODB_JSON@ or @ION@ .
--
-- /Note:/ Consider using 'exportFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edExportFormat :: Lens.Lens' ExportDescription (Core.Maybe Types.ExportFormat)
edExportFormat = Lens.field @"exportFormat"
{-# INLINEABLE edExportFormat #-}
{-# DEPRECATED exportFormat "Use generic-lens or generic-optics with 'exportFormat' instead"  #-}

-- | The name of the manifest file for the export task.
--
-- /Note:/ Consider using 'exportManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edExportManifest :: Lens.Lens' ExportDescription (Core.Maybe Types.ExportManifest)
edExportManifest = Lens.field @"exportManifest"
{-# INLINEABLE edExportManifest #-}
{-# DEPRECATED exportManifest "Use generic-lens or generic-optics with 'exportManifest' instead"  #-}

-- | Export can be in one of the following states: IN_PROGRESS, COMPLETED, or FAILED.
--
-- /Note:/ Consider using 'exportStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edExportStatus :: Lens.Lens' ExportDescription (Core.Maybe Types.ExportStatus)
edExportStatus = Lens.field @"exportStatus"
{-# INLINEABLE edExportStatus #-}
{-# DEPRECATED exportStatus "Use generic-lens or generic-optics with 'exportStatus' instead"  #-}

-- | Point in time from which table data was exported.
--
-- /Note:/ Consider using 'exportTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edExportTime :: Lens.Lens' ExportDescription (Core.Maybe Core.NominalDiffTime)
edExportTime = Lens.field @"exportTime"
{-# INLINEABLE edExportTime #-}
{-# DEPRECATED exportTime "Use generic-lens or generic-optics with 'exportTime' instead"  #-}

-- | Status code for the result of the failed export.
--
-- /Note:/ Consider using 'failureCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edFailureCode :: Lens.Lens' ExportDescription (Core.Maybe Types.FailureCode)
edFailureCode = Lens.field @"failureCode"
{-# INLINEABLE edFailureCode #-}
{-# DEPRECATED failureCode "Use generic-lens or generic-optics with 'failureCode' instead"  #-}

-- | Export failure reason description.
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edFailureMessage :: Lens.Lens' ExportDescription (Core.Maybe Types.FailureMessage)
edFailureMessage = Lens.field @"failureMessage"
{-# INLINEABLE edFailureMessage #-}
{-# DEPRECATED failureMessage "Use generic-lens or generic-optics with 'failureMessage' instead"  #-}

-- | The number of items exported.
--
-- /Note:/ Consider using 'itemCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edItemCount :: Lens.Lens' ExportDescription (Core.Maybe Core.Natural)
edItemCount = Lens.field @"itemCount"
{-# INLINEABLE edItemCount #-}
{-# DEPRECATED itemCount "Use generic-lens or generic-optics with 'itemCount' instead"  #-}

-- | The name of the Amazon S3 bucket containing the export.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edS3Bucket :: Lens.Lens' ExportDescription (Core.Maybe Types.S3Bucket)
edS3Bucket = Lens.field @"s3Bucket"
{-# INLINEABLE edS3Bucket #-}
{-# DEPRECATED s3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead"  #-}

-- | The ID of the AWS account that owns the bucket containing the export.
--
-- /Note:/ Consider using 's3BucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edS3BucketOwner :: Lens.Lens' ExportDescription (Core.Maybe Types.S3BucketOwner)
edS3BucketOwner = Lens.field @"s3BucketOwner"
{-# INLINEABLE edS3BucketOwner #-}
{-# DEPRECATED s3BucketOwner "Use generic-lens or generic-optics with 's3BucketOwner' instead"  #-}

-- | The Amazon S3 bucket prefix used as the file name and path of the exported snapshot.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edS3Prefix :: Lens.Lens' ExportDescription (Core.Maybe Types.S3Prefix)
edS3Prefix = Lens.field @"s3Prefix"
{-# INLINEABLE edS3Prefix #-}
{-# DEPRECATED s3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead"  #-}

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
{-# INLINEABLE edS3SseAlgorithm #-}
{-# DEPRECATED s3SseAlgorithm "Use generic-lens or generic-optics with 's3SseAlgorithm' instead"  #-}

-- | The ID of the AWS KMS managed key used to encrypt the S3 bucket where export data is stored (if applicable).
--
-- /Note:/ Consider using 's3SseKmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edS3SseKmsKeyId :: Lens.Lens' ExportDescription (Core.Maybe Types.S3SseKmsKeyId)
edS3SseKmsKeyId = Lens.field @"s3SseKmsKeyId"
{-# INLINEABLE edS3SseKmsKeyId #-}
{-# DEPRECATED s3SseKmsKeyId "Use generic-lens or generic-optics with 's3SseKmsKeyId' instead"  #-}

-- | The time at which the export task began.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edStartTime :: Lens.Lens' ExportDescription (Core.Maybe Core.NominalDiffTime)
edStartTime = Lens.field @"startTime"
{-# INLINEABLE edStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The Amazon Resource Name (ARN) of the table that was exported.
--
-- /Note:/ Consider using 'tableArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edTableArn :: Lens.Lens' ExportDescription (Core.Maybe Types.TableArn)
edTableArn = Lens.field @"tableArn"
{-# INLINEABLE edTableArn #-}
{-# DEPRECATED tableArn "Use generic-lens or generic-optics with 'tableArn' instead"  #-}

-- | Unique ID of the table that was exported.
--
-- /Note:/ Consider using 'tableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edTableId :: Lens.Lens' ExportDescription (Core.Maybe Types.TableId)
edTableId = Lens.field @"tableId"
{-# INLINEABLE edTableId #-}
{-# DEPRECATED tableId "Use generic-lens or generic-optics with 'tableId' instead"  #-}

instance Core.FromJSON ExportDescription where
        parseJSON
          = Core.withObject "ExportDescription" Core.$
              \ x ->
                ExportDescription' Core.<$>
                  (x Core..:? "BilledSizeBytes") Core.<*> x Core..:? "ClientToken"
                    Core.<*> x Core..:? "EndTime"
                    Core.<*> x Core..:? "ExportArn"
                    Core.<*> x Core..:? "ExportFormat"
                    Core.<*> x Core..:? "ExportManifest"
                    Core.<*> x Core..:? "ExportStatus"
                    Core.<*> x Core..:? "ExportTime"
                    Core.<*> x Core..:? "FailureCode"
                    Core.<*> x Core..:? "FailureMessage"
                    Core.<*> x Core..:? "ItemCount"
                    Core.<*> x Core..:? "S3Bucket"
                    Core.<*> x Core..:? "S3BucketOwner"
                    Core.<*> x Core..:? "S3Prefix"
                    Core.<*> x Core..:? "S3SseAlgorithm"
                    Core.<*> x Core..:? "S3SseKmsKeyId"
                    Core.<*> x Core..:? "StartTime"
                    Core.<*> x Core..:? "TableArn"
                    Core.<*> x Core..:? "TableId"
