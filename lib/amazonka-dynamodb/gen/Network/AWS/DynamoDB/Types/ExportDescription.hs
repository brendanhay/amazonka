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
    edS3BucketOwner,
    edExportFormat,
    edS3SseKMSKeyId,
    edClientToken,
    edStartTime,
    edFailureCode,
    edExportStatus,
    edFailureMessage,
    edTableARN,
    edBilledSizeBytes,
    edExportARN,
    edExportTime,
    edS3SseAlgorithm,
    edEndTime,
    edS3Prefix,
    edExportManifest,
    edTableId,
    edItemCount,
    edS3Bucket,
  )
where

import Network.AWS.DynamoDB.Types.ExportFormat
import Network.AWS.DynamoDB.Types.ExportStatus
import Network.AWS.DynamoDB.Types.S3SseAlgorithm
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the properties of the exported table.
--
-- /See:/ 'mkExportDescription' smart constructor.
data ExportDescription = ExportDescription'
  { s3BucketOwner ::
      Lude.Maybe Lude.Text,
    exportFormat :: Lude.Maybe ExportFormat,
    s3SseKMSKeyId :: Lude.Maybe Lude.Text,
    clientToken :: Lude.Maybe Lude.Text,
    startTime :: Lude.Maybe Lude.Timestamp,
    failureCode :: Lude.Maybe Lude.Text,
    exportStatus :: Lude.Maybe ExportStatus,
    failureMessage :: Lude.Maybe Lude.Text,
    tableARN :: Lude.Maybe Lude.Text,
    billedSizeBytes :: Lude.Maybe Lude.Natural,
    exportARN :: Lude.Maybe Lude.Text,
    exportTime :: Lude.Maybe Lude.Timestamp,
    s3SseAlgorithm :: Lude.Maybe S3SseAlgorithm,
    endTime :: Lude.Maybe Lude.Timestamp,
    s3Prefix :: Lude.Maybe Lude.Text,
    exportManifest :: Lude.Maybe Lude.Text,
    tableId :: Lude.Maybe Lude.Text,
    itemCount :: Lude.Maybe Lude.Natural,
    s3Bucket :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportDescription' with the minimum fields required to make a request.
--
-- * 'billedSizeBytes' - The billable size of the table export.
-- * 'clientToken' - The client token that was provided for the export task. A client token makes calls to @ExportTableToPointInTimeInput@ idempotent, meaning that multiple identical calls have the same effect as one single call.
-- * 'endTime' - The time at which the export task completed.
-- * 'exportARN' - The Amazon Resource Name (ARN) of the table export.
-- * 'exportFormat' - The format of the exported data. Valid values for @ExportFormat@ are @DYNAMODB_JSON@ or @ION@ .
-- * 'exportManifest' - The name of the manifest file for the export task.
-- * 'exportStatus' - Export can be in one of the following states: IN_PROGRESS, COMPLETED, or FAILED.
-- * 'exportTime' - Point in time from which table data was exported.
-- * 'failureCode' - Status code for the result of the failed export.
-- * 'failureMessage' - Export failure reason description.
-- * 'itemCount' - The number of items exported.
-- * 's3Bucket' - The name of the Amazon S3 bucket containing the export.
-- * 's3BucketOwner' - The ID of the AWS account that owns the bucket containing the export.
-- * 's3Prefix' - The Amazon S3 bucket prefix used as the file name and path of the exported snapshot.
-- * 's3SseAlgorithm' - Type of encryption used on the bucket where export data is stored. Valid values for @S3SseAlgorithm@ are:
--
--
--     * @AES256@ - server-side encryption with Amazon S3 managed keys
--
--
--     * @KMS@ - server-side encryption with AWS KMS managed keys
--
--
-- * 's3SseKMSKeyId' - The ID of the AWS KMS managed key used to encrypt the S3 bucket where export data is stored (if applicable).
-- * 'startTime' - The time at which the export task began.
-- * 'tableARN' - The Amazon Resource Name (ARN) of the table that was exported.
-- * 'tableId' - Unique ID of the table that was exported.
mkExportDescription ::
  ExportDescription
mkExportDescription =
  ExportDescription'
    { s3BucketOwner = Lude.Nothing,
      exportFormat = Lude.Nothing,
      s3SseKMSKeyId = Lude.Nothing,
      clientToken = Lude.Nothing,
      startTime = Lude.Nothing,
      failureCode = Lude.Nothing,
      exportStatus = Lude.Nothing,
      failureMessage = Lude.Nothing,
      tableARN = Lude.Nothing,
      billedSizeBytes = Lude.Nothing,
      exportARN = Lude.Nothing,
      exportTime = Lude.Nothing,
      s3SseAlgorithm = Lude.Nothing,
      endTime = Lude.Nothing,
      s3Prefix = Lude.Nothing,
      exportManifest = Lude.Nothing,
      tableId = Lude.Nothing,
      itemCount = Lude.Nothing,
      s3Bucket = Lude.Nothing
    }

-- | The ID of the AWS account that owns the bucket containing the export.
--
-- /Note:/ Consider using 's3BucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edS3BucketOwner :: Lens.Lens' ExportDescription (Lude.Maybe Lude.Text)
edS3BucketOwner = Lens.lens (s3BucketOwner :: ExportDescription -> Lude.Maybe Lude.Text) (\s a -> s {s3BucketOwner = a} :: ExportDescription)
{-# DEPRECATED edS3BucketOwner "Use generic-lens or generic-optics with 's3BucketOwner' instead." #-}

-- | The format of the exported data. Valid values for @ExportFormat@ are @DYNAMODB_JSON@ or @ION@ .
--
-- /Note:/ Consider using 'exportFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edExportFormat :: Lens.Lens' ExportDescription (Lude.Maybe ExportFormat)
edExportFormat = Lens.lens (exportFormat :: ExportDescription -> Lude.Maybe ExportFormat) (\s a -> s {exportFormat = a} :: ExportDescription)
{-# DEPRECATED edExportFormat "Use generic-lens or generic-optics with 'exportFormat' instead." #-}

-- | The ID of the AWS KMS managed key used to encrypt the S3 bucket where export data is stored (if applicable).
--
-- /Note:/ Consider using 's3SseKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edS3SseKMSKeyId :: Lens.Lens' ExportDescription (Lude.Maybe Lude.Text)
edS3SseKMSKeyId = Lens.lens (s3SseKMSKeyId :: ExportDescription -> Lude.Maybe Lude.Text) (\s a -> s {s3SseKMSKeyId = a} :: ExportDescription)
{-# DEPRECATED edS3SseKMSKeyId "Use generic-lens or generic-optics with 's3SseKMSKeyId' instead." #-}

-- | The client token that was provided for the export task. A client token makes calls to @ExportTableToPointInTimeInput@ idempotent, meaning that multiple identical calls have the same effect as one single call.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edClientToken :: Lens.Lens' ExportDescription (Lude.Maybe Lude.Text)
edClientToken = Lens.lens (clientToken :: ExportDescription -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: ExportDescription)
{-# DEPRECATED edClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The time at which the export task began.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edStartTime :: Lens.Lens' ExportDescription (Lude.Maybe Lude.Timestamp)
edStartTime = Lens.lens (startTime :: ExportDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: ExportDescription)
{-# DEPRECATED edStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Status code for the result of the failed export.
--
-- /Note:/ Consider using 'failureCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edFailureCode :: Lens.Lens' ExportDescription (Lude.Maybe Lude.Text)
edFailureCode = Lens.lens (failureCode :: ExportDescription -> Lude.Maybe Lude.Text) (\s a -> s {failureCode = a} :: ExportDescription)
{-# DEPRECATED edFailureCode "Use generic-lens or generic-optics with 'failureCode' instead." #-}

-- | Export can be in one of the following states: IN_PROGRESS, COMPLETED, or FAILED.
--
-- /Note:/ Consider using 'exportStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edExportStatus :: Lens.Lens' ExportDescription (Lude.Maybe ExportStatus)
edExportStatus = Lens.lens (exportStatus :: ExportDescription -> Lude.Maybe ExportStatus) (\s a -> s {exportStatus = a} :: ExportDescription)
{-# DEPRECATED edExportStatus "Use generic-lens or generic-optics with 'exportStatus' instead." #-}

-- | Export failure reason description.
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edFailureMessage :: Lens.Lens' ExportDescription (Lude.Maybe Lude.Text)
edFailureMessage = Lens.lens (failureMessage :: ExportDescription -> Lude.Maybe Lude.Text) (\s a -> s {failureMessage = a} :: ExportDescription)
{-# DEPRECATED edFailureMessage "Use generic-lens or generic-optics with 'failureMessage' instead." #-}

-- | The Amazon Resource Name (ARN) of the table that was exported.
--
-- /Note:/ Consider using 'tableARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edTableARN :: Lens.Lens' ExportDescription (Lude.Maybe Lude.Text)
edTableARN = Lens.lens (tableARN :: ExportDescription -> Lude.Maybe Lude.Text) (\s a -> s {tableARN = a} :: ExportDescription)
{-# DEPRECATED edTableARN "Use generic-lens or generic-optics with 'tableARN' instead." #-}

-- | The billable size of the table export.
--
-- /Note:/ Consider using 'billedSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edBilledSizeBytes :: Lens.Lens' ExportDescription (Lude.Maybe Lude.Natural)
edBilledSizeBytes = Lens.lens (billedSizeBytes :: ExportDescription -> Lude.Maybe Lude.Natural) (\s a -> s {billedSizeBytes = a} :: ExportDescription)
{-# DEPRECATED edBilledSizeBytes "Use generic-lens or generic-optics with 'billedSizeBytes' instead." #-}

-- | The Amazon Resource Name (ARN) of the table export.
--
-- /Note:/ Consider using 'exportARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edExportARN :: Lens.Lens' ExportDescription (Lude.Maybe Lude.Text)
edExportARN = Lens.lens (exportARN :: ExportDescription -> Lude.Maybe Lude.Text) (\s a -> s {exportARN = a} :: ExportDescription)
{-# DEPRECATED edExportARN "Use generic-lens or generic-optics with 'exportARN' instead." #-}

-- | Point in time from which table data was exported.
--
-- /Note:/ Consider using 'exportTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edExportTime :: Lens.Lens' ExportDescription (Lude.Maybe Lude.Timestamp)
edExportTime = Lens.lens (exportTime :: ExportDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {exportTime = a} :: ExportDescription)
{-# DEPRECATED edExportTime "Use generic-lens or generic-optics with 'exportTime' instead." #-}

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
edS3SseAlgorithm :: Lens.Lens' ExportDescription (Lude.Maybe S3SseAlgorithm)
edS3SseAlgorithm = Lens.lens (s3SseAlgorithm :: ExportDescription -> Lude.Maybe S3SseAlgorithm) (\s a -> s {s3SseAlgorithm = a} :: ExportDescription)
{-# DEPRECATED edS3SseAlgorithm "Use generic-lens or generic-optics with 's3SseAlgorithm' instead." #-}

-- | The time at which the export task completed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edEndTime :: Lens.Lens' ExportDescription (Lude.Maybe Lude.Timestamp)
edEndTime = Lens.lens (endTime :: ExportDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: ExportDescription)
{-# DEPRECATED edEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The Amazon S3 bucket prefix used as the file name and path of the exported snapshot.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edS3Prefix :: Lens.Lens' ExportDescription (Lude.Maybe Lude.Text)
edS3Prefix = Lens.lens (s3Prefix :: ExportDescription -> Lude.Maybe Lude.Text) (\s a -> s {s3Prefix = a} :: ExportDescription)
{-# DEPRECATED edS3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead." #-}

-- | The name of the manifest file for the export task.
--
-- /Note:/ Consider using 'exportManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edExportManifest :: Lens.Lens' ExportDescription (Lude.Maybe Lude.Text)
edExportManifest = Lens.lens (exportManifest :: ExportDescription -> Lude.Maybe Lude.Text) (\s a -> s {exportManifest = a} :: ExportDescription)
{-# DEPRECATED edExportManifest "Use generic-lens or generic-optics with 'exportManifest' instead." #-}

-- | Unique ID of the table that was exported.
--
-- /Note:/ Consider using 'tableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edTableId :: Lens.Lens' ExportDescription (Lude.Maybe Lude.Text)
edTableId = Lens.lens (tableId :: ExportDescription -> Lude.Maybe Lude.Text) (\s a -> s {tableId = a} :: ExportDescription)
{-# DEPRECATED edTableId "Use generic-lens or generic-optics with 'tableId' instead." #-}

-- | The number of items exported.
--
-- /Note:/ Consider using 'itemCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edItemCount :: Lens.Lens' ExportDescription (Lude.Maybe Lude.Natural)
edItemCount = Lens.lens (itemCount :: ExportDescription -> Lude.Maybe Lude.Natural) (\s a -> s {itemCount = a} :: ExportDescription)
{-# DEPRECATED edItemCount "Use generic-lens or generic-optics with 'itemCount' instead." #-}

-- | The name of the Amazon S3 bucket containing the export.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edS3Bucket :: Lens.Lens' ExportDescription (Lude.Maybe Lude.Text)
edS3Bucket = Lens.lens (s3Bucket :: ExportDescription -> Lude.Maybe Lude.Text) (\s a -> s {s3Bucket = a} :: ExportDescription)
{-# DEPRECATED edS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

instance Lude.FromJSON ExportDescription where
  parseJSON =
    Lude.withObject
      "ExportDescription"
      ( \x ->
          ExportDescription'
            Lude.<$> (x Lude..:? "S3BucketOwner")
            Lude.<*> (x Lude..:? "ExportFormat")
            Lude.<*> (x Lude..:? "S3SseKmsKeyId")
            Lude.<*> (x Lude..:? "ClientToken")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "FailureCode")
            Lude.<*> (x Lude..:? "ExportStatus")
            Lude.<*> (x Lude..:? "FailureMessage")
            Lude.<*> (x Lude..:? "TableArn")
            Lude.<*> (x Lude..:? "BilledSizeBytes")
            Lude.<*> (x Lude..:? "ExportArn")
            Lude.<*> (x Lude..:? "ExportTime")
            Lude.<*> (x Lude..:? "S3SseAlgorithm")
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "S3Prefix")
            Lude.<*> (x Lude..:? "ExportManifest")
            Lude.<*> (x Lude..:? "TableId")
            Lude.<*> (x Lude..:? "ItemCount")
            Lude.<*> (x Lude..:? "S3Bucket")
      )
