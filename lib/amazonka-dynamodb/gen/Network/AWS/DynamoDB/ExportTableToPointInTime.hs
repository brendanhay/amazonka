{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.ExportTableToPointInTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports table data to an S3 bucket. The table must have point in time recovery enabled, and you can export data from any time within the point in time recovery window.
module Network.AWS.DynamoDB.ExportTableToPointInTime
  ( -- * Creating a request
    ExportTableToPointInTime (..),
    mkExportTableToPointInTime,

    -- ** Request lenses
    ettpitS3BucketOwner,
    ettpitExportFormat,
    ettpitS3SseKMSKeyId,
    ettpitClientToken,
    ettpitExportTime,
    ettpitS3SseAlgorithm,
    ettpitS3Prefix,
    ettpitTableARN,
    ettpitS3Bucket,

    -- * Destructuring the response
    ExportTableToPointInTimeResponse (..),
    mkExportTableToPointInTimeResponse,

    -- ** Response lenses
    ettpitrsExportDescription,
    ettpitrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkExportTableToPointInTime' smart constructor.
data ExportTableToPointInTime = ExportTableToPointInTime'
  { s3BucketOwner ::
      Lude.Maybe Lude.Text,
    exportFormat :: Lude.Maybe ExportFormat,
    s3SseKMSKeyId :: Lude.Maybe Lude.Text,
    clientToken :: Lude.Maybe Lude.Text,
    exportTime :: Lude.Maybe Lude.Timestamp,
    s3SseAlgorithm ::
      Lude.Maybe S3SseAlgorithm,
    s3Prefix :: Lude.Maybe Lude.Text,
    tableARN :: Lude.Text,
    s3Bucket :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportTableToPointInTime' with the minimum fields required to make a request.
--
-- * 'clientToken' - Providing a @ClientToken@ makes the call to @ExportTableToPointInTimeInput@ idempotent, meaning that multiple identical calls have the same effect as one single call.
--
-- A client token is valid for 8 hours after the first request that uses it is completed. After 8 hours, any request with the same client token is treated as a new request. Do not resubmit the same request with the same client token for more than 8 hours, or the result might not be idempotent.
-- If you submit a request with the same client token but a change in other parameters within the 8-hour idempotency window, DynamoDB returns an @IdempotentParameterMismatch@ exception.
-- * 'exportFormat' - The format for the exported data. Valid values for @ExportFormat@ are @DYNAMODB_JSON@ or @ION@ .
-- * 'exportTime' - Time in the past from which to export table data. The table export will be a snapshot of the table's state at this point in time.
-- * 's3Bucket' - The name of the Amazon S3 bucket to export the snapshot to.
-- * 's3BucketOwner' - The ID of the AWS account that owns the bucket the export will be stored in.
-- * 's3Prefix' - The Amazon S3 bucket prefix to use as the file name and path of the exported snapshot.
-- * 's3SseAlgorithm' - Type of encryption used on the bucket where export data will be stored. Valid values for @S3SseAlgorithm@ are:
--
--
--     * @AES256@ - server-side encryption with Amazon S3 managed keys
--
--
--     * @KMS@ - server-side encryption with AWS KMS managed keys
--
--
-- * 's3SseKMSKeyId' - The ID of the AWS KMS managed key used to encrypt the S3 bucket where export data will be stored (if applicable).
-- * 'tableARN' - The Amazon Resource Name (ARN) associated with the table to export.
mkExportTableToPointInTime ::
  -- | 'tableARN'
  Lude.Text ->
  -- | 's3Bucket'
  Lude.Text ->
  ExportTableToPointInTime
mkExportTableToPointInTime pTableARN_ pS3Bucket_ =
  ExportTableToPointInTime'
    { s3BucketOwner = Lude.Nothing,
      exportFormat = Lude.Nothing,
      s3SseKMSKeyId = Lude.Nothing,
      clientToken = Lude.Nothing,
      exportTime = Lude.Nothing,
      s3SseAlgorithm = Lude.Nothing,
      s3Prefix = Lude.Nothing,
      tableARN = pTableARN_,
      s3Bucket = pS3Bucket_
    }

-- | The ID of the AWS account that owns the bucket the export will be stored in.
--
-- /Note:/ Consider using 's3BucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ettpitS3BucketOwner :: Lens.Lens' ExportTableToPointInTime (Lude.Maybe Lude.Text)
ettpitS3BucketOwner = Lens.lens (s3BucketOwner :: ExportTableToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {s3BucketOwner = a} :: ExportTableToPointInTime)
{-# DEPRECATED ettpitS3BucketOwner "Use generic-lens or generic-optics with 's3BucketOwner' instead." #-}

-- | The format for the exported data. Valid values for @ExportFormat@ are @DYNAMODB_JSON@ or @ION@ .
--
-- /Note:/ Consider using 'exportFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ettpitExportFormat :: Lens.Lens' ExportTableToPointInTime (Lude.Maybe ExportFormat)
ettpitExportFormat = Lens.lens (exportFormat :: ExportTableToPointInTime -> Lude.Maybe ExportFormat) (\s a -> s {exportFormat = a} :: ExportTableToPointInTime)
{-# DEPRECATED ettpitExportFormat "Use generic-lens or generic-optics with 'exportFormat' instead." #-}

-- | The ID of the AWS KMS managed key used to encrypt the S3 bucket where export data will be stored (if applicable).
--
-- /Note:/ Consider using 's3SseKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ettpitS3SseKMSKeyId :: Lens.Lens' ExportTableToPointInTime (Lude.Maybe Lude.Text)
ettpitS3SseKMSKeyId = Lens.lens (s3SseKMSKeyId :: ExportTableToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {s3SseKMSKeyId = a} :: ExportTableToPointInTime)
{-# DEPRECATED ettpitS3SseKMSKeyId "Use generic-lens or generic-optics with 's3SseKMSKeyId' instead." #-}

-- | Providing a @ClientToken@ makes the call to @ExportTableToPointInTimeInput@ idempotent, meaning that multiple identical calls have the same effect as one single call.
--
-- A client token is valid for 8 hours after the first request that uses it is completed. After 8 hours, any request with the same client token is treated as a new request. Do not resubmit the same request with the same client token for more than 8 hours, or the result might not be idempotent.
-- If you submit a request with the same client token but a change in other parameters within the 8-hour idempotency window, DynamoDB returns an @IdempotentParameterMismatch@ exception.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ettpitClientToken :: Lens.Lens' ExportTableToPointInTime (Lude.Maybe Lude.Text)
ettpitClientToken = Lens.lens (clientToken :: ExportTableToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: ExportTableToPointInTime)
{-# DEPRECATED ettpitClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Time in the past from which to export table data. The table export will be a snapshot of the table's state at this point in time.
--
-- /Note:/ Consider using 'exportTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ettpitExportTime :: Lens.Lens' ExportTableToPointInTime (Lude.Maybe Lude.Timestamp)
ettpitExportTime = Lens.lens (exportTime :: ExportTableToPointInTime -> Lude.Maybe Lude.Timestamp) (\s a -> s {exportTime = a} :: ExportTableToPointInTime)
{-# DEPRECATED ettpitExportTime "Use generic-lens or generic-optics with 'exportTime' instead." #-}

-- | Type of encryption used on the bucket where export data will be stored. Valid values for @S3SseAlgorithm@ are:
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
ettpitS3SseAlgorithm :: Lens.Lens' ExportTableToPointInTime (Lude.Maybe S3SseAlgorithm)
ettpitS3SseAlgorithm = Lens.lens (s3SseAlgorithm :: ExportTableToPointInTime -> Lude.Maybe S3SseAlgorithm) (\s a -> s {s3SseAlgorithm = a} :: ExportTableToPointInTime)
{-# DEPRECATED ettpitS3SseAlgorithm "Use generic-lens or generic-optics with 's3SseAlgorithm' instead." #-}

-- | The Amazon S3 bucket prefix to use as the file name and path of the exported snapshot.
--
-- /Note:/ Consider using 's3Prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ettpitS3Prefix :: Lens.Lens' ExportTableToPointInTime (Lude.Maybe Lude.Text)
ettpitS3Prefix = Lens.lens (s3Prefix :: ExportTableToPointInTime -> Lude.Maybe Lude.Text) (\s a -> s {s3Prefix = a} :: ExportTableToPointInTime)
{-# DEPRECATED ettpitS3Prefix "Use generic-lens or generic-optics with 's3Prefix' instead." #-}

-- | The Amazon Resource Name (ARN) associated with the table to export.
--
-- /Note:/ Consider using 'tableARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ettpitTableARN :: Lens.Lens' ExportTableToPointInTime Lude.Text
ettpitTableARN = Lens.lens (tableARN :: ExportTableToPointInTime -> Lude.Text) (\s a -> s {tableARN = a} :: ExportTableToPointInTime)
{-# DEPRECATED ettpitTableARN "Use generic-lens or generic-optics with 'tableARN' instead." #-}

-- | The name of the Amazon S3 bucket to export the snapshot to.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ettpitS3Bucket :: Lens.Lens' ExportTableToPointInTime Lude.Text
ettpitS3Bucket = Lens.lens (s3Bucket :: ExportTableToPointInTime -> Lude.Text) (\s a -> s {s3Bucket = a} :: ExportTableToPointInTime)
{-# DEPRECATED ettpitS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

instance Lude.AWSRequest ExportTableToPointInTime where
  type Rs ExportTableToPointInTime = ExportTableToPointInTimeResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          ExportTableToPointInTimeResponse'
            Lude.<$> (x Lude..?> "ExportDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ExportTableToPointInTime where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.ExportTableToPointInTime" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ExportTableToPointInTime where
  toJSON ExportTableToPointInTime' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3BucketOwner" Lude..=) Lude.<$> s3BucketOwner,
            ("ExportFormat" Lude..=) Lude.<$> exportFormat,
            ("S3SseKmsKeyId" Lude..=) Lude.<$> s3SseKMSKeyId,
            ("ClientToken" Lude..=) Lude.<$> clientToken,
            ("ExportTime" Lude..=) Lude.<$> exportTime,
            ("S3SseAlgorithm" Lude..=) Lude.<$> s3SseAlgorithm,
            ("S3Prefix" Lude..=) Lude.<$> s3Prefix,
            Lude.Just ("TableArn" Lude..= tableARN),
            Lude.Just ("S3Bucket" Lude..= s3Bucket)
          ]
      )

instance Lude.ToPath ExportTableToPointInTime where
  toPath = Lude.const "/"

instance Lude.ToQuery ExportTableToPointInTime where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkExportTableToPointInTimeResponse' smart constructor.
data ExportTableToPointInTimeResponse = ExportTableToPointInTimeResponse'
  { exportDescription ::
      Lude.Maybe
        ExportDescription,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportTableToPointInTimeResponse' with the minimum fields required to make a request.
--
-- * 'exportDescription' - Contains a description of the table export.
-- * 'responseStatus' - The response status code.
mkExportTableToPointInTimeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ExportTableToPointInTimeResponse
mkExportTableToPointInTimeResponse pResponseStatus_ =
  ExportTableToPointInTimeResponse'
    { exportDescription =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains a description of the table export.
--
-- /Note:/ Consider using 'exportDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ettpitrsExportDescription :: Lens.Lens' ExportTableToPointInTimeResponse (Lude.Maybe ExportDescription)
ettpitrsExportDescription = Lens.lens (exportDescription :: ExportTableToPointInTimeResponse -> Lude.Maybe ExportDescription) (\s a -> s {exportDescription = a} :: ExportTableToPointInTimeResponse)
{-# DEPRECATED ettpitrsExportDescription "Use generic-lens or generic-optics with 'exportDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ettpitrsResponseStatus :: Lens.Lens' ExportTableToPointInTimeResponse Lude.Int
ettpitrsResponseStatus = Lens.lens (responseStatus :: ExportTableToPointInTimeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ExportTableToPointInTimeResponse)
{-# DEPRECATED ettpitrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
