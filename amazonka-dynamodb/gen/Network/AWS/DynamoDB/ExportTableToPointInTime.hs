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
-- Module      : Network.AWS.DynamoDB.ExportTableToPointInTime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports table data to an S3 bucket. The table must have point in time
-- recovery enabled, and you can export data from any time within the point
-- in time recovery window.
module Network.AWS.DynamoDB.ExportTableToPointInTime
  ( -- * Creating a Request
    ExportTableToPointInTime (..),
    newExportTableToPointInTime,

    -- * Request Lenses
    exportTableToPointInTime_exportFormat,
    exportTableToPointInTime_exportTime,
    exportTableToPointInTime_s3BucketOwner,
    exportTableToPointInTime_s3Prefix,
    exportTableToPointInTime_s3SseKmsKeyId,
    exportTableToPointInTime_clientToken,
    exportTableToPointInTime_s3SseAlgorithm,
    exportTableToPointInTime_tableArn,
    exportTableToPointInTime_s3Bucket,

    -- * Destructuring the Response
    ExportTableToPointInTimeResponse (..),
    newExportTableToPointInTimeResponse,

    -- * Response Lenses
    exportTableToPointInTimeResponse_exportDescription,
    exportTableToPointInTimeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newExportTableToPointInTime' smart constructor.
data ExportTableToPointInTime = ExportTableToPointInTime'
  { -- | The format for the exported data. Valid values for @ExportFormat@ are
    -- @DYNAMODB_JSON@ or @ION@.
    exportFormat :: Core.Maybe ExportFormat,
    -- | Time in the past from which to export table data. The table export will
    -- be a snapshot of the table\'s state at this point in time.
    exportTime :: Core.Maybe Core.POSIX,
    -- | The ID of the AWS account that owns the bucket the export will be stored
    -- in.
    s3BucketOwner :: Core.Maybe Core.Text,
    -- | The Amazon S3 bucket prefix to use as the file name and path of the
    -- exported snapshot.
    s3Prefix :: Core.Maybe Core.Text,
    -- | The ID of the AWS KMS managed key used to encrypt the S3 bucket where
    -- export data will be stored (if applicable).
    s3SseKmsKeyId :: Core.Maybe Core.Text,
    -- | Providing a @ClientToken@ makes the call to
    -- @ExportTableToPointInTimeInput@ idempotent, meaning that multiple
    -- identical calls have the same effect as one single call.
    --
    -- A client token is valid for 8 hours after the first request that uses it
    -- is completed. After 8 hours, any request with the same client token is
    -- treated as a new request. Do not resubmit the same request with the same
    -- client token for more than 8 hours, or the result might not be
    -- idempotent.
    --
    -- If you submit a request with the same client token but a change in other
    -- parameters within the 8-hour idempotency window, DynamoDB returns an
    -- @IdempotentParameterMismatch@ exception.
    clientToken :: Core.Maybe Core.Text,
    -- | Type of encryption used on the bucket where export data will be stored.
    -- Valid values for @S3SseAlgorithm@ are:
    --
    -- -   @AES256@ - server-side encryption with Amazon S3 managed keys
    --
    -- -   @KMS@ - server-side encryption with AWS KMS managed keys
    s3SseAlgorithm :: Core.Maybe S3SseAlgorithm,
    -- | The Amazon Resource Name (ARN) associated with the table to export.
    tableArn :: Core.Text,
    -- | The name of the Amazon S3 bucket to export the snapshot to.
    s3Bucket :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExportTableToPointInTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportFormat', 'exportTableToPointInTime_exportFormat' - The format for the exported data. Valid values for @ExportFormat@ are
-- @DYNAMODB_JSON@ or @ION@.
--
-- 'exportTime', 'exportTableToPointInTime_exportTime' - Time in the past from which to export table data. The table export will
-- be a snapshot of the table\'s state at this point in time.
--
-- 's3BucketOwner', 'exportTableToPointInTime_s3BucketOwner' - The ID of the AWS account that owns the bucket the export will be stored
-- in.
--
-- 's3Prefix', 'exportTableToPointInTime_s3Prefix' - The Amazon S3 bucket prefix to use as the file name and path of the
-- exported snapshot.
--
-- 's3SseKmsKeyId', 'exportTableToPointInTime_s3SseKmsKeyId' - The ID of the AWS KMS managed key used to encrypt the S3 bucket where
-- export data will be stored (if applicable).
--
-- 'clientToken', 'exportTableToPointInTime_clientToken' - Providing a @ClientToken@ makes the call to
-- @ExportTableToPointInTimeInput@ idempotent, meaning that multiple
-- identical calls have the same effect as one single call.
--
-- A client token is valid for 8 hours after the first request that uses it
-- is completed. After 8 hours, any request with the same client token is
-- treated as a new request. Do not resubmit the same request with the same
-- client token for more than 8 hours, or the result might not be
-- idempotent.
--
-- If you submit a request with the same client token but a change in other
-- parameters within the 8-hour idempotency window, DynamoDB returns an
-- @IdempotentParameterMismatch@ exception.
--
-- 's3SseAlgorithm', 'exportTableToPointInTime_s3SseAlgorithm' - Type of encryption used on the bucket where export data will be stored.
-- Valid values for @S3SseAlgorithm@ are:
--
-- -   @AES256@ - server-side encryption with Amazon S3 managed keys
--
-- -   @KMS@ - server-side encryption with AWS KMS managed keys
--
-- 'tableArn', 'exportTableToPointInTime_tableArn' - The Amazon Resource Name (ARN) associated with the table to export.
--
-- 's3Bucket', 'exportTableToPointInTime_s3Bucket' - The name of the Amazon S3 bucket to export the snapshot to.
newExportTableToPointInTime ::
  -- | 'tableArn'
  Core.Text ->
  -- | 's3Bucket'
  Core.Text ->
  ExportTableToPointInTime
newExportTableToPointInTime pTableArn_ pS3Bucket_ =
  ExportTableToPointInTime'
    { exportFormat =
        Core.Nothing,
      exportTime = Core.Nothing,
      s3BucketOwner = Core.Nothing,
      s3Prefix = Core.Nothing,
      s3SseKmsKeyId = Core.Nothing,
      clientToken = Core.Nothing,
      s3SseAlgorithm = Core.Nothing,
      tableArn = pTableArn_,
      s3Bucket = pS3Bucket_
    }

-- | The format for the exported data. Valid values for @ExportFormat@ are
-- @DYNAMODB_JSON@ or @ION@.
exportTableToPointInTime_exportFormat :: Lens.Lens' ExportTableToPointInTime (Core.Maybe ExportFormat)
exportTableToPointInTime_exportFormat = Lens.lens (\ExportTableToPointInTime' {exportFormat} -> exportFormat) (\s@ExportTableToPointInTime' {} a -> s {exportFormat = a} :: ExportTableToPointInTime)

-- | Time in the past from which to export table data. The table export will
-- be a snapshot of the table\'s state at this point in time.
exportTableToPointInTime_exportTime :: Lens.Lens' ExportTableToPointInTime (Core.Maybe Core.UTCTime)
exportTableToPointInTime_exportTime = Lens.lens (\ExportTableToPointInTime' {exportTime} -> exportTime) (\s@ExportTableToPointInTime' {} a -> s {exportTime = a} :: ExportTableToPointInTime) Core.. Lens.mapping Core._Time

-- | The ID of the AWS account that owns the bucket the export will be stored
-- in.
exportTableToPointInTime_s3BucketOwner :: Lens.Lens' ExportTableToPointInTime (Core.Maybe Core.Text)
exportTableToPointInTime_s3BucketOwner = Lens.lens (\ExportTableToPointInTime' {s3BucketOwner} -> s3BucketOwner) (\s@ExportTableToPointInTime' {} a -> s {s3BucketOwner = a} :: ExportTableToPointInTime)

-- | The Amazon S3 bucket prefix to use as the file name and path of the
-- exported snapshot.
exportTableToPointInTime_s3Prefix :: Lens.Lens' ExportTableToPointInTime (Core.Maybe Core.Text)
exportTableToPointInTime_s3Prefix = Lens.lens (\ExportTableToPointInTime' {s3Prefix} -> s3Prefix) (\s@ExportTableToPointInTime' {} a -> s {s3Prefix = a} :: ExportTableToPointInTime)

-- | The ID of the AWS KMS managed key used to encrypt the S3 bucket where
-- export data will be stored (if applicable).
exportTableToPointInTime_s3SseKmsKeyId :: Lens.Lens' ExportTableToPointInTime (Core.Maybe Core.Text)
exportTableToPointInTime_s3SseKmsKeyId = Lens.lens (\ExportTableToPointInTime' {s3SseKmsKeyId} -> s3SseKmsKeyId) (\s@ExportTableToPointInTime' {} a -> s {s3SseKmsKeyId = a} :: ExportTableToPointInTime)

-- | Providing a @ClientToken@ makes the call to
-- @ExportTableToPointInTimeInput@ idempotent, meaning that multiple
-- identical calls have the same effect as one single call.
--
-- A client token is valid for 8 hours after the first request that uses it
-- is completed. After 8 hours, any request with the same client token is
-- treated as a new request. Do not resubmit the same request with the same
-- client token for more than 8 hours, or the result might not be
-- idempotent.
--
-- If you submit a request with the same client token but a change in other
-- parameters within the 8-hour idempotency window, DynamoDB returns an
-- @IdempotentParameterMismatch@ exception.
exportTableToPointInTime_clientToken :: Lens.Lens' ExportTableToPointInTime (Core.Maybe Core.Text)
exportTableToPointInTime_clientToken = Lens.lens (\ExportTableToPointInTime' {clientToken} -> clientToken) (\s@ExportTableToPointInTime' {} a -> s {clientToken = a} :: ExportTableToPointInTime)

-- | Type of encryption used on the bucket where export data will be stored.
-- Valid values for @S3SseAlgorithm@ are:
--
-- -   @AES256@ - server-side encryption with Amazon S3 managed keys
--
-- -   @KMS@ - server-side encryption with AWS KMS managed keys
exportTableToPointInTime_s3SseAlgorithm :: Lens.Lens' ExportTableToPointInTime (Core.Maybe S3SseAlgorithm)
exportTableToPointInTime_s3SseAlgorithm = Lens.lens (\ExportTableToPointInTime' {s3SseAlgorithm} -> s3SseAlgorithm) (\s@ExportTableToPointInTime' {} a -> s {s3SseAlgorithm = a} :: ExportTableToPointInTime)

-- | The Amazon Resource Name (ARN) associated with the table to export.
exportTableToPointInTime_tableArn :: Lens.Lens' ExportTableToPointInTime Core.Text
exportTableToPointInTime_tableArn = Lens.lens (\ExportTableToPointInTime' {tableArn} -> tableArn) (\s@ExportTableToPointInTime' {} a -> s {tableArn = a} :: ExportTableToPointInTime)

-- | The name of the Amazon S3 bucket to export the snapshot to.
exportTableToPointInTime_s3Bucket :: Lens.Lens' ExportTableToPointInTime Core.Text
exportTableToPointInTime_s3Bucket = Lens.lens (\ExportTableToPointInTime' {s3Bucket} -> s3Bucket) (\s@ExportTableToPointInTime' {} a -> s {s3Bucket = a} :: ExportTableToPointInTime)

instance Core.AWSRequest ExportTableToPointInTime where
  type
    AWSResponse ExportTableToPointInTime =
      ExportTableToPointInTimeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportTableToPointInTimeResponse'
            Core.<$> (x Core..?> "ExportDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ExportTableToPointInTime

instance Core.NFData ExportTableToPointInTime

instance Core.ToHeaders ExportTableToPointInTime where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.ExportTableToPointInTime" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ExportTableToPointInTime where
  toJSON ExportTableToPointInTime' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ExportFormat" Core..=) Core.<$> exportFormat,
            ("ExportTime" Core..=) Core.<$> exportTime,
            ("S3BucketOwner" Core..=) Core.<$> s3BucketOwner,
            ("S3Prefix" Core..=) Core.<$> s3Prefix,
            ("S3SseKmsKeyId" Core..=) Core.<$> s3SseKmsKeyId,
            ("ClientToken" Core..=) Core.<$> clientToken,
            ("S3SseAlgorithm" Core..=) Core.<$> s3SseAlgorithm,
            Core.Just ("TableArn" Core..= tableArn),
            Core.Just ("S3Bucket" Core..= s3Bucket)
          ]
      )

instance Core.ToPath ExportTableToPointInTime where
  toPath = Core.const "/"

instance Core.ToQuery ExportTableToPointInTime where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newExportTableToPointInTimeResponse' smart constructor.
data ExportTableToPointInTimeResponse = ExportTableToPointInTimeResponse'
  { -- | Contains a description of the table export.
    exportDescription :: Core.Maybe ExportDescription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExportTableToPointInTimeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportDescription', 'exportTableToPointInTimeResponse_exportDescription' - Contains a description of the table export.
--
-- 'httpStatus', 'exportTableToPointInTimeResponse_httpStatus' - The response's http status code.
newExportTableToPointInTimeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ExportTableToPointInTimeResponse
newExportTableToPointInTimeResponse pHttpStatus_ =
  ExportTableToPointInTimeResponse'
    { exportDescription =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains a description of the table export.
exportTableToPointInTimeResponse_exportDescription :: Lens.Lens' ExportTableToPointInTimeResponse (Core.Maybe ExportDescription)
exportTableToPointInTimeResponse_exportDescription = Lens.lens (\ExportTableToPointInTimeResponse' {exportDescription} -> exportDescription) (\s@ExportTableToPointInTimeResponse' {} a -> s {exportDescription = a} :: ExportTableToPointInTimeResponse)

-- | The response's http status code.
exportTableToPointInTimeResponse_httpStatus :: Lens.Lens' ExportTableToPointInTimeResponse Core.Int
exportTableToPointInTimeResponse_httpStatus = Lens.lens (\ExportTableToPointInTimeResponse' {httpStatus} -> httpStatus) (\s@ExportTableToPointInTimeResponse' {} a -> s {httpStatus = a} :: ExportTableToPointInTimeResponse)

instance Core.NFData ExportTableToPointInTimeResponse
