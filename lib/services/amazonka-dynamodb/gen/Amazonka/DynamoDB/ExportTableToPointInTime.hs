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
-- Module      : Amazonka.DynamoDB.ExportTableToPointInTime
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports table data to an S3 bucket. The table must have point in time
-- recovery enabled, and you can export data from any time within the point
-- in time recovery window.
module Amazonka.DynamoDB.ExportTableToPointInTime
  ( -- * Creating a Request
    ExportTableToPointInTime (..),
    newExportTableToPointInTime,

    -- * Request Lenses
    exportTableToPointInTime_clientToken,
    exportTableToPointInTime_exportFormat,
    exportTableToPointInTime_exportTime,
    exportTableToPointInTime_s3BucketOwner,
    exportTableToPointInTime_s3Prefix,
    exportTableToPointInTime_s3SseAlgorithm,
    exportTableToPointInTime_s3SseKmsKeyId,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExportTableToPointInTime' smart constructor.
data ExportTableToPointInTime = ExportTableToPointInTime'
  { -- | Providing a @ClientToken@ makes the call to
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
    -- @ImportConflictException@.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The format for the exported data. Valid values for @ExportFormat@ are
    -- @DYNAMODB_JSON@ or @ION@.
    exportFormat :: Prelude.Maybe ExportFormat,
    -- | Time in the past from which to export table data, counted in seconds
    -- from the start of the Unix epoch. The table export will be a snapshot of
    -- the table\'s state at this point in time.
    exportTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the Amazon Web Services account that owns the bucket the
    -- export will be stored in.
    s3BucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket prefix to use as the file name and path of the
    -- exported snapshot.
    s3Prefix :: Prelude.Maybe Prelude.Text,
    -- | Type of encryption used on the bucket where export data will be stored.
    -- Valid values for @S3SseAlgorithm@ are:
    --
    -- -   @AES256@ - server-side encryption with Amazon S3 managed keys
    --
    -- -   @KMS@ - server-side encryption with KMS managed keys
    s3SseAlgorithm :: Prelude.Maybe S3SseAlgorithm,
    -- | The ID of the KMS managed key used to encrypt the S3 bucket where export
    -- data will be stored (if applicable).
    s3SseKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) associated with the table to export.
    tableArn :: Prelude.Text,
    -- | The name of the Amazon S3 bucket to export the snapshot to.
    s3Bucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportTableToPointInTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- @ImportConflictException@.
--
-- 'exportFormat', 'exportTableToPointInTime_exportFormat' - The format for the exported data. Valid values for @ExportFormat@ are
-- @DYNAMODB_JSON@ or @ION@.
--
-- 'exportTime', 'exportTableToPointInTime_exportTime' - Time in the past from which to export table data, counted in seconds
-- from the start of the Unix epoch. The table export will be a snapshot of
-- the table\'s state at this point in time.
--
-- 's3BucketOwner', 'exportTableToPointInTime_s3BucketOwner' - The ID of the Amazon Web Services account that owns the bucket the
-- export will be stored in.
--
-- 's3Prefix', 'exportTableToPointInTime_s3Prefix' - The Amazon S3 bucket prefix to use as the file name and path of the
-- exported snapshot.
--
-- 's3SseAlgorithm', 'exportTableToPointInTime_s3SseAlgorithm' - Type of encryption used on the bucket where export data will be stored.
-- Valid values for @S3SseAlgorithm@ are:
--
-- -   @AES256@ - server-side encryption with Amazon S3 managed keys
--
-- -   @KMS@ - server-side encryption with KMS managed keys
--
-- 's3SseKmsKeyId', 'exportTableToPointInTime_s3SseKmsKeyId' - The ID of the KMS managed key used to encrypt the S3 bucket where export
-- data will be stored (if applicable).
--
-- 'tableArn', 'exportTableToPointInTime_tableArn' - The Amazon Resource Name (ARN) associated with the table to export.
--
-- 's3Bucket', 'exportTableToPointInTime_s3Bucket' - The name of the Amazon S3 bucket to export the snapshot to.
newExportTableToPointInTime ::
  -- | 'tableArn'
  Prelude.Text ->
  -- | 's3Bucket'
  Prelude.Text ->
  ExportTableToPointInTime
newExportTableToPointInTime pTableArn_ pS3Bucket_ =
  ExportTableToPointInTime'
    { clientToken =
        Prelude.Nothing,
      exportFormat = Prelude.Nothing,
      exportTime = Prelude.Nothing,
      s3BucketOwner = Prelude.Nothing,
      s3Prefix = Prelude.Nothing,
      s3SseAlgorithm = Prelude.Nothing,
      s3SseKmsKeyId = Prelude.Nothing,
      tableArn = pTableArn_,
      s3Bucket = pS3Bucket_
    }

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
-- @ImportConflictException@.
exportTableToPointInTime_clientToken :: Lens.Lens' ExportTableToPointInTime (Prelude.Maybe Prelude.Text)
exportTableToPointInTime_clientToken = Lens.lens (\ExportTableToPointInTime' {clientToken} -> clientToken) (\s@ExportTableToPointInTime' {} a -> s {clientToken = a} :: ExportTableToPointInTime)

-- | The format for the exported data. Valid values for @ExportFormat@ are
-- @DYNAMODB_JSON@ or @ION@.
exportTableToPointInTime_exportFormat :: Lens.Lens' ExportTableToPointInTime (Prelude.Maybe ExportFormat)
exportTableToPointInTime_exportFormat = Lens.lens (\ExportTableToPointInTime' {exportFormat} -> exportFormat) (\s@ExportTableToPointInTime' {} a -> s {exportFormat = a} :: ExportTableToPointInTime)

-- | Time in the past from which to export table data, counted in seconds
-- from the start of the Unix epoch. The table export will be a snapshot of
-- the table\'s state at this point in time.
exportTableToPointInTime_exportTime :: Lens.Lens' ExportTableToPointInTime (Prelude.Maybe Prelude.UTCTime)
exportTableToPointInTime_exportTime = Lens.lens (\ExportTableToPointInTime' {exportTime} -> exportTime) (\s@ExportTableToPointInTime' {} a -> s {exportTime = a} :: ExportTableToPointInTime) Prelude.. Lens.mapping Data._Time

-- | The ID of the Amazon Web Services account that owns the bucket the
-- export will be stored in.
exportTableToPointInTime_s3BucketOwner :: Lens.Lens' ExportTableToPointInTime (Prelude.Maybe Prelude.Text)
exportTableToPointInTime_s3BucketOwner = Lens.lens (\ExportTableToPointInTime' {s3BucketOwner} -> s3BucketOwner) (\s@ExportTableToPointInTime' {} a -> s {s3BucketOwner = a} :: ExportTableToPointInTime)

-- | The Amazon S3 bucket prefix to use as the file name and path of the
-- exported snapshot.
exportTableToPointInTime_s3Prefix :: Lens.Lens' ExportTableToPointInTime (Prelude.Maybe Prelude.Text)
exportTableToPointInTime_s3Prefix = Lens.lens (\ExportTableToPointInTime' {s3Prefix} -> s3Prefix) (\s@ExportTableToPointInTime' {} a -> s {s3Prefix = a} :: ExportTableToPointInTime)

-- | Type of encryption used on the bucket where export data will be stored.
-- Valid values for @S3SseAlgorithm@ are:
--
-- -   @AES256@ - server-side encryption with Amazon S3 managed keys
--
-- -   @KMS@ - server-side encryption with KMS managed keys
exportTableToPointInTime_s3SseAlgorithm :: Lens.Lens' ExportTableToPointInTime (Prelude.Maybe S3SseAlgorithm)
exportTableToPointInTime_s3SseAlgorithm = Lens.lens (\ExportTableToPointInTime' {s3SseAlgorithm} -> s3SseAlgorithm) (\s@ExportTableToPointInTime' {} a -> s {s3SseAlgorithm = a} :: ExportTableToPointInTime)

-- | The ID of the KMS managed key used to encrypt the S3 bucket where export
-- data will be stored (if applicable).
exportTableToPointInTime_s3SseKmsKeyId :: Lens.Lens' ExportTableToPointInTime (Prelude.Maybe Prelude.Text)
exportTableToPointInTime_s3SseKmsKeyId = Lens.lens (\ExportTableToPointInTime' {s3SseKmsKeyId} -> s3SseKmsKeyId) (\s@ExportTableToPointInTime' {} a -> s {s3SseKmsKeyId = a} :: ExportTableToPointInTime)

-- | The Amazon Resource Name (ARN) associated with the table to export.
exportTableToPointInTime_tableArn :: Lens.Lens' ExportTableToPointInTime Prelude.Text
exportTableToPointInTime_tableArn = Lens.lens (\ExportTableToPointInTime' {tableArn} -> tableArn) (\s@ExportTableToPointInTime' {} a -> s {tableArn = a} :: ExportTableToPointInTime)

-- | The name of the Amazon S3 bucket to export the snapshot to.
exportTableToPointInTime_s3Bucket :: Lens.Lens' ExportTableToPointInTime Prelude.Text
exportTableToPointInTime_s3Bucket = Lens.lens (\ExportTableToPointInTime' {s3Bucket} -> s3Bucket) (\s@ExportTableToPointInTime' {} a -> s {s3Bucket = a} :: ExportTableToPointInTime)

instance Core.AWSRequest ExportTableToPointInTime where
  type
    AWSResponse ExportTableToPointInTime =
      ExportTableToPointInTimeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportTableToPointInTimeResponse'
            Prelude.<$> (x Data..?> "ExportDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExportTableToPointInTime where
  hashWithSalt _salt ExportTableToPointInTime' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` exportFormat
      `Prelude.hashWithSalt` exportTime
      `Prelude.hashWithSalt` s3BucketOwner
      `Prelude.hashWithSalt` s3Prefix
      `Prelude.hashWithSalt` s3SseAlgorithm
      `Prelude.hashWithSalt` s3SseKmsKeyId
      `Prelude.hashWithSalt` tableArn
      `Prelude.hashWithSalt` s3Bucket

instance Prelude.NFData ExportTableToPointInTime where
  rnf ExportTableToPointInTime' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf exportFormat
      `Prelude.seq` Prelude.rnf exportTime
      `Prelude.seq` Prelude.rnf s3BucketOwner
      `Prelude.seq` Prelude.rnf s3Prefix
      `Prelude.seq` Prelude.rnf s3SseAlgorithm
      `Prelude.seq` Prelude.rnf s3SseKmsKeyId
      `Prelude.seq` Prelude.rnf tableArn
      `Prelude.seq` Prelude.rnf s3Bucket

instance Data.ToHeaders ExportTableToPointInTime where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.ExportTableToPointInTime" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ExportTableToPointInTime where
  toJSON ExportTableToPointInTime' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("ExportFormat" Data..=) Prelude.<$> exportFormat,
            ("ExportTime" Data..=) Prelude.<$> exportTime,
            ("S3BucketOwner" Data..=) Prelude.<$> s3BucketOwner,
            ("S3Prefix" Data..=) Prelude.<$> s3Prefix,
            ("S3SseAlgorithm" Data..=)
              Prelude.<$> s3SseAlgorithm,
            ("S3SseKmsKeyId" Data..=) Prelude.<$> s3SseKmsKeyId,
            Prelude.Just ("TableArn" Data..= tableArn),
            Prelude.Just ("S3Bucket" Data..= s3Bucket)
          ]
      )

instance Data.ToPath ExportTableToPointInTime where
  toPath = Prelude.const "/"

instance Data.ToQuery ExportTableToPointInTime where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExportTableToPointInTimeResponse' smart constructor.
data ExportTableToPointInTimeResponse = ExportTableToPointInTimeResponse'
  { -- | Contains a description of the table export.
    exportDescription :: Prelude.Maybe ExportDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ExportTableToPointInTimeResponse
newExportTableToPointInTimeResponse pHttpStatus_ =
  ExportTableToPointInTimeResponse'
    { exportDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains a description of the table export.
exportTableToPointInTimeResponse_exportDescription :: Lens.Lens' ExportTableToPointInTimeResponse (Prelude.Maybe ExportDescription)
exportTableToPointInTimeResponse_exportDescription = Lens.lens (\ExportTableToPointInTimeResponse' {exportDescription} -> exportDescription) (\s@ExportTableToPointInTimeResponse' {} a -> s {exportDescription = a} :: ExportTableToPointInTimeResponse)

-- | The response's http status code.
exportTableToPointInTimeResponse_httpStatus :: Lens.Lens' ExportTableToPointInTimeResponse Prelude.Int
exportTableToPointInTimeResponse_httpStatus = Lens.lens (\ExportTableToPointInTimeResponse' {httpStatus} -> httpStatus) (\s@ExportTableToPointInTimeResponse' {} a -> s {httpStatus = a} :: ExportTableToPointInTimeResponse)

instance
  Prelude.NFData
    ExportTableToPointInTimeResponse
  where
  rnf ExportTableToPointInTimeResponse' {..} =
    Prelude.rnf exportDescription
      `Prelude.seq` Prelude.rnf httpStatus
