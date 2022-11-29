{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DynamoDB.Types.ExportDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ExportDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.ExportFormat
import Amazonka.DynamoDB.Types.ExportStatus
import Amazonka.DynamoDB.Types.S3SseAlgorithm
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the properties of the exported table.
--
-- /See:/ 'newExportDescription' smart constructor.
data ExportDescription = ExportDescription'
  { -- | The name of the Amazon S3 bucket containing the export.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The client token that was provided for the export task. A client token
    -- makes calls to @ExportTableToPointInTimeInput@ idempotent, meaning that
    -- multiple identical calls have the same effect as one single call.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the table that was exported.
    tableArn :: Prelude.Maybe Prelude.Text,
    -- | Status code for the result of the failed export.
    failureCode :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the table export.
    exportArn :: Prelude.Maybe Prelude.Text,
    -- | The billable size of the table export.
    billedSizeBytes :: Prelude.Maybe Prelude.Natural,
    -- | Type of encryption used on the bucket where export data is stored. Valid
    -- values for @S3SseAlgorithm@ are:
    --
    -- -   @AES256@ - server-side encryption with Amazon S3 managed keys
    --
    -- -   @KMS@ - server-side encryption with KMS managed keys
    s3SseAlgorithm :: Prelude.Maybe S3SseAlgorithm,
    -- | The number of items exported.
    itemCount :: Prelude.Maybe Prelude.Natural,
    -- | The time at which the export task completed.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | Export failure reason description.
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | Unique ID of the table that was exported.
    tableId :: Prelude.Maybe Prelude.Text,
    -- | The format of the exported data. Valid values for @ExportFormat@ are
    -- @DYNAMODB_JSON@ or @ION@.
    exportFormat :: Prelude.Maybe ExportFormat,
    -- | Point in time from which table data was exported.
    exportTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of the Amazon Web Services account that owns the bucket
    -- containing the export.
    s3BucketOwner :: Prelude.Maybe Prelude.Text,
    -- | Export can be in one of the following states: IN_PROGRESS, COMPLETED, or
    -- FAILED.
    exportStatus :: Prelude.Maybe ExportStatus,
    -- | The ID of the KMS managed key used to encrypt the S3 bucket where export
    -- data is stored (if applicable).
    s3SseKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the manifest file for the export task.
    exportManifest :: Prelude.Maybe Prelude.Text,
    -- | The time at which the export task began.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon S3 bucket prefix used as the file name and path of the
    -- exported snapshot.
    s3Prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Bucket', 'exportDescription_s3Bucket' - The name of the Amazon S3 bucket containing the export.
--
-- 'clientToken', 'exportDescription_clientToken' - The client token that was provided for the export task. A client token
-- makes calls to @ExportTableToPointInTimeInput@ idempotent, meaning that
-- multiple identical calls have the same effect as one single call.
--
-- 'tableArn', 'exportDescription_tableArn' - The Amazon Resource Name (ARN) of the table that was exported.
--
-- 'failureCode', 'exportDescription_failureCode' - Status code for the result of the failed export.
--
-- 'exportArn', 'exportDescription_exportArn' - The Amazon Resource Name (ARN) of the table export.
--
-- 'billedSizeBytes', 'exportDescription_billedSizeBytes' - The billable size of the table export.
--
-- 's3SseAlgorithm', 'exportDescription_s3SseAlgorithm' - Type of encryption used on the bucket where export data is stored. Valid
-- values for @S3SseAlgorithm@ are:
--
-- -   @AES256@ - server-side encryption with Amazon S3 managed keys
--
-- -   @KMS@ - server-side encryption with KMS managed keys
--
-- 'itemCount', 'exportDescription_itemCount' - The number of items exported.
--
-- 'endTime', 'exportDescription_endTime' - The time at which the export task completed.
--
-- 'failureMessage', 'exportDescription_failureMessage' - Export failure reason description.
--
-- 'tableId', 'exportDescription_tableId' - Unique ID of the table that was exported.
--
-- 'exportFormat', 'exportDescription_exportFormat' - The format of the exported data. Valid values for @ExportFormat@ are
-- @DYNAMODB_JSON@ or @ION@.
--
-- 'exportTime', 'exportDescription_exportTime' - Point in time from which table data was exported.
--
-- 's3BucketOwner', 'exportDescription_s3BucketOwner' - The ID of the Amazon Web Services account that owns the bucket
-- containing the export.
--
-- 'exportStatus', 'exportDescription_exportStatus' - Export can be in one of the following states: IN_PROGRESS, COMPLETED, or
-- FAILED.
--
-- 's3SseKmsKeyId', 'exportDescription_s3SseKmsKeyId' - The ID of the KMS managed key used to encrypt the S3 bucket where export
-- data is stored (if applicable).
--
-- 'exportManifest', 'exportDescription_exportManifest' - The name of the manifest file for the export task.
--
-- 'startTime', 'exportDescription_startTime' - The time at which the export task began.
--
-- 's3Prefix', 'exportDescription_s3Prefix' - The Amazon S3 bucket prefix used as the file name and path of the
-- exported snapshot.
newExportDescription ::
  ExportDescription
newExportDescription =
  ExportDescription'
    { s3Bucket = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      tableArn = Prelude.Nothing,
      failureCode = Prelude.Nothing,
      exportArn = Prelude.Nothing,
      billedSizeBytes = Prelude.Nothing,
      s3SseAlgorithm = Prelude.Nothing,
      itemCount = Prelude.Nothing,
      endTime = Prelude.Nothing,
      failureMessage = Prelude.Nothing,
      tableId = Prelude.Nothing,
      exportFormat = Prelude.Nothing,
      exportTime = Prelude.Nothing,
      s3BucketOwner = Prelude.Nothing,
      exportStatus = Prelude.Nothing,
      s3SseKmsKeyId = Prelude.Nothing,
      exportManifest = Prelude.Nothing,
      startTime = Prelude.Nothing,
      s3Prefix = Prelude.Nothing
    }

-- | The name of the Amazon S3 bucket containing the export.
exportDescription_s3Bucket :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Text)
exportDescription_s3Bucket = Lens.lens (\ExportDescription' {s3Bucket} -> s3Bucket) (\s@ExportDescription' {} a -> s {s3Bucket = a} :: ExportDescription)

-- | The client token that was provided for the export task. A client token
-- makes calls to @ExportTableToPointInTimeInput@ idempotent, meaning that
-- multiple identical calls have the same effect as one single call.
exportDescription_clientToken :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Text)
exportDescription_clientToken = Lens.lens (\ExportDescription' {clientToken} -> clientToken) (\s@ExportDescription' {} a -> s {clientToken = a} :: ExportDescription)

-- | The Amazon Resource Name (ARN) of the table that was exported.
exportDescription_tableArn :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Text)
exportDescription_tableArn = Lens.lens (\ExportDescription' {tableArn} -> tableArn) (\s@ExportDescription' {} a -> s {tableArn = a} :: ExportDescription)

-- | Status code for the result of the failed export.
exportDescription_failureCode :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Text)
exportDescription_failureCode = Lens.lens (\ExportDescription' {failureCode} -> failureCode) (\s@ExportDescription' {} a -> s {failureCode = a} :: ExportDescription)

-- | The Amazon Resource Name (ARN) of the table export.
exportDescription_exportArn :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Text)
exportDescription_exportArn = Lens.lens (\ExportDescription' {exportArn} -> exportArn) (\s@ExportDescription' {} a -> s {exportArn = a} :: ExportDescription)

-- | The billable size of the table export.
exportDescription_billedSizeBytes :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Natural)
exportDescription_billedSizeBytes = Lens.lens (\ExportDescription' {billedSizeBytes} -> billedSizeBytes) (\s@ExportDescription' {} a -> s {billedSizeBytes = a} :: ExportDescription)

-- | Type of encryption used on the bucket where export data is stored. Valid
-- values for @S3SseAlgorithm@ are:
--
-- -   @AES256@ - server-side encryption with Amazon S3 managed keys
--
-- -   @KMS@ - server-side encryption with KMS managed keys
exportDescription_s3SseAlgorithm :: Lens.Lens' ExportDescription (Prelude.Maybe S3SseAlgorithm)
exportDescription_s3SseAlgorithm = Lens.lens (\ExportDescription' {s3SseAlgorithm} -> s3SseAlgorithm) (\s@ExportDescription' {} a -> s {s3SseAlgorithm = a} :: ExportDescription)

-- | The number of items exported.
exportDescription_itemCount :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Natural)
exportDescription_itemCount = Lens.lens (\ExportDescription' {itemCount} -> itemCount) (\s@ExportDescription' {} a -> s {itemCount = a} :: ExportDescription)

-- | The time at which the export task completed.
exportDescription_endTime :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.UTCTime)
exportDescription_endTime = Lens.lens (\ExportDescription' {endTime} -> endTime) (\s@ExportDescription' {} a -> s {endTime = a} :: ExportDescription) Prelude.. Lens.mapping Core._Time

-- | Export failure reason description.
exportDescription_failureMessage :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Text)
exportDescription_failureMessage = Lens.lens (\ExportDescription' {failureMessage} -> failureMessage) (\s@ExportDescription' {} a -> s {failureMessage = a} :: ExportDescription)

-- | Unique ID of the table that was exported.
exportDescription_tableId :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Text)
exportDescription_tableId = Lens.lens (\ExportDescription' {tableId} -> tableId) (\s@ExportDescription' {} a -> s {tableId = a} :: ExportDescription)

-- | The format of the exported data. Valid values for @ExportFormat@ are
-- @DYNAMODB_JSON@ or @ION@.
exportDescription_exportFormat :: Lens.Lens' ExportDescription (Prelude.Maybe ExportFormat)
exportDescription_exportFormat = Lens.lens (\ExportDescription' {exportFormat} -> exportFormat) (\s@ExportDescription' {} a -> s {exportFormat = a} :: ExportDescription)

-- | Point in time from which table data was exported.
exportDescription_exportTime :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.UTCTime)
exportDescription_exportTime = Lens.lens (\ExportDescription' {exportTime} -> exportTime) (\s@ExportDescription' {} a -> s {exportTime = a} :: ExportDescription) Prelude.. Lens.mapping Core._Time

-- | The ID of the Amazon Web Services account that owns the bucket
-- containing the export.
exportDescription_s3BucketOwner :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Text)
exportDescription_s3BucketOwner = Lens.lens (\ExportDescription' {s3BucketOwner} -> s3BucketOwner) (\s@ExportDescription' {} a -> s {s3BucketOwner = a} :: ExportDescription)

-- | Export can be in one of the following states: IN_PROGRESS, COMPLETED, or
-- FAILED.
exportDescription_exportStatus :: Lens.Lens' ExportDescription (Prelude.Maybe ExportStatus)
exportDescription_exportStatus = Lens.lens (\ExportDescription' {exportStatus} -> exportStatus) (\s@ExportDescription' {} a -> s {exportStatus = a} :: ExportDescription)

-- | The ID of the KMS managed key used to encrypt the S3 bucket where export
-- data is stored (if applicable).
exportDescription_s3SseKmsKeyId :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Text)
exportDescription_s3SseKmsKeyId = Lens.lens (\ExportDescription' {s3SseKmsKeyId} -> s3SseKmsKeyId) (\s@ExportDescription' {} a -> s {s3SseKmsKeyId = a} :: ExportDescription)

-- | The name of the manifest file for the export task.
exportDescription_exportManifest :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Text)
exportDescription_exportManifest = Lens.lens (\ExportDescription' {exportManifest} -> exportManifest) (\s@ExportDescription' {} a -> s {exportManifest = a} :: ExportDescription)

-- | The time at which the export task began.
exportDescription_startTime :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.UTCTime)
exportDescription_startTime = Lens.lens (\ExportDescription' {startTime} -> startTime) (\s@ExportDescription' {} a -> s {startTime = a} :: ExportDescription) Prelude.. Lens.mapping Core._Time

-- | The Amazon S3 bucket prefix used as the file name and path of the
-- exported snapshot.
exportDescription_s3Prefix :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Text)
exportDescription_s3Prefix = Lens.lens (\ExportDescription' {s3Prefix} -> s3Prefix) (\s@ExportDescription' {} a -> s {s3Prefix = a} :: ExportDescription)

instance Core.FromJSON ExportDescription where
  parseJSON =
    Core.withObject
      "ExportDescription"
      ( \x ->
          ExportDescription'
            Prelude.<$> (x Core..:? "S3Bucket")
            Prelude.<*> (x Core..:? "ClientToken")
            Prelude.<*> (x Core..:? "TableArn")
            Prelude.<*> (x Core..:? "FailureCode")
            Prelude.<*> (x Core..:? "ExportArn")
            Prelude.<*> (x Core..:? "BilledSizeBytes")
            Prelude.<*> (x Core..:? "S3SseAlgorithm")
            Prelude.<*> (x Core..:? "ItemCount")
            Prelude.<*> (x Core..:? "EndTime")
            Prelude.<*> (x Core..:? "FailureMessage")
            Prelude.<*> (x Core..:? "TableId")
            Prelude.<*> (x Core..:? "ExportFormat")
            Prelude.<*> (x Core..:? "ExportTime")
            Prelude.<*> (x Core..:? "S3BucketOwner")
            Prelude.<*> (x Core..:? "ExportStatus")
            Prelude.<*> (x Core..:? "S3SseKmsKeyId")
            Prelude.<*> (x Core..:? "ExportManifest")
            Prelude.<*> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "S3Prefix")
      )

instance Prelude.Hashable ExportDescription where
  hashWithSalt _salt ExportDescription' {..} =
    _salt `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tableArn
      `Prelude.hashWithSalt` failureCode
      `Prelude.hashWithSalt` exportArn
      `Prelude.hashWithSalt` billedSizeBytes
      `Prelude.hashWithSalt` s3SseAlgorithm
      `Prelude.hashWithSalt` itemCount
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` failureMessage
      `Prelude.hashWithSalt` tableId
      `Prelude.hashWithSalt` exportFormat
      `Prelude.hashWithSalt` exportTime
      `Prelude.hashWithSalt` s3BucketOwner
      `Prelude.hashWithSalt` exportStatus
      `Prelude.hashWithSalt` s3SseKmsKeyId
      `Prelude.hashWithSalt` exportManifest
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` s3Prefix

instance Prelude.NFData ExportDescription where
  rnf ExportDescription' {..} =
    Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf tableArn
      `Prelude.seq` Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf exportArn
      `Prelude.seq` Prelude.rnf billedSizeBytes
      `Prelude.seq` Prelude.rnf s3SseAlgorithm
      `Prelude.seq` Prelude.rnf itemCount
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf failureMessage
      `Prelude.seq` Prelude.rnf tableId
      `Prelude.seq` Prelude.rnf exportFormat
      `Prelude.seq` Prelude.rnf exportTime
      `Prelude.seq` Prelude.rnf s3BucketOwner
      `Prelude.seq` Prelude.rnf exportStatus
      `Prelude.seq` Prelude.rnf s3SseKmsKeyId
      `Prelude.seq` Prelude.rnf exportManifest
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf s3Prefix
