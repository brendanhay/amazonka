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
-- Module      : Network.AWS.DynamoDB.Types.ExportDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ExportDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.ExportFormat
import Network.AWS.DynamoDB.Types.ExportStatus
import Network.AWS.DynamoDB.Types.S3SseAlgorithm
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the properties of the exported table.
--
-- /See:/ 'newExportDescription' smart constructor.
data ExportDescription = ExportDescription'
  { -- | The format of the exported data. Valid values for @ExportFormat@ are
    -- @DYNAMODB_JSON@ or @ION@.
    exportFormat :: Prelude.Maybe ExportFormat,
    -- | Point in time from which table data was exported.
    exportTime :: Prelude.Maybe Core.POSIX,
    -- | The billable size of the table export.
    billedSizeBytes :: Prelude.Maybe Prelude.Natural,
    -- | The name of the Amazon S3 bucket containing the export.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the table that was exported.
    tableArn :: Prelude.Maybe Prelude.Text,
    -- | Unique ID of the table that was exported.
    tableId :: Prelude.Maybe Prelude.Text,
    -- | Export failure reason description.
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | Export can be in one of the following states: IN_PROGRESS, COMPLETED, or
    -- FAILED.
    exportStatus :: Prelude.Maybe ExportStatus,
    -- | The time at which the export task began.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | Status code for the result of the failed export.
    failureCode :: Prelude.Maybe Prelude.Text,
    -- | The time at which the export task completed.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of the AWS account that owns the bucket containing the export.
    s3BucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the table export.
    exportArn :: Prelude.Maybe Prelude.Text,
    -- | The number of items exported.
    itemCount :: Prelude.Maybe Prelude.Natural,
    -- | The name of the manifest file for the export task.
    exportManifest :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket prefix used as the file name and path of the
    -- exported snapshot.
    s3Prefix :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AWS KMS managed key used to encrypt the S3 bucket where
    -- export data is stored (if applicable).
    s3SseKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The client token that was provided for the export task. A client token
    -- makes calls to @ExportTableToPointInTimeInput@ idempotent, meaning that
    -- multiple identical calls have the same effect as one single call.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Type of encryption used on the bucket where export data is stored. Valid
    -- values for @S3SseAlgorithm@ are:
    --
    -- -   @AES256@ - server-side encryption with Amazon S3 managed keys
    --
    -- -   @KMS@ - server-side encryption with AWS KMS managed keys
    s3SseAlgorithm :: Prelude.Maybe S3SseAlgorithm
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
-- 'exportFormat', 'exportDescription_exportFormat' - The format of the exported data. Valid values for @ExportFormat@ are
-- @DYNAMODB_JSON@ or @ION@.
--
-- 'exportTime', 'exportDescription_exportTime' - Point in time from which table data was exported.
--
-- 'billedSizeBytes', 'exportDescription_billedSizeBytes' - The billable size of the table export.
--
-- 's3Bucket', 'exportDescription_s3Bucket' - The name of the Amazon S3 bucket containing the export.
--
-- 'tableArn', 'exportDescription_tableArn' - The Amazon Resource Name (ARN) of the table that was exported.
--
-- 'tableId', 'exportDescription_tableId' - Unique ID of the table that was exported.
--
-- 'failureMessage', 'exportDescription_failureMessage' - Export failure reason description.
--
-- 'exportStatus', 'exportDescription_exportStatus' - Export can be in one of the following states: IN_PROGRESS, COMPLETED, or
-- FAILED.
--
-- 'startTime', 'exportDescription_startTime' - The time at which the export task began.
--
-- 'failureCode', 'exportDescription_failureCode' - Status code for the result of the failed export.
--
-- 'endTime', 'exportDescription_endTime' - The time at which the export task completed.
--
-- 's3BucketOwner', 'exportDescription_s3BucketOwner' - The ID of the AWS account that owns the bucket containing the export.
--
-- 'exportArn', 'exportDescription_exportArn' - The Amazon Resource Name (ARN) of the table export.
--
-- 'itemCount', 'exportDescription_itemCount' - The number of items exported.
--
-- 'exportManifest', 'exportDescription_exportManifest' - The name of the manifest file for the export task.
--
-- 's3Prefix', 'exportDescription_s3Prefix' - The Amazon S3 bucket prefix used as the file name and path of the
-- exported snapshot.
--
-- 's3SseKmsKeyId', 'exportDescription_s3SseKmsKeyId' - The ID of the AWS KMS managed key used to encrypt the S3 bucket where
-- export data is stored (if applicable).
--
-- 'clientToken', 'exportDescription_clientToken' - The client token that was provided for the export task. A client token
-- makes calls to @ExportTableToPointInTimeInput@ idempotent, meaning that
-- multiple identical calls have the same effect as one single call.
--
-- 's3SseAlgorithm', 'exportDescription_s3SseAlgorithm' - Type of encryption used on the bucket where export data is stored. Valid
-- values for @S3SseAlgorithm@ are:
--
-- -   @AES256@ - server-side encryption with Amazon S3 managed keys
--
-- -   @KMS@ - server-side encryption with AWS KMS managed keys
newExportDescription ::
  ExportDescription
newExportDescription =
  ExportDescription'
    { exportFormat = Prelude.Nothing,
      exportTime = Prelude.Nothing,
      billedSizeBytes = Prelude.Nothing,
      s3Bucket = Prelude.Nothing,
      tableArn = Prelude.Nothing,
      tableId = Prelude.Nothing,
      failureMessage = Prelude.Nothing,
      exportStatus = Prelude.Nothing,
      startTime = Prelude.Nothing,
      failureCode = Prelude.Nothing,
      endTime = Prelude.Nothing,
      s3BucketOwner = Prelude.Nothing,
      exportArn = Prelude.Nothing,
      itemCount = Prelude.Nothing,
      exportManifest = Prelude.Nothing,
      s3Prefix = Prelude.Nothing,
      s3SseKmsKeyId = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      s3SseAlgorithm = Prelude.Nothing
    }

-- | The format of the exported data. Valid values for @ExportFormat@ are
-- @DYNAMODB_JSON@ or @ION@.
exportDescription_exportFormat :: Lens.Lens' ExportDescription (Prelude.Maybe ExportFormat)
exportDescription_exportFormat = Lens.lens (\ExportDescription' {exportFormat} -> exportFormat) (\s@ExportDescription' {} a -> s {exportFormat = a} :: ExportDescription)

-- | Point in time from which table data was exported.
exportDescription_exportTime :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.UTCTime)
exportDescription_exportTime = Lens.lens (\ExportDescription' {exportTime} -> exportTime) (\s@ExportDescription' {} a -> s {exportTime = a} :: ExportDescription) Prelude.. Lens.mapping Core._Time

-- | The billable size of the table export.
exportDescription_billedSizeBytes :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Natural)
exportDescription_billedSizeBytes = Lens.lens (\ExportDescription' {billedSizeBytes} -> billedSizeBytes) (\s@ExportDescription' {} a -> s {billedSizeBytes = a} :: ExportDescription)

-- | The name of the Amazon S3 bucket containing the export.
exportDescription_s3Bucket :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Text)
exportDescription_s3Bucket = Lens.lens (\ExportDescription' {s3Bucket} -> s3Bucket) (\s@ExportDescription' {} a -> s {s3Bucket = a} :: ExportDescription)

-- | The Amazon Resource Name (ARN) of the table that was exported.
exportDescription_tableArn :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Text)
exportDescription_tableArn = Lens.lens (\ExportDescription' {tableArn} -> tableArn) (\s@ExportDescription' {} a -> s {tableArn = a} :: ExportDescription)

-- | Unique ID of the table that was exported.
exportDescription_tableId :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Text)
exportDescription_tableId = Lens.lens (\ExportDescription' {tableId} -> tableId) (\s@ExportDescription' {} a -> s {tableId = a} :: ExportDescription)

-- | Export failure reason description.
exportDescription_failureMessage :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Text)
exportDescription_failureMessage = Lens.lens (\ExportDescription' {failureMessage} -> failureMessage) (\s@ExportDescription' {} a -> s {failureMessage = a} :: ExportDescription)

-- | Export can be in one of the following states: IN_PROGRESS, COMPLETED, or
-- FAILED.
exportDescription_exportStatus :: Lens.Lens' ExportDescription (Prelude.Maybe ExportStatus)
exportDescription_exportStatus = Lens.lens (\ExportDescription' {exportStatus} -> exportStatus) (\s@ExportDescription' {} a -> s {exportStatus = a} :: ExportDescription)

-- | The time at which the export task began.
exportDescription_startTime :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.UTCTime)
exportDescription_startTime = Lens.lens (\ExportDescription' {startTime} -> startTime) (\s@ExportDescription' {} a -> s {startTime = a} :: ExportDescription) Prelude.. Lens.mapping Core._Time

-- | Status code for the result of the failed export.
exportDescription_failureCode :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Text)
exportDescription_failureCode = Lens.lens (\ExportDescription' {failureCode} -> failureCode) (\s@ExportDescription' {} a -> s {failureCode = a} :: ExportDescription)

-- | The time at which the export task completed.
exportDescription_endTime :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.UTCTime)
exportDescription_endTime = Lens.lens (\ExportDescription' {endTime} -> endTime) (\s@ExportDescription' {} a -> s {endTime = a} :: ExportDescription) Prelude.. Lens.mapping Core._Time

-- | The ID of the AWS account that owns the bucket containing the export.
exportDescription_s3BucketOwner :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Text)
exportDescription_s3BucketOwner = Lens.lens (\ExportDescription' {s3BucketOwner} -> s3BucketOwner) (\s@ExportDescription' {} a -> s {s3BucketOwner = a} :: ExportDescription)

-- | The Amazon Resource Name (ARN) of the table export.
exportDescription_exportArn :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Text)
exportDescription_exportArn = Lens.lens (\ExportDescription' {exportArn} -> exportArn) (\s@ExportDescription' {} a -> s {exportArn = a} :: ExportDescription)

-- | The number of items exported.
exportDescription_itemCount :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Natural)
exportDescription_itemCount = Lens.lens (\ExportDescription' {itemCount} -> itemCount) (\s@ExportDescription' {} a -> s {itemCount = a} :: ExportDescription)

-- | The name of the manifest file for the export task.
exportDescription_exportManifest :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Text)
exportDescription_exportManifest = Lens.lens (\ExportDescription' {exportManifest} -> exportManifest) (\s@ExportDescription' {} a -> s {exportManifest = a} :: ExportDescription)

-- | The Amazon S3 bucket prefix used as the file name and path of the
-- exported snapshot.
exportDescription_s3Prefix :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Text)
exportDescription_s3Prefix = Lens.lens (\ExportDescription' {s3Prefix} -> s3Prefix) (\s@ExportDescription' {} a -> s {s3Prefix = a} :: ExportDescription)

-- | The ID of the AWS KMS managed key used to encrypt the S3 bucket where
-- export data is stored (if applicable).
exportDescription_s3SseKmsKeyId :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Text)
exportDescription_s3SseKmsKeyId = Lens.lens (\ExportDescription' {s3SseKmsKeyId} -> s3SseKmsKeyId) (\s@ExportDescription' {} a -> s {s3SseKmsKeyId = a} :: ExportDescription)

-- | The client token that was provided for the export task. A client token
-- makes calls to @ExportTableToPointInTimeInput@ idempotent, meaning that
-- multiple identical calls have the same effect as one single call.
exportDescription_clientToken :: Lens.Lens' ExportDescription (Prelude.Maybe Prelude.Text)
exportDescription_clientToken = Lens.lens (\ExportDescription' {clientToken} -> clientToken) (\s@ExportDescription' {} a -> s {clientToken = a} :: ExportDescription)

-- | Type of encryption used on the bucket where export data is stored. Valid
-- values for @S3SseAlgorithm@ are:
--
-- -   @AES256@ - server-side encryption with Amazon S3 managed keys
--
-- -   @KMS@ - server-side encryption with AWS KMS managed keys
exportDescription_s3SseAlgorithm :: Lens.Lens' ExportDescription (Prelude.Maybe S3SseAlgorithm)
exportDescription_s3SseAlgorithm = Lens.lens (\ExportDescription' {s3SseAlgorithm} -> s3SseAlgorithm) (\s@ExportDescription' {} a -> s {s3SseAlgorithm = a} :: ExportDescription)

instance Core.FromJSON ExportDescription where
  parseJSON =
    Core.withObject
      "ExportDescription"
      ( \x ->
          ExportDescription'
            Prelude.<$> (x Core..:? "ExportFormat")
            Prelude.<*> (x Core..:? "ExportTime")
            Prelude.<*> (x Core..:? "BilledSizeBytes")
            Prelude.<*> (x Core..:? "S3Bucket")
            Prelude.<*> (x Core..:? "TableArn")
            Prelude.<*> (x Core..:? "TableId")
            Prelude.<*> (x Core..:? "FailureMessage")
            Prelude.<*> (x Core..:? "ExportStatus")
            Prelude.<*> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "FailureCode")
            Prelude.<*> (x Core..:? "EndTime")
            Prelude.<*> (x Core..:? "S3BucketOwner")
            Prelude.<*> (x Core..:? "ExportArn")
            Prelude.<*> (x Core..:? "ItemCount")
            Prelude.<*> (x Core..:? "ExportManifest")
            Prelude.<*> (x Core..:? "S3Prefix")
            Prelude.<*> (x Core..:? "S3SseKmsKeyId")
            Prelude.<*> (x Core..:? "ClientToken")
            Prelude.<*> (x Core..:? "S3SseAlgorithm")
      )

instance Prelude.Hashable ExportDescription

instance Prelude.NFData ExportDescription
