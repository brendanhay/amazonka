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
-- Module      : Amazonka.DynamoDB.Types.ImportTableDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ImportTableDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.ImportStatus
import Amazonka.DynamoDB.Types.InputCompressionType
import Amazonka.DynamoDB.Types.InputFormat
import Amazonka.DynamoDB.Types.InputFormatOptions
import Amazonka.DynamoDB.Types.S3BucketSource
import Amazonka.DynamoDB.Types.TableCreationParameters
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the properties of the table being imported into.
--
-- /See:/ 'newImportTableDescription' smart constructor.
data ImportTableDescription = ImportTableDescription'
  { -- | The client token that was provided for the import task. Reusing the
    -- client token on retry makes a call to @ImportTable@ idempotent.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Number (ARN) of the Cloudwatch Log Group associated
    -- with the target table.
    cloudWatchLogGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the creation of the table associated with this import
    -- task completed.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The number of errors occurred on importing the source file into the
    -- target table.
    errorCount :: Prelude.Maybe Prelude.Natural,
    -- | The error code corresponding to the failure that the import job ran into
    -- during execution.
    failureCode :: Prelude.Maybe Prelude.Text,
    -- | The error message corresponding to the failure that the import job ran
    -- into during execution.
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Number (ARN) corresponding to the import request.
    importArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the import.
    importStatus :: Prelude.Maybe ImportStatus,
    -- | The number of items successfully imported into the new table.
    importedItemCount :: Prelude.Maybe Prelude.Natural,
    -- | The compression options for the data that has been imported into the
    -- target table. The values are NONE, GZIP, or ZSTD.
    inputCompressionType :: Prelude.Maybe InputCompressionType,
    -- | The format of the source data going into the target table.
    inputFormat :: Prelude.Maybe InputFormat,
    -- | The format options for the data that was imported into the target table.
    -- There is one value, CsvOption.
    inputFormatOptions :: Prelude.Maybe InputFormatOptions,
    -- | The total number of items processed from the source file.
    processedItemCount :: Prelude.Maybe Prelude.Natural,
    -- | The total size of data processed from the source file, in Bytes.
    processedSizeBytes :: Prelude.Maybe Prelude.Integer,
    -- | Values for the S3 bucket the source file is imported from. Includes
    -- bucket name (required), key prefix (optional) and bucket account owner
    -- ID (optional).
    s3BucketSource :: Prelude.Maybe S3BucketSource,
    -- | The time when this import task started.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Number (ARN) of the table being imported into.
    tableArn :: Prelude.Maybe Prelude.Text,
    -- | The parameters for the new table that is being imported into.
    tableCreationParameters :: Prelude.Maybe TableCreationParameters,
    -- | The table id corresponding to the table created by import table process.
    tableId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportTableDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'importTableDescription_clientToken' - The client token that was provided for the import task. Reusing the
-- client token on retry makes a call to @ImportTable@ idempotent.
--
-- 'cloudWatchLogGroupArn', 'importTableDescription_cloudWatchLogGroupArn' - The Amazon Resource Number (ARN) of the Cloudwatch Log Group associated
-- with the target table.
--
-- 'endTime', 'importTableDescription_endTime' - The time at which the creation of the table associated with this import
-- task completed.
--
-- 'errorCount', 'importTableDescription_errorCount' - The number of errors occurred on importing the source file into the
-- target table.
--
-- 'failureCode', 'importTableDescription_failureCode' - The error code corresponding to the failure that the import job ran into
-- during execution.
--
-- 'failureMessage', 'importTableDescription_failureMessage' - The error message corresponding to the failure that the import job ran
-- into during execution.
--
-- 'importArn', 'importTableDescription_importArn' - The Amazon Resource Number (ARN) corresponding to the import request.
--
-- 'importStatus', 'importTableDescription_importStatus' - The status of the import.
--
-- 'importedItemCount', 'importTableDescription_importedItemCount' - The number of items successfully imported into the new table.
--
-- 'inputCompressionType', 'importTableDescription_inputCompressionType' - The compression options for the data that has been imported into the
-- target table. The values are NONE, GZIP, or ZSTD.
--
-- 'inputFormat', 'importTableDescription_inputFormat' - The format of the source data going into the target table.
--
-- 'inputFormatOptions', 'importTableDescription_inputFormatOptions' - The format options for the data that was imported into the target table.
-- There is one value, CsvOption.
--
-- 'processedItemCount', 'importTableDescription_processedItemCount' - The total number of items processed from the source file.
--
-- 'processedSizeBytes', 'importTableDescription_processedSizeBytes' - The total size of data processed from the source file, in Bytes.
--
-- 's3BucketSource', 'importTableDescription_s3BucketSource' - Values for the S3 bucket the source file is imported from. Includes
-- bucket name (required), key prefix (optional) and bucket account owner
-- ID (optional).
--
-- 'startTime', 'importTableDescription_startTime' - The time when this import task started.
--
-- 'tableArn', 'importTableDescription_tableArn' - The Amazon Resource Number (ARN) of the table being imported into.
--
-- 'tableCreationParameters', 'importTableDescription_tableCreationParameters' - The parameters for the new table that is being imported into.
--
-- 'tableId', 'importTableDescription_tableId' - The table id corresponding to the table created by import table process.
newImportTableDescription ::
  ImportTableDescription
newImportTableDescription =
  ImportTableDescription'
    { clientToken =
        Prelude.Nothing,
      cloudWatchLogGroupArn = Prelude.Nothing,
      endTime = Prelude.Nothing,
      errorCount = Prelude.Nothing,
      failureCode = Prelude.Nothing,
      failureMessage = Prelude.Nothing,
      importArn = Prelude.Nothing,
      importStatus = Prelude.Nothing,
      importedItemCount = Prelude.Nothing,
      inputCompressionType = Prelude.Nothing,
      inputFormat = Prelude.Nothing,
      inputFormatOptions = Prelude.Nothing,
      processedItemCount = Prelude.Nothing,
      processedSizeBytes = Prelude.Nothing,
      s3BucketSource = Prelude.Nothing,
      startTime = Prelude.Nothing,
      tableArn = Prelude.Nothing,
      tableCreationParameters = Prelude.Nothing,
      tableId = Prelude.Nothing
    }

-- | The client token that was provided for the import task. Reusing the
-- client token on retry makes a call to @ImportTable@ idempotent.
importTableDescription_clientToken :: Lens.Lens' ImportTableDescription (Prelude.Maybe Prelude.Text)
importTableDescription_clientToken = Lens.lens (\ImportTableDescription' {clientToken} -> clientToken) (\s@ImportTableDescription' {} a -> s {clientToken = a} :: ImportTableDescription)

-- | The Amazon Resource Number (ARN) of the Cloudwatch Log Group associated
-- with the target table.
importTableDescription_cloudWatchLogGroupArn :: Lens.Lens' ImportTableDescription (Prelude.Maybe Prelude.Text)
importTableDescription_cloudWatchLogGroupArn = Lens.lens (\ImportTableDescription' {cloudWatchLogGroupArn} -> cloudWatchLogGroupArn) (\s@ImportTableDescription' {} a -> s {cloudWatchLogGroupArn = a} :: ImportTableDescription)

-- | The time at which the creation of the table associated with this import
-- task completed.
importTableDescription_endTime :: Lens.Lens' ImportTableDescription (Prelude.Maybe Prelude.UTCTime)
importTableDescription_endTime = Lens.lens (\ImportTableDescription' {endTime} -> endTime) (\s@ImportTableDescription' {} a -> s {endTime = a} :: ImportTableDescription) Prelude.. Lens.mapping Data._Time

-- | The number of errors occurred on importing the source file into the
-- target table.
importTableDescription_errorCount :: Lens.Lens' ImportTableDescription (Prelude.Maybe Prelude.Natural)
importTableDescription_errorCount = Lens.lens (\ImportTableDescription' {errorCount} -> errorCount) (\s@ImportTableDescription' {} a -> s {errorCount = a} :: ImportTableDescription)

-- | The error code corresponding to the failure that the import job ran into
-- during execution.
importTableDescription_failureCode :: Lens.Lens' ImportTableDescription (Prelude.Maybe Prelude.Text)
importTableDescription_failureCode = Lens.lens (\ImportTableDescription' {failureCode} -> failureCode) (\s@ImportTableDescription' {} a -> s {failureCode = a} :: ImportTableDescription)

-- | The error message corresponding to the failure that the import job ran
-- into during execution.
importTableDescription_failureMessage :: Lens.Lens' ImportTableDescription (Prelude.Maybe Prelude.Text)
importTableDescription_failureMessage = Lens.lens (\ImportTableDescription' {failureMessage} -> failureMessage) (\s@ImportTableDescription' {} a -> s {failureMessage = a} :: ImportTableDescription)

-- | The Amazon Resource Number (ARN) corresponding to the import request.
importTableDescription_importArn :: Lens.Lens' ImportTableDescription (Prelude.Maybe Prelude.Text)
importTableDescription_importArn = Lens.lens (\ImportTableDescription' {importArn} -> importArn) (\s@ImportTableDescription' {} a -> s {importArn = a} :: ImportTableDescription)

-- | The status of the import.
importTableDescription_importStatus :: Lens.Lens' ImportTableDescription (Prelude.Maybe ImportStatus)
importTableDescription_importStatus = Lens.lens (\ImportTableDescription' {importStatus} -> importStatus) (\s@ImportTableDescription' {} a -> s {importStatus = a} :: ImportTableDescription)

-- | The number of items successfully imported into the new table.
importTableDescription_importedItemCount :: Lens.Lens' ImportTableDescription (Prelude.Maybe Prelude.Natural)
importTableDescription_importedItemCount = Lens.lens (\ImportTableDescription' {importedItemCount} -> importedItemCount) (\s@ImportTableDescription' {} a -> s {importedItemCount = a} :: ImportTableDescription)

-- | The compression options for the data that has been imported into the
-- target table. The values are NONE, GZIP, or ZSTD.
importTableDescription_inputCompressionType :: Lens.Lens' ImportTableDescription (Prelude.Maybe InputCompressionType)
importTableDescription_inputCompressionType = Lens.lens (\ImportTableDescription' {inputCompressionType} -> inputCompressionType) (\s@ImportTableDescription' {} a -> s {inputCompressionType = a} :: ImportTableDescription)

-- | The format of the source data going into the target table.
importTableDescription_inputFormat :: Lens.Lens' ImportTableDescription (Prelude.Maybe InputFormat)
importTableDescription_inputFormat = Lens.lens (\ImportTableDescription' {inputFormat} -> inputFormat) (\s@ImportTableDescription' {} a -> s {inputFormat = a} :: ImportTableDescription)

-- | The format options for the data that was imported into the target table.
-- There is one value, CsvOption.
importTableDescription_inputFormatOptions :: Lens.Lens' ImportTableDescription (Prelude.Maybe InputFormatOptions)
importTableDescription_inputFormatOptions = Lens.lens (\ImportTableDescription' {inputFormatOptions} -> inputFormatOptions) (\s@ImportTableDescription' {} a -> s {inputFormatOptions = a} :: ImportTableDescription)

-- | The total number of items processed from the source file.
importTableDescription_processedItemCount :: Lens.Lens' ImportTableDescription (Prelude.Maybe Prelude.Natural)
importTableDescription_processedItemCount = Lens.lens (\ImportTableDescription' {processedItemCount} -> processedItemCount) (\s@ImportTableDescription' {} a -> s {processedItemCount = a} :: ImportTableDescription)

-- | The total size of data processed from the source file, in Bytes.
importTableDescription_processedSizeBytes :: Lens.Lens' ImportTableDescription (Prelude.Maybe Prelude.Integer)
importTableDescription_processedSizeBytes = Lens.lens (\ImportTableDescription' {processedSizeBytes} -> processedSizeBytes) (\s@ImportTableDescription' {} a -> s {processedSizeBytes = a} :: ImportTableDescription)

-- | Values for the S3 bucket the source file is imported from. Includes
-- bucket name (required), key prefix (optional) and bucket account owner
-- ID (optional).
importTableDescription_s3BucketSource :: Lens.Lens' ImportTableDescription (Prelude.Maybe S3BucketSource)
importTableDescription_s3BucketSource = Lens.lens (\ImportTableDescription' {s3BucketSource} -> s3BucketSource) (\s@ImportTableDescription' {} a -> s {s3BucketSource = a} :: ImportTableDescription)

-- | The time when this import task started.
importTableDescription_startTime :: Lens.Lens' ImportTableDescription (Prelude.Maybe Prelude.UTCTime)
importTableDescription_startTime = Lens.lens (\ImportTableDescription' {startTime} -> startTime) (\s@ImportTableDescription' {} a -> s {startTime = a} :: ImportTableDescription) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Number (ARN) of the table being imported into.
importTableDescription_tableArn :: Lens.Lens' ImportTableDescription (Prelude.Maybe Prelude.Text)
importTableDescription_tableArn = Lens.lens (\ImportTableDescription' {tableArn} -> tableArn) (\s@ImportTableDescription' {} a -> s {tableArn = a} :: ImportTableDescription)

-- | The parameters for the new table that is being imported into.
importTableDescription_tableCreationParameters :: Lens.Lens' ImportTableDescription (Prelude.Maybe TableCreationParameters)
importTableDescription_tableCreationParameters = Lens.lens (\ImportTableDescription' {tableCreationParameters} -> tableCreationParameters) (\s@ImportTableDescription' {} a -> s {tableCreationParameters = a} :: ImportTableDescription)

-- | The table id corresponding to the table created by import table process.
importTableDescription_tableId :: Lens.Lens' ImportTableDescription (Prelude.Maybe Prelude.Text)
importTableDescription_tableId = Lens.lens (\ImportTableDescription' {tableId} -> tableId) (\s@ImportTableDescription' {} a -> s {tableId = a} :: ImportTableDescription)

instance Data.FromJSON ImportTableDescription where
  parseJSON =
    Data.withObject
      "ImportTableDescription"
      ( \x ->
          ImportTableDescription'
            Prelude.<$> (x Data..:? "ClientToken")
            Prelude.<*> (x Data..:? "CloudWatchLogGroupArn")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "ErrorCount")
            Prelude.<*> (x Data..:? "FailureCode")
            Prelude.<*> (x Data..:? "FailureMessage")
            Prelude.<*> (x Data..:? "ImportArn")
            Prelude.<*> (x Data..:? "ImportStatus")
            Prelude.<*> (x Data..:? "ImportedItemCount")
            Prelude.<*> (x Data..:? "InputCompressionType")
            Prelude.<*> (x Data..:? "InputFormat")
            Prelude.<*> (x Data..:? "InputFormatOptions")
            Prelude.<*> (x Data..:? "ProcessedItemCount")
            Prelude.<*> (x Data..:? "ProcessedSizeBytes")
            Prelude.<*> (x Data..:? "S3BucketSource")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "TableArn")
            Prelude.<*> (x Data..:? "TableCreationParameters")
            Prelude.<*> (x Data..:? "TableId")
      )

instance Prelude.Hashable ImportTableDescription where
  hashWithSalt _salt ImportTableDescription' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` cloudWatchLogGroupArn
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` errorCount
      `Prelude.hashWithSalt` failureCode
      `Prelude.hashWithSalt` failureMessage
      `Prelude.hashWithSalt` importArn
      `Prelude.hashWithSalt` importStatus
      `Prelude.hashWithSalt` importedItemCount
      `Prelude.hashWithSalt` inputCompressionType
      `Prelude.hashWithSalt` inputFormat
      `Prelude.hashWithSalt` inputFormatOptions
      `Prelude.hashWithSalt` processedItemCount
      `Prelude.hashWithSalt` processedSizeBytes
      `Prelude.hashWithSalt` s3BucketSource
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` tableArn
      `Prelude.hashWithSalt` tableCreationParameters
      `Prelude.hashWithSalt` tableId

instance Prelude.NFData ImportTableDescription where
  rnf ImportTableDescription' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf cloudWatchLogGroupArn
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf errorCount
      `Prelude.seq` Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf failureMessage
      `Prelude.seq` Prelude.rnf importArn
      `Prelude.seq` Prelude.rnf importStatus
      `Prelude.seq` Prelude.rnf importedItemCount
      `Prelude.seq` Prelude.rnf inputCompressionType
      `Prelude.seq` Prelude.rnf inputFormat
      `Prelude.seq` Prelude.rnf inputFormatOptions
      `Prelude.seq` Prelude.rnf processedItemCount
      `Prelude.seq` Prelude.rnf processedSizeBytes
      `Prelude.seq` Prelude.rnf s3BucketSource
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf tableArn
      `Prelude.seq` Prelude.rnf
        tableCreationParameters
      `Prelude.seq` Prelude.rnf tableId
