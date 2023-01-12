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
-- Module      : Amazonka.DynamoDB.Types.ImportSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ImportSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.ImportStatus
import Amazonka.DynamoDB.Types.InputFormat
import Amazonka.DynamoDB.Types.S3BucketSource
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Summary information about the source file for the import.
--
-- /See:/ 'newImportSummary' smart constructor.
data ImportSummary = ImportSummary'
  { -- | The Amazon Resource Number (ARN) of the Cloudwatch Log Group associated
    -- with this import task.
    cloudWatchLogGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which this import task ended. (Does this include the
    -- successful complete creation of the table it was imported to?)
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Number (ARN) corresponding to the import request.
    importArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the import operation.
    importStatus :: Prelude.Maybe ImportStatus,
    -- | The format of the source data. Valid values are @CSV@, @DYNAMODB_JSON@
    -- or @ION@.
    inputFormat :: Prelude.Maybe InputFormat,
    -- | The path and S3 bucket of the source file that is being imported. This
    -- includes the S3Bucket (required), S3KeyPrefix (optional) and
    -- S3BucketOwner (optional if the bucket is owned by the requester).
    s3BucketSource :: Prelude.Maybe S3BucketSource,
    -- | The time at which this import task began.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Number (ARN) of the table being imported into.
    tableArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogGroupArn', 'importSummary_cloudWatchLogGroupArn' - The Amazon Resource Number (ARN) of the Cloudwatch Log Group associated
-- with this import task.
--
-- 'endTime', 'importSummary_endTime' - The time at which this import task ended. (Does this include the
-- successful complete creation of the table it was imported to?)
--
-- 'importArn', 'importSummary_importArn' - The Amazon Resource Number (ARN) corresponding to the import request.
--
-- 'importStatus', 'importSummary_importStatus' - The status of the import operation.
--
-- 'inputFormat', 'importSummary_inputFormat' - The format of the source data. Valid values are @CSV@, @DYNAMODB_JSON@
-- or @ION@.
--
-- 's3BucketSource', 'importSummary_s3BucketSource' - The path and S3 bucket of the source file that is being imported. This
-- includes the S3Bucket (required), S3KeyPrefix (optional) and
-- S3BucketOwner (optional if the bucket is owned by the requester).
--
-- 'startTime', 'importSummary_startTime' - The time at which this import task began.
--
-- 'tableArn', 'importSummary_tableArn' - The Amazon Resource Number (ARN) of the table being imported into.
newImportSummary ::
  ImportSummary
newImportSummary =
  ImportSummary'
    { cloudWatchLogGroupArn =
        Prelude.Nothing,
      endTime = Prelude.Nothing,
      importArn = Prelude.Nothing,
      importStatus = Prelude.Nothing,
      inputFormat = Prelude.Nothing,
      s3BucketSource = Prelude.Nothing,
      startTime = Prelude.Nothing,
      tableArn = Prelude.Nothing
    }

-- | The Amazon Resource Number (ARN) of the Cloudwatch Log Group associated
-- with this import task.
importSummary_cloudWatchLogGroupArn :: Lens.Lens' ImportSummary (Prelude.Maybe Prelude.Text)
importSummary_cloudWatchLogGroupArn = Lens.lens (\ImportSummary' {cloudWatchLogGroupArn} -> cloudWatchLogGroupArn) (\s@ImportSummary' {} a -> s {cloudWatchLogGroupArn = a} :: ImportSummary)

-- | The time at which this import task ended. (Does this include the
-- successful complete creation of the table it was imported to?)
importSummary_endTime :: Lens.Lens' ImportSummary (Prelude.Maybe Prelude.UTCTime)
importSummary_endTime = Lens.lens (\ImportSummary' {endTime} -> endTime) (\s@ImportSummary' {} a -> s {endTime = a} :: ImportSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Number (ARN) corresponding to the import request.
importSummary_importArn :: Lens.Lens' ImportSummary (Prelude.Maybe Prelude.Text)
importSummary_importArn = Lens.lens (\ImportSummary' {importArn} -> importArn) (\s@ImportSummary' {} a -> s {importArn = a} :: ImportSummary)

-- | The status of the import operation.
importSummary_importStatus :: Lens.Lens' ImportSummary (Prelude.Maybe ImportStatus)
importSummary_importStatus = Lens.lens (\ImportSummary' {importStatus} -> importStatus) (\s@ImportSummary' {} a -> s {importStatus = a} :: ImportSummary)

-- | The format of the source data. Valid values are @CSV@, @DYNAMODB_JSON@
-- or @ION@.
importSummary_inputFormat :: Lens.Lens' ImportSummary (Prelude.Maybe InputFormat)
importSummary_inputFormat = Lens.lens (\ImportSummary' {inputFormat} -> inputFormat) (\s@ImportSummary' {} a -> s {inputFormat = a} :: ImportSummary)

-- | The path and S3 bucket of the source file that is being imported. This
-- includes the S3Bucket (required), S3KeyPrefix (optional) and
-- S3BucketOwner (optional if the bucket is owned by the requester).
importSummary_s3BucketSource :: Lens.Lens' ImportSummary (Prelude.Maybe S3BucketSource)
importSummary_s3BucketSource = Lens.lens (\ImportSummary' {s3BucketSource} -> s3BucketSource) (\s@ImportSummary' {} a -> s {s3BucketSource = a} :: ImportSummary)

-- | The time at which this import task began.
importSummary_startTime :: Lens.Lens' ImportSummary (Prelude.Maybe Prelude.UTCTime)
importSummary_startTime = Lens.lens (\ImportSummary' {startTime} -> startTime) (\s@ImportSummary' {} a -> s {startTime = a} :: ImportSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Number (ARN) of the table being imported into.
importSummary_tableArn :: Lens.Lens' ImportSummary (Prelude.Maybe Prelude.Text)
importSummary_tableArn = Lens.lens (\ImportSummary' {tableArn} -> tableArn) (\s@ImportSummary' {} a -> s {tableArn = a} :: ImportSummary)

instance Data.FromJSON ImportSummary where
  parseJSON =
    Data.withObject
      "ImportSummary"
      ( \x ->
          ImportSummary'
            Prelude.<$> (x Data..:? "CloudWatchLogGroupArn")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "ImportArn")
            Prelude.<*> (x Data..:? "ImportStatus")
            Prelude.<*> (x Data..:? "InputFormat")
            Prelude.<*> (x Data..:? "S3BucketSource")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "TableArn")
      )

instance Prelude.Hashable ImportSummary where
  hashWithSalt _salt ImportSummary' {..} =
    _salt `Prelude.hashWithSalt` cloudWatchLogGroupArn
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` importArn
      `Prelude.hashWithSalt` importStatus
      `Prelude.hashWithSalt` inputFormat
      `Prelude.hashWithSalt` s3BucketSource
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` tableArn

instance Prelude.NFData ImportSummary where
  rnf ImportSummary' {..} =
    Prelude.rnf cloudWatchLogGroupArn
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf importArn
      `Prelude.seq` Prelude.rnf importStatus
      `Prelude.seq` Prelude.rnf inputFormat
      `Prelude.seq` Prelude.rnf s3BucketSource
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf tableArn
