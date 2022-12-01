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
-- Module      : Amazonka.MigrationHubStrategy.Types.ImportFileTaskInformation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.ImportFileTaskInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubStrategy.Types.ImportFileTaskStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about the import file tasks you request.
--
-- /See:/ 'newImportFileTaskInformation' smart constructor.
data ImportFileTaskInformation = ImportFileTaskInformation'
  { -- | The S3 bucket name for status report of import task.
    statusReportS3Bucket :: Prelude.Maybe Prelude.Text,
    -- | Status of import file task.
    status :: Prelude.Maybe ImportFileTaskStatus,
    -- | The time that the import task completes.
    completionTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of the import file task.
    id :: Prelude.Maybe Prelude.Text,
    -- | The number of records that failed to be imported.
    numberOfRecordsFailed :: Prelude.Maybe Prelude.Int,
    -- | The name of the import task given in @StartImportFileTask@.
    importName :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket where the import file is located.
    inputS3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 key name of the import file.
    inputS3Key :: Prelude.Maybe Prelude.Text,
    -- | The number of records successfully imported.
    numberOfRecordsSuccess :: Prelude.Maybe Prelude.Int,
    -- | Start time of the import task.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon S3 key name for status report of import task. The report
    -- contains details about whether each record imported successfully or why
    -- it did not.
    statusReportS3Key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportFileTaskInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusReportS3Bucket', 'importFileTaskInformation_statusReportS3Bucket' - The S3 bucket name for status report of import task.
--
-- 'status', 'importFileTaskInformation_status' - Status of import file task.
--
-- 'completionTime', 'importFileTaskInformation_completionTime' - The time that the import task completes.
--
-- 'id', 'importFileTaskInformation_id' - The ID of the import file task.
--
-- 'numberOfRecordsFailed', 'importFileTaskInformation_numberOfRecordsFailed' - The number of records that failed to be imported.
--
-- 'importName', 'importFileTaskInformation_importName' - The name of the import task given in @StartImportFileTask@.
--
-- 'inputS3Bucket', 'importFileTaskInformation_inputS3Bucket' - The S3 bucket where the import file is located.
--
-- 'inputS3Key', 'importFileTaskInformation_inputS3Key' - The Amazon S3 key name of the import file.
--
-- 'numberOfRecordsSuccess', 'importFileTaskInformation_numberOfRecordsSuccess' - The number of records successfully imported.
--
-- 'startTime', 'importFileTaskInformation_startTime' - Start time of the import task.
--
-- 'statusReportS3Key', 'importFileTaskInformation_statusReportS3Key' - The Amazon S3 key name for status report of import task. The report
-- contains details about whether each record imported successfully or why
-- it did not.
newImportFileTaskInformation ::
  ImportFileTaskInformation
newImportFileTaskInformation =
  ImportFileTaskInformation'
    { statusReportS3Bucket =
        Prelude.Nothing,
      status = Prelude.Nothing,
      completionTime = Prelude.Nothing,
      id = Prelude.Nothing,
      numberOfRecordsFailed = Prelude.Nothing,
      importName = Prelude.Nothing,
      inputS3Bucket = Prelude.Nothing,
      inputS3Key = Prelude.Nothing,
      numberOfRecordsSuccess = Prelude.Nothing,
      startTime = Prelude.Nothing,
      statusReportS3Key = Prelude.Nothing
    }

-- | The S3 bucket name for status report of import task.
importFileTaskInformation_statusReportS3Bucket :: Lens.Lens' ImportFileTaskInformation (Prelude.Maybe Prelude.Text)
importFileTaskInformation_statusReportS3Bucket = Lens.lens (\ImportFileTaskInformation' {statusReportS3Bucket} -> statusReportS3Bucket) (\s@ImportFileTaskInformation' {} a -> s {statusReportS3Bucket = a} :: ImportFileTaskInformation)

-- | Status of import file task.
importFileTaskInformation_status :: Lens.Lens' ImportFileTaskInformation (Prelude.Maybe ImportFileTaskStatus)
importFileTaskInformation_status = Lens.lens (\ImportFileTaskInformation' {status} -> status) (\s@ImportFileTaskInformation' {} a -> s {status = a} :: ImportFileTaskInformation)

-- | The time that the import task completes.
importFileTaskInformation_completionTime :: Lens.Lens' ImportFileTaskInformation (Prelude.Maybe Prelude.UTCTime)
importFileTaskInformation_completionTime = Lens.lens (\ImportFileTaskInformation' {completionTime} -> completionTime) (\s@ImportFileTaskInformation' {} a -> s {completionTime = a} :: ImportFileTaskInformation) Prelude.. Lens.mapping Core._Time

-- | The ID of the import file task.
importFileTaskInformation_id :: Lens.Lens' ImportFileTaskInformation (Prelude.Maybe Prelude.Text)
importFileTaskInformation_id = Lens.lens (\ImportFileTaskInformation' {id} -> id) (\s@ImportFileTaskInformation' {} a -> s {id = a} :: ImportFileTaskInformation)

-- | The number of records that failed to be imported.
importFileTaskInformation_numberOfRecordsFailed :: Lens.Lens' ImportFileTaskInformation (Prelude.Maybe Prelude.Int)
importFileTaskInformation_numberOfRecordsFailed = Lens.lens (\ImportFileTaskInformation' {numberOfRecordsFailed} -> numberOfRecordsFailed) (\s@ImportFileTaskInformation' {} a -> s {numberOfRecordsFailed = a} :: ImportFileTaskInformation)

-- | The name of the import task given in @StartImportFileTask@.
importFileTaskInformation_importName :: Lens.Lens' ImportFileTaskInformation (Prelude.Maybe Prelude.Text)
importFileTaskInformation_importName = Lens.lens (\ImportFileTaskInformation' {importName} -> importName) (\s@ImportFileTaskInformation' {} a -> s {importName = a} :: ImportFileTaskInformation)

-- | The S3 bucket where the import file is located.
importFileTaskInformation_inputS3Bucket :: Lens.Lens' ImportFileTaskInformation (Prelude.Maybe Prelude.Text)
importFileTaskInformation_inputS3Bucket = Lens.lens (\ImportFileTaskInformation' {inputS3Bucket} -> inputS3Bucket) (\s@ImportFileTaskInformation' {} a -> s {inputS3Bucket = a} :: ImportFileTaskInformation)

-- | The Amazon S3 key name of the import file.
importFileTaskInformation_inputS3Key :: Lens.Lens' ImportFileTaskInformation (Prelude.Maybe Prelude.Text)
importFileTaskInformation_inputS3Key = Lens.lens (\ImportFileTaskInformation' {inputS3Key} -> inputS3Key) (\s@ImportFileTaskInformation' {} a -> s {inputS3Key = a} :: ImportFileTaskInformation)

-- | The number of records successfully imported.
importFileTaskInformation_numberOfRecordsSuccess :: Lens.Lens' ImportFileTaskInformation (Prelude.Maybe Prelude.Int)
importFileTaskInformation_numberOfRecordsSuccess = Lens.lens (\ImportFileTaskInformation' {numberOfRecordsSuccess} -> numberOfRecordsSuccess) (\s@ImportFileTaskInformation' {} a -> s {numberOfRecordsSuccess = a} :: ImportFileTaskInformation)

-- | Start time of the import task.
importFileTaskInformation_startTime :: Lens.Lens' ImportFileTaskInformation (Prelude.Maybe Prelude.UTCTime)
importFileTaskInformation_startTime = Lens.lens (\ImportFileTaskInformation' {startTime} -> startTime) (\s@ImportFileTaskInformation' {} a -> s {startTime = a} :: ImportFileTaskInformation) Prelude.. Lens.mapping Core._Time

-- | The Amazon S3 key name for status report of import task. The report
-- contains details about whether each record imported successfully or why
-- it did not.
importFileTaskInformation_statusReportS3Key :: Lens.Lens' ImportFileTaskInformation (Prelude.Maybe Prelude.Text)
importFileTaskInformation_statusReportS3Key = Lens.lens (\ImportFileTaskInformation' {statusReportS3Key} -> statusReportS3Key) (\s@ImportFileTaskInformation' {} a -> s {statusReportS3Key = a} :: ImportFileTaskInformation)

instance Core.FromJSON ImportFileTaskInformation where
  parseJSON =
    Core.withObject
      "ImportFileTaskInformation"
      ( \x ->
          ImportFileTaskInformation'
            Prelude.<$> (x Core..:? "statusReportS3Bucket")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "completionTime")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "numberOfRecordsFailed")
            Prelude.<*> (x Core..:? "importName")
            Prelude.<*> (x Core..:? "inputS3Bucket")
            Prelude.<*> (x Core..:? "inputS3Key")
            Prelude.<*> (x Core..:? "numberOfRecordsSuccess")
            Prelude.<*> (x Core..:? "startTime")
            Prelude.<*> (x Core..:? "statusReportS3Key")
      )

instance Prelude.Hashable ImportFileTaskInformation where
  hashWithSalt _salt ImportFileTaskInformation' {..} =
    _salt `Prelude.hashWithSalt` statusReportS3Bucket
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` numberOfRecordsFailed
      `Prelude.hashWithSalt` importName
      `Prelude.hashWithSalt` inputS3Bucket
      `Prelude.hashWithSalt` inputS3Key
      `Prelude.hashWithSalt` numberOfRecordsSuccess
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` statusReportS3Key

instance Prelude.NFData ImportFileTaskInformation where
  rnf ImportFileTaskInformation' {..} =
    Prelude.rnf statusReportS3Bucket
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf numberOfRecordsFailed
      `Prelude.seq` Prelude.rnf importName
      `Prelude.seq` Prelude.rnf inputS3Bucket
      `Prelude.seq` Prelude.rnf inputS3Key
      `Prelude.seq` Prelude.rnf numberOfRecordsSuccess
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf statusReportS3Key
