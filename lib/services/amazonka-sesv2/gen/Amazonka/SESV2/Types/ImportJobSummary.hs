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
-- Module      : Amazonka.SESV2.Types.ImportJobSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.ImportJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.ImportDestination
import Amazonka.SESV2.Types.JobStatus

-- | A summary of the import job.
--
-- /See:/ 'newImportJobSummary' smart constructor.
data ImportJobSummary = ImportJobSummary'
  { -- | The date and time when the import job was created.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The number of records that failed processing because of invalid input or
    -- other reasons.
    failedRecordsCount :: Prelude.Maybe Prelude.Int,
    importDestination :: Prelude.Maybe ImportDestination,
    jobId :: Prelude.Maybe Prelude.Text,
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The current number of records processed.
    processedRecordsCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'importJobSummary_createdTimestamp' - The date and time when the import job was created.
--
-- 'failedRecordsCount', 'importJobSummary_failedRecordsCount' - The number of records that failed processing because of invalid input or
-- other reasons.
--
-- 'importDestination', 'importJobSummary_importDestination' - Undocumented member.
--
-- 'jobId', 'importJobSummary_jobId' - Undocumented member.
--
-- 'jobStatus', 'importJobSummary_jobStatus' - Undocumented member.
--
-- 'processedRecordsCount', 'importJobSummary_processedRecordsCount' - The current number of records processed.
newImportJobSummary ::
  ImportJobSummary
newImportJobSummary =
  ImportJobSummary'
    { createdTimestamp =
        Prelude.Nothing,
      failedRecordsCount = Prelude.Nothing,
      importDestination = Prelude.Nothing,
      jobId = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      processedRecordsCount = Prelude.Nothing
    }

-- | The date and time when the import job was created.
importJobSummary_createdTimestamp :: Lens.Lens' ImportJobSummary (Prelude.Maybe Prelude.UTCTime)
importJobSummary_createdTimestamp = Lens.lens (\ImportJobSummary' {createdTimestamp} -> createdTimestamp) (\s@ImportJobSummary' {} a -> s {createdTimestamp = a} :: ImportJobSummary) Prelude.. Lens.mapping Data._Time

-- | The number of records that failed processing because of invalid input or
-- other reasons.
importJobSummary_failedRecordsCount :: Lens.Lens' ImportJobSummary (Prelude.Maybe Prelude.Int)
importJobSummary_failedRecordsCount = Lens.lens (\ImportJobSummary' {failedRecordsCount} -> failedRecordsCount) (\s@ImportJobSummary' {} a -> s {failedRecordsCount = a} :: ImportJobSummary)

-- | Undocumented member.
importJobSummary_importDestination :: Lens.Lens' ImportJobSummary (Prelude.Maybe ImportDestination)
importJobSummary_importDestination = Lens.lens (\ImportJobSummary' {importDestination} -> importDestination) (\s@ImportJobSummary' {} a -> s {importDestination = a} :: ImportJobSummary)

-- | Undocumented member.
importJobSummary_jobId :: Lens.Lens' ImportJobSummary (Prelude.Maybe Prelude.Text)
importJobSummary_jobId = Lens.lens (\ImportJobSummary' {jobId} -> jobId) (\s@ImportJobSummary' {} a -> s {jobId = a} :: ImportJobSummary)

-- | Undocumented member.
importJobSummary_jobStatus :: Lens.Lens' ImportJobSummary (Prelude.Maybe JobStatus)
importJobSummary_jobStatus = Lens.lens (\ImportJobSummary' {jobStatus} -> jobStatus) (\s@ImportJobSummary' {} a -> s {jobStatus = a} :: ImportJobSummary)

-- | The current number of records processed.
importJobSummary_processedRecordsCount :: Lens.Lens' ImportJobSummary (Prelude.Maybe Prelude.Int)
importJobSummary_processedRecordsCount = Lens.lens (\ImportJobSummary' {processedRecordsCount} -> processedRecordsCount) (\s@ImportJobSummary' {} a -> s {processedRecordsCount = a} :: ImportJobSummary)

instance Data.FromJSON ImportJobSummary where
  parseJSON =
    Data.withObject
      "ImportJobSummary"
      ( \x ->
          ImportJobSummary'
            Prelude.<$> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "FailedRecordsCount")
            Prelude.<*> (x Data..:? "ImportDestination")
            Prelude.<*> (x Data..:? "JobId")
            Prelude.<*> (x Data..:? "JobStatus")
            Prelude.<*> (x Data..:? "ProcessedRecordsCount")
      )

instance Prelude.Hashable ImportJobSummary where
  hashWithSalt _salt ImportJobSummary' {..} =
    _salt
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` failedRecordsCount
      `Prelude.hashWithSalt` importDestination
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` jobStatus
      `Prelude.hashWithSalt` processedRecordsCount

instance Prelude.NFData ImportJobSummary where
  rnf ImportJobSummary' {..} =
    Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf failedRecordsCount
      `Prelude.seq` Prelude.rnf importDestination
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf processedRecordsCount
