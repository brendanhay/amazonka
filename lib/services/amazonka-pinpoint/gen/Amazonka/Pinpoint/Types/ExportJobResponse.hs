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
-- Module      : Amazonka.Pinpoint.Types.ExportJobResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.ExportJobResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.ExportJobResource
import Amazonka.Pinpoint.Types.JobStatus
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the status and settings of a job that exports
-- endpoint definitions to a file. The file can be added directly to an
-- Amazon Simple Storage Service (Amazon S3) bucket by using the Amazon
-- Pinpoint API or downloaded directly to a computer by using the Amazon
-- Pinpoint console.
--
-- /See:/ 'newExportJobResponse' smart constructor.
data ExportJobResponse = ExportJobResponse'
  { -- | The number of pieces that weren\'t processed successfully (failed) by
    -- the export job, as of the time of the request.
    failedPieces :: Prelude.Maybe Prelude.Int,
    -- | The number of pieces that were processed successfully (completed) by the
    -- export job, as of the time of the request.
    completedPieces :: Prelude.Maybe Prelude.Int,
    -- | The total number of pieces that must be processed to complete the export
    -- job. Each piece consists of an approximately equal portion of the
    -- endpoint definitions that are part of the export job.
    totalPieces :: Prelude.Maybe Prelude.Int,
    -- | The date, in ISO 8601 format, when the export job was completed.
    completionDate :: Prelude.Maybe Prelude.Text,
    -- | The total number of endpoint definitions that were processed by the
    -- export job.
    totalProcessed :: Prelude.Maybe Prelude.Int,
    -- | An array of entries, one for each of the first 100 entries that weren\'t
    -- processed successfully (failed) by the export job, if any.
    failures :: Prelude.Maybe [Prelude.Text],
    -- | The total number of endpoint definitions that weren\'t processed
    -- successfully (failed) by the export job, typically because an error,
    -- such as a syntax error, occurred.
    totalFailures :: Prelude.Maybe Prelude.Int,
    -- | The status of the export job. The job status is FAILED if Amazon
    -- Pinpoint wasn\'t able to process one or more pieces in the job.
    jobStatus :: JobStatus,
    -- | The date, in ISO 8601 format, when the export job was created.
    creationDate :: Prelude.Text,
    -- | The job type. This value is EXPORT for export jobs.
    type' :: Prelude.Text,
    -- | The resource settings that apply to the export job.
    definition :: ExportJobResource,
    -- | The unique identifier for the export job.
    id :: Prelude.Text,
    -- | The unique identifier for the application that\'s associated with the
    -- export job.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedPieces', 'exportJobResponse_failedPieces' - The number of pieces that weren\'t processed successfully (failed) by
-- the export job, as of the time of the request.
--
-- 'completedPieces', 'exportJobResponse_completedPieces' - The number of pieces that were processed successfully (completed) by the
-- export job, as of the time of the request.
--
-- 'totalPieces', 'exportJobResponse_totalPieces' - The total number of pieces that must be processed to complete the export
-- job. Each piece consists of an approximately equal portion of the
-- endpoint definitions that are part of the export job.
--
-- 'completionDate', 'exportJobResponse_completionDate' - The date, in ISO 8601 format, when the export job was completed.
--
-- 'totalProcessed', 'exportJobResponse_totalProcessed' - The total number of endpoint definitions that were processed by the
-- export job.
--
-- 'failures', 'exportJobResponse_failures' - An array of entries, one for each of the first 100 entries that weren\'t
-- processed successfully (failed) by the export job, if any.
--
-- 'totalFailures', 'exportJobResponse_totalFailures' - The total number of endpoint definitions that weren\'t processed
-- successfully (failed) by the export job, typically because an error,
-- such as a syntax error, occurred.
--
-- 'jobStatus', 'exportJobResponse_jobStatus' - The status of the export job. The job status is FAILED if Amazon
-- Pinpoint wasn\'t able to process one or more pieces in the job.
--
-- 'creationDate', 'exportJobResponse_creationDate' - The date, in ISO 8601 format, when the export job was created.
--
-- 'type'', 'exportJobResponse_type' - The job type. This value is EXPORT for export jobs.
--
-- 'definition', 'exportJobResponse_definition' - The resource settings that apply to the export job.
--
-- 'id', 'exportJobResponse_id' - The unique identifier for the export job.
--
-- 'applicationId', 'exportJobResponse_applicationId' - The unique identifier for the application that\'s associated with the
-- export job.
newExportJobResponse ::
  -- | 'jobStatus'
  JobStatus ->
  -- | 'creationDate'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  -- | 'definition'
  ExportJobResource ->
  -- | 'id'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  ExportJobResponse
newExportJobResponse
  pJobStatus_
  pCreationDate_
  pType_
  pDefinition_
  pId_
  pApplicationId_ =
    ExportJobResponse'
      { failedPieces = Prelude.Nothing,
        completedPieces = Prelude.Nothing,
        totalPieces = Prelude.Nothing,
        completionDate = Prelude.Nothing,
        totalProcessed = Prelude.Nothing,
        failures = Prelude.Nothing,
        totalFailures = Prelude.Nothing,
        jobStatus = pJobStatus_,
        creationDate = pCreationDate_,
        type' = pType_,
        definition = pDefinition_,
        id = pId_,
        applicationId = pApplicationId_
      }

-- | The number of pieces that weren\'t processed successfully (failed) by
-- the export job, as of the time of the request.
exportJobResponse_failedPieces :: Lens.Lens' ExportJobResponse (Prelude.Maybe Prelude.Int)
exportJobResponse_failedPieces = Lens.lens (\ExportJobResponse' {failedPieces} -> failedPieces) (\s@ExportJobResponse' {} a -> s {failedPieces = a} :: ExportJobResponse)

-- | The number of pieces that were processed successfully (completed) by the
-- export job, as of the time of the request.
exportJobResponse_completedPieces :: Lens.Lens' ExportJobResponse (Prelude.Maybe Prelude.Int)
exportJobResponse_completedPieces = Lens.lens (\ExportJobResponse' {completedPieces} -> completedPieces) (\s@ExportJobResponse' {} a -> s {completedPieces = a} :: ExportJobResponse)

-- | The total number of pieces that must be processed to complete the export
-- job. Each piece consists of an approximately equal portion of the
-- endpoint definitions that are part of the export job.
exportJobResponse_totalPieces :: Lens.Lens' ExportJobResponse (Prelude.Maybe Prelude.Int)
exportJobResponse_totalPieces = Lens.lens (\ExportJobResponse' {totalPieces} -> totalPieces) (\s@ExportJobResponse' {} a -> s {totalPieces = a} :: ExportJobResponse)

-- | The date, in ISO 8601 format, when the export job was completed.
exportJobResponse_completionDate :: Lens.Lens' ExportJobResponse (Prelude.Maybe Prelude.Text)
exportJobResponse_completionDate = Lens.lens (\ExportJobResponse' {completionDate} -> completionDate) (\s@ExportJobResponse' {} a -> s {completionDate = a} :: ExportJobResponse)

-- | The total number of endpoint definitions that were processed by the
-- export job.
exportJobResponse_totalProcessed :: Lens.Lens' ExportJobResponse (Prelude.Maybe Prelude.Int)
exportJobResponse_totalProcessed = Lens.lens (\ExportJobResponse' {totalProcessed} -> totalProcessed) (\s@ExportJobResponse' {} a -> s {totalProcessed = a} :: ExportJobResponse)

-- | An array of entries, one for each of the first 100 entries that weren\'t
-- processed successfully (failed) by the export job, if any.
exportJobResponse_failures :: Lens.Lens' ExportJobResponse (Prelude.Maybe [Prelude.Text])
exportJobResponse_failures = Lens.lens (\ExportJobResponse' {failures} -> failures) (\s@ExportJobResponse' {} a -> s {failures = a} :: ExportJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The total number of endpoint definitions that weren\'t processed
-- successfully (failed) by the export job, typically because an error,
-- such as a syntax error, occurred.
exportJobResponse_totalFailures :: Lens.Lens' ExportJobResponse (Prelude.Maybe Prelude.Int)
exportJobResponse_totalFailures = Lens.lens (\ExportJobResponse' {totalFailures} -> totalFailures) (\s@ExportJobResponse' {} a -> s {totalFailures = a} :: ExportJobResponse)

-- | The status of the export job. The job status is FAILED if Amazon
-- Pinpoint wasn\'t able to process one or more pieces in the job.
exportJobResponse_jobStatus :: Lens.Lens' ExportJobResponse JobStatus
exportJobResponse_jobStatus = Lens.lens (\ExportJobResponse' {jobStatus} -> jobStatus) (\s@ExportJobResponse' {} a -> s {jobStatus = a} :: ExportJobResponse)

-- | The date, in ISO 8601 format, when the export job was created.
exportJobResponse_creationDate :: Lens.Lens' ExportJobResponse Prelude.Text
exportJobResponse_creationDate = Lens.lens (\ExportJobResponse' {creationDate} -> creationDate) (\s@ExportJobResponse' {} a -> s {creationDate = a} :: ExportJobResponse)

-- | The job type. This value is EXPORT for export jobs.
exportJobResponse_type :: Lens.Lens' ExportJobResponse Prelude.Text
exportJobResponse_type = Lens.lens (\ExportJobResponse' {type'} -> type') (\s@ExportJobResponse' {} a -> s {type' = a} :: ExportJobResponse)

-- | The resource settings that apply to the export job.
exportJobResponse_definition :: Lens.Lens' ExportJobResponse ExportJobResource
exportJobResponse_definition = Lens.lens (\ExportJobResponse' {definition} -> definition) (\s@ExportJobResponse' {} a -> s {definition = a} :: ExportJobResponse)

-- | The unique identifier for the export job.
exportJobResponse_id :: Lens.Lens' ExportJobResponse Prelude.Text
exportJobResponse_id = Lens.lens (\ExportJobResponse' {id} -> id) (\s@ExportJobResponse' {} a -> s {id = a} :: ExportJobResponse)

-- | The unique identifier for the application that\'s associated with the
-- export job.
exportJobResponse_applicationId :: Lens.Lens' ExportJobResponse Prelude.Text
exportJobResponse_applicationId = Lens.lens (\ExportJobResponse' {applicationId} -> applicationId) (\s@ExportJobResponse' {} a -> s {applicationId = a} :: ExportJobResponse)

instance Core.FromJSON ExportJobResponse where
  parseJSON =
    Core.withObject
      "ExportJobResponse"
      ( \x ->
          ExportJobResponse'
            Prelude.<$> (x Core..:? "FailedPieces")
            Prelude.<*> (x Core..:? "CompletedPieces")
            Prelude.<*> (x Core..:? "TotalPieces")
            Prelude.<*> (x Core..:? "CompletionDate")
            Prelude.<*> (x Core..:? "TotalProcessed")
            Prelude.<*> (x Core..:? "Failures" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "TotalFailures")
            Prelude.<*> (x Core..: "JobStatus")
            Prelude.<*> (x Core..: "CreationDate")
            Prelude.<*> (x Core..: "Type")
            Prelude.<*> (x Core..: "Definition")
            Prelude.<*> (x Core..: "Id")
            Prelude.<*> (x Core..: "ApplicationId")
      )

instance Prelude.Hashable ExportJobResponse where
  hashWithSalt _salt ExportJobResponse' {..} =
    _salt `Prelude.hashWithSalt` failedPieces
      `Prelude.hashWithSalt` completedPieces
      `Prelude.hashWithSalt` totalPieces
      `Prelude.hashWithSalt` completionDate
      `Prelude.hashWithSalt` totalProcessed
      `Prelude.hashWithSalt` failures
      `Prelude.hashWithSalt` totalFailures
      `Prelude.hashWithSalt` jobStatus
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` definition
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData ExportJobResponse where
  rnf ExportJobResponse' {..} =
    Prelude.rnf failedPieces
      `Prelude.seq` Prelude.rnf completedPieces
      `Prelude.seq` Prelude.rnf totalPieces
      `Prelude.seq` Prelude.rnf completionDate
      `Prelude.seq` Prelude.rnf totalProcessed
      `Prelude.seq` Prelude.rnf failures
      `Prelude.seq` Prelude.rnf totalFailures
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf applicationId
