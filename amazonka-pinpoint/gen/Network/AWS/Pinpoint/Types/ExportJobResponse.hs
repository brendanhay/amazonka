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
-- Module      : Network.AWS.Pinpoint.Types.ExportJobResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ExportJobResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ExportJobResource
import Network.AWS.Pinpoint.Types.JobStatus

-- | Provides information about the status and settings of a job that exports
-- endpoint definitions to a file. The file can be added directly to an
-- Amazon Simple Storage Service (Amazon S3) bucket by using the Amazon
-- Pinpoint API or downloaded directly to a computer by using the Amazon
-- Pinpoint console.
--
-- /See:/ 'newExportJobResponse' smart constructor.
data ExportJobResponse = ExportJobResponse'
  { -- | The total number of endpoint definitions that weren\'t processed
    -- successfully (failed) by the export job, typically because an error,
    -- such as a syntax error, occurred.
    totalFailures :: Core.Maybe Core.Int,
    -- | An array of entries, one for each of the first 100 entries that weren\'t
    -- processed successfully (failed) by the export job, if any.
    failures :: Core.Maybe [Core.Text],
    -- | The total number of endpoint definitions that were processed by the
    -- export job.
    totalProcessed :: Core.Maybe Core.Int,
    -- | The number of pieces that weren\'t processed successfully (failed) by
    -- the export job, as of the time of the request.
    failedPieces :: Core.Maybe Core.Int,
    -- | The number of pieces that were processed successfully (completed) by the
    -- export job, as of the time of the request.
    completedPieces :: Core.Maybe Core.Int,
    -- | The total number of pieces that must be processed to complete the export
    -- job. Each piece consists of an approximately equal portion of the
    -- endpoint definitions that are part of the export job.
    totalPieces :: Core.Maybe Core.Int,
    -- | The date, in ISO 8601 format, when the export job was completed.
    completionDate :: Core.Maybe Core.Text,
    -- | The status of the export job. The job status is FAILED if Amazon
    -- Pinpoint wasn\'t able to process one or more pieces in the job.
    jobStatus :: JobStatus,
    -- | The date, in ISO 8601 format, when the export job was created.
    creationDate :: Core.Text,
    -- | The job type. This value is EXPORT for export jobs.
    type' :: Core.Text,
    -- | The resource settings that apply to the export job.
    definition :: ExportJobResource,
    -- | The unique identifier for the export job.
    id :: Core.Text,
    -- | The unique identifier for the application that\'s associated with the
    -- export job.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalFailures', 'exportJobResponse_totalFailures' - The total number of endpoint definitions that weren\'t processed
-- successfully (failed) by the export job, typically because an error,
-- such as a syntax error, occurred.
--
-- 'failures', 'exportJobResponse_failures' - An array of entries, one for each of the first 100 entries that weren\'t
-- processed successfully (failed) by the export job, if any.
--
-- 'totalProcessed', 'exportJobResponse_totalProcessed' - The total number of endpoint definitions that were processed by the
-- export job.
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
  Core.Text ->
  -- | 'type''
  Core.Text ->
  -- | 'definition'
  ExportJobResource ->
  -- | 'id'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  ExportJobResponse
newExportJobResponse
  pJobStatus_
  pCreationDate_
  pType_
  pDefinition_
  pId_
  pApplicationId_ =
    ExportJobResponse'
      { totalFailures = Core.Nothing,
        failures = Core.Nothing,
        totalProcessed = Core.Nothing,
        failedPieces = Core.Nothing,
        completedPieces = Core.Nothing,
        totalPieces = Core.Nothing,
        completionDate = Core.Nothing,
        jobStatus = pJobStatus_,
        creationDate = pCreationDate_,
        type' = pType_,
        definition = pDefinition_,
        id = pId_,
        applicationId = pApplicationId_
      }

-- | The total number of endpoint definitions that weren\'t processed
-- successfully (failed) by the export job, typically because an error,
-- such as a syntax error, occurred.
exportJobResponse_totalFailures :: Lens.Lens' ExportJobResponse (Core.Maybe Core.Int)
exportJobResponse_totalFailures = Lens.lens (\ExportJobResponse' {totalFailures} -> totalFailures) (\s@ExportJobResponse' {} a -> s {totalFailures = a} :: ExportJobResponse)

-- | An array of entries, one for each of the first 100 entries that weren\'t
-- processed successfully (failed) by the export job, if any.
exportJobResponse_failures :: Lens.Lens' ExportJobResponse (Core.Maybe [Core.Text])
exportJobResponse_failures = Lens.lens (\ExportJobResponse' {failures} -> failures) (\s@ExportJobResponse' {} a -> s {failures = a} :: ExportJobResponse) Core.. Lens.mapping Lens._Coerce

-- | The total number of endpoint definitions that were processed by the
-- export job.
exportJobResponse_totalProcessed :: Lens.Lens' ExportJobResponse (Core.Maybe Core.Int)
exportJobResponse_totalProcessed = Lens.lens (\ExportJobResponse' {totalProcessed} -> totalProcessed) (\s@ExportJobResponse' {} a -> s {totalProcessed = a} :: ExportJobResponse)

-- | The number of pieces that weren\'t processed successfully (failed) by
-- the export job, as of the time of the request.
exportJobResponse_failedPieces :: Lens.Lens' ExportJobResponse (Core.Maybe Core.Int)
exportJobResponse_failedPieces = Lens.lens (\ExportJobResponse' {failedPieces} -> failedPieces) (\s@ExportJobResponse' {} a -> s {failedPieces = a} :: ExportJobResponse)

-- | The number of pieces that were processed successfully (completed) by the
-- export job, as of the time of the request.
exportJobResponse_completedPieces :: Lens.Lens' ExportJobResponse (Core.Maybe Core.Int)
exportJobResponse_completedPieces = Lens.lens (\ExportJobResponse' {completedPieces} -> completedPieces) (\s@ExportJobResponse' {} a -> s {completedPieces = a} :: ExportJobResponse)

-- | The total number of pieces that must be processed to complete the export
-- job. Each piece consists of an approximately equal portion of the
-- endpoint definitions that are part of the export job.
exportJobResponse_totalPieces :: Lens.Lens' ExportJobResponse (Core.Maybe Core.Int)
exportJobResponse_totalPieces = Lens.lens (\ExportJobResponse' {totalPieces} -> totalPieces) (\s@ExportJobResponse' {} a -> s {totalPieces = a} :: ExportJobResponse)

-- | The date, in ISO 8601 format, when the export job was completed.
exportJobResponse_completionDate :: Lens.Lens' ExportJobResponse (Core.Maybe Core.Text)
exportJobResponse_completionDate = Lens.lens (\ExportJobResponse' {completionDate} -> completionDate) (\s@ExportJobResponse' {} a -> s {completionDate = a} :: ExportJobResponse)

-- | The status of the export job. The job status is FAILED if Amazon
-- Pinpoint wasn\'t able to process one or more pieces in the job.
exportJobResponse_jobStatus :: Lens.Lens' ExportJobResponse JobStatus
exportJobResponse_jobStatus = Lens.lens (\ExportJobResponse' {jobStatus} -> jobStatus) (\s@ExportJobResponse' {} a -> s {jobStatus = a} :: ExportJobResponse)

-- | The date, in ISO 8601 format, when the export job was created.
exportJobResponse_creationDate :: Lens.Lens' ExportJobResponse Core.Text
exportJobResponse_creationDate = Lens.lens (\ExportJobResponse' {creationDate} -> creationDate) (\s@ExportJobResponse' {} a -> s {creationDate = a} :: ExportJobResponse)

-- | The job type. This value is EXPORT for export jobs.
exportJobResponse_type :: Lens.Lens' ExportJobResponse Core.Text
exportJobResponse_type = Lens.lens (\ExportJobResponse' {type'} -> type') (\s@ExportJobResponse' {} a -> s {type' = a} :: ExportJobResponse)

-- | The resource settings that apply to the export job.
exportJobResponse_definition :: Lens.Lens' ExportJobResponse ExportJobResource
exportJobResponse_definition = Lens.lens (\ExportJobResponse' {definition} -> definition) (\s@ExportJobResponse' {} a -> s {definition = a} :: ExportJobResponse)

-- | The unique identifier for the export job.
exportJobResponse_id :: Lens.Lens' ExportJobResponse Core.Text
exportJobResponse_id = Lens.lens (\ExportJobResponse' {id} -> id) (\s@ExportJobResponse' {} a -> s {id = a} :: ExportJobResponse)

-- | The unique identifier for the application that\'s associated with the
-- export job.
exportJobResponse_applicationId :: Lens.Lens' ExportJobResponse Core.Text
exportJobResponse_applicationId = Lens.lens (\ExportJobResponse' {applicationId} -> applicationId) (\s@ExportJobResponse' {} a -> s {applicationId = a} :: ExportJobResponse)

instance Core.FromJSON ExportJobResponse where
  parseJSON =
    Core.withObject
      "ExportJobResponse"
      ( \x ->
          ExportJobResponse'
            Core.<$> (x Core..:? "TotalFailures")
            Core.<*> (x Core..:? "Failures" Core..!= Core.mempty)
            Core.<*> (x Core..:? "TotalProcessed")
            Core.<*> (x Core..:? "FailedPieces")
            Core.<*> (x Core..:? "CompletedPieces")
            Core.<*> (x Core..:? "TotalPieces")
            Core.<*> (x Core..:? "CompletionDate")
            Core.<*> (x Core..: "JobStatus")
            Core.<*> (x Core..: "CreationDate")
            Core.<*> (x Core..: "Type")
            Core.<*> (x Core..: "Definition")
            Core.<*> (x Core..: "Id")
            Core.<*> (x Core..: "ApplicationId")
      )

instance Core.Hashable ExportJobResponse

instance Core.NFData ExportJobResponse
