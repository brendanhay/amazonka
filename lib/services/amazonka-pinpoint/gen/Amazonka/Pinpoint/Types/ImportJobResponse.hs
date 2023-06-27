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
-- Module      : Amazonka.Pinpoint.Types.ImportJobResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.ImportJobResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.ImportJobResource
import Amazonka.Pinpoint.Types.JobStatus
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the status and settings of a job that imports
-- endpoint definitions from one or more files. The files can be stored in
-- an Amazon Simple Storage Service (Amazon S3) bucket or uploaded directly
-- from a computer by using the Amazon Pinpoint console.
--
-- /See:/ 'newImportJobResponse' smart constructor.
data ImportJobResponse = ImportJobResponse'
  { -- | The number of pieces that were processed successfully (completed) by the
    -- import job, as of the time of the request.
    completedPieces :: Prelude.Maybe Prelude.Int,
    -- | The date, in ISO 8601 format, when the import job was completed.
    completionDate :: Prelude.Maybe Prelude.Text,
    -- | The number of pieces that weren\'t processed successfully (failed) by
    -- the import job, as of the time of the request.
    failedPieces :: Prelude.Maybe Prelude.Int,
    -- | An array of entries, one for each of the first 100 entries that weren\'t
    -- processed successfully (failed) by the import job, if any.
    failures :: Prelude.Maybe [Prelude.Text],
    -- | The total number of endpoint definitions that weren\'t processed
    -- successfully (failed) by the import job, typically because an error,
    -- such as a syntax error, occurred.
    totalFailures :: Prelude.Maybe Prelude.Int,
    -- | The total number of pieces that must be processed to complete the import
    -- job. Each piece consists of an approximately equal portion of the
    -- endpoint definitions that are part of the import job.
    totalPieces :: Prelude.Maybe Prelude.Int,
    -- | The total number of endpoint definitions that were processed by the
    -- import job.
    totalProcessed :: Prelude.Maybe Prelude.Int,
    -- | The status of the import job. The job status is FAILED if Amazon
    -- Pinpoint wasn\'t able to process one or more pieces in the job.
    jobStatus :: JobStatus,
    -- | The date, in ISO 8601 format, when the import job was created.
    creationDate :: Prelude.Text,
    -- | The job type. This value is IMPORT for import jobs.
    type' :: Prelude.Text,
    -- | The resource settings that apply to the import job.
    definition :: ImportJobResource,
    -- | The unique identifier for the import job.
    id :: Prelude.Text,
    -- | The unique identifier for the application that\'s associated with the
    -- import job.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completedPieces', 'importJobResponse_completedPieces' - The number of pieces that were processed successfully (completed) by the
-- import job, as of the time of the request.
--
-- 'completionDate', 'importJobResponse_completionDate' - The date, in ISO 8601 format, when the import job was completed.
--
-- 'failedPieces', 'importJobResponse_failedPieces' - The number of pieces that weren\'t processed successfully (failed) by
-- the import job, as of the time of the request.
--
-- 'failures', 'importJobResponse_failures' - An array of entries, one for each of the first 100 entries that weren\'t
-- processed successfully (failed) by the import job, if any.
--
-- 'totalFailures', 'importJobResponse_totalFailures' - The total number of endpoint definitions that weren\'t processed
-- successfully (failed) by the import job, typically because an error,
-- such as a syntax error, occurred.
--
-- 'totalPieces', 'importJobResponse_totalPieces' - The total number of pieces that must be processed to complete the import
-- job. Each piece consists of an approximately equal portion of the
-- endpoint definitions that are part of the import job.
--
-- 'totalProcessed', 'importJobResponse_totalProcessed' - The total number of endpoint definitions that were processed by the
-- import job.
--
-- 'jobStatus', 'importJobResponse_jobStatus' - The status of the import job. The job status is FAILED if Amazon
-- Pinpoint wasn\'t able to process one or more pieces in the job.
--
-- 'creationDate', 'importJobResponse_creationDate' - The date, in ISO 8601 format, when the import job was created.
--
-- 'type'', 'importJobResponse_type' - The job type. This value is IMPORT for import jobs.
--
-- 'definition', 'importJobResponse_definition' - The resource settings that apply to the import job.
--
-- 'id', 'importJobResponse_id' - The unique identifier for the import job.
--
-- 'applicationId', 'importJobResponse_applicationId' - The unique identifier for the application that\'s associated with the
-- import job.
newImportJobResponse ::
  -- | 'jobStatus'
  JobStatus ->
  -- | 'creationDate'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  -- | 'definition'
  ImportJobResource ->
  -- | 'id'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  ImportJobResponse
newImportJobResponse
  pJobStatus_
  pCreationDate_
  pType_
  pDefinition_
  pId_
  pApplicationId_ =
    ImportJobResponse'
      { completedPieces =
          Prelude.Nothing,
        completionDate = Prelude.Nothing,
        failedPieces = Prelude.Nothing,
        failures = Prelude.Nothing,
        totalFailures = Prelude.Nothing,
        totalPieces = Prelude.Nothing,
        totalProcessed = Prelude.Nothing,
        jobStatus = pJobStatus_,
        creationDate = pCreationDate_,
        type' = pType_,
        definition = pDefinition_,
        id = pId_,
        applicationId = pApplicationId_
      }

-- | The number of pieces that were processed successfully (completed) by the
-- import job, as of the time of the request.
importJobResponse_completedPieces :: Lens.Lens' ImportJobResponse (Prelude.Maybe Prelude.Int)
importJobResponse_completedPieces = Lens.lens (\ImportJobResponse' {completedPieces} -> completedPieces) (\s@ImportJobResponse' {} a -> s {completedPieces = a} :: ImportJobResponse)

-- | The date, in ISO 8601 format, when the import job was completed.
importJobResponse_completionDate :: Lens.Lens' ImportJobResponse (Prelude.Maybe Prelude.Text)
importJobResponse_completionDate = Lens.lens (\ImportJobResponse' {completionDate} -> completionDate) (\s@ImportJobResponse' {} a -> s {completionDate = a} :: ImportJobResponse)

-- | The number of pieces that weren\'t processed successfully (failed) by
-- the import job, as of the time of the request.
importJobResponse_failedPieces :: Lens.Lens' ImportJobResponse (Prelude.Maybe Prelude.Int)
importJobResponse_failedPieces = Lens.lens (\ImportJobResponse' {failedPieces} -> failedPieces) (\s@ImportJobResponse' {} a -> s {failedPieces = a} :: ImportJobResponse)

-- | An array of entries, one for each of the first 100 entries that weren\'t
-- processed successfully (failed) by the import job, if any.
importJobResponse_failures :: Lens.Lens' ImportJobResponse (Prelude.Maybe [Prelude.Text])
importJobResponse_failures = Lens.lens (\ImportJobResponse' {failures} -> failures) (\s@ImportJobResponse' {} a -> s {failures = a} :: ImportJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The total number of endpoint definitions that weren\'t processed
-- successfully (failed) by the import job, typically because an error,
-- such as a syntax error, occurred.
importJobResponse_totalFailures :: Lens.Lens' ImportJobResponse (Prelude.Maybe Prelude.Int)
importJobResponse_totalFailures = Lens.lens (\ImportJobResponse' {totalFailures} -> totalFailures) (\s@ImportJobResponse' {} a -> s {totalFailures = a} :: ImportJobResponse)

-- | The total number of pieces that must be processed to complete the import
-- job. Each piece consists of an approximately equal portion of the
-- endpoint definitions that are part of the import job.
importJobResponse_totalPieces :: Lens.Lens' ImportJobResponse (Prelude.Maybe Prelude.Int)
importJobResponse_totalPieces = Lens.lens (\ImportJobResponse' {totalPieces} -> totalPieces) (\s@ImportJobResponse' {} a -> s {totalPieces = a} :: ImportJobResponse)

-- | The total number of endpoint definitions that were processed by the
-- import job.
importJobResponse_totalProcessed :: Lens.Lens' ImportJobResponse (Prelude.Maybe Prelude.Int)
importJobResponse_totalProcessed = Lens.lens (\ImportJobResponse' {totalProcessed} -> totalProcessed) (\s@ImportJobResponse' {} a -> s {totalProcessed = a} :: ImportJobResponse)

-- | The status of the import job. The job status is FAILED if Amazon
-- Pinpoint wasn\'t able to process one or more pieces in the job.
importJobResponse_jobStatus :: Lens.Lens' ImportJobResponse JobStatus
importJobResponse_jobStatus = Lens.lens (\ImportJobResponse' {jobStatus} -> jobStatus) (\s@ImportJobResponse' {} a -> s {jobStatus = a} :: ImportJobResponse)

-- | The date, in ISO 8601 format, when the import job was created.
importJobResponse_creationDate :: Lens.Lens' ImportJobResponse Prelude.Text
importJobResponse_creationDate = Lens.lens (\ImportJobResponse' {creationDate} -> creationDate) (\s@ImportJobResponse' {} a -> s {creationDate = a} :: ImportJobResponse)

-- | The job type. This value is IMPORT for import jobs.
importJobResponse_type :: Lens.Lens' ImportJobResponse Prelude.Text
importJobResponse_type = Lens.lens (\ImportJobResponse' {type'} -> type') (\s@ImportJobResponse' {} a -> s {type' = a} :: ImportJobResponse)

-- | The resource settings that apply to the import job.
importJobResponse_definition :: Lens.Lens' ImportJobResponse ImportJobResource
importJobResponse_definition = Lens.lens (\ImportJobResponse' {definition} -> definition) (\s@ImportJobResponse' {} a -> s {definition = a} :: ImportJobResponse)

-- | The unique identifier for the import job.
importJobResponse_id :: Lens.Lens' ImportJobResponse Prelude.Text
importJobResponse_id = Lens.lens (\ImportJobResponse' {id} -> id) (\s@ImportJobResponse' {} a -> s {id = a} :: ImportJobResponse)

-- | The unique identifier for the application that\'s associated with the
-- import job.
importJobResponse_applicationId :: Lens.Lens' ImportJobResponse Prelude.Text
importJobResponse_applicationId = Lens.lens (\ImportJobResponse' {applicationId} -> applicationId) (\s@ImportJobResponse' {} a -> s {applicationId = a} :: ImportJobResponse)

instance Data.FromJSON ImportJobResponse where
  parseJSON =
    Data.withObject
      "ImportJobResponse"
      ( \x ->
          ImportJobResponse'
            Prelude.<$> (x Data..:? "CompletedPieces")
            Prelude.<*> (x Data..:? "CompletionDate")
            Prelude.<*> (x Data..:? "FailedPieces")
            Prelude.<*> (x Data..:? "Failures" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TotalFailures")
            Prelude.<*> (x Data..:? "TotalPieces")
            Prelude.<*> (x Data..:? "TotalProcessed")
            Prelude.<*> (x Data..: "JobStatus")
            Prelude.<*> (x Data..: "CreationDate")
            Prelude.<*> (x Data..: "Type")
            Prelude.<*> (x Data..: "Definition")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "ApplicationId")
      )

instance Prelude.Hashable ImportJobResponse where
  hashWithSalt _salt ImportJobResponse' {..} =
    _salt
      `Prelude.hashWithSalt` completedPieces
      `Prelude.hashWithSalt` completionDate
      `Prelude.hashWithSalt` failedPieces
      `Prelude.hashWithSalt` failures
      `Prelude.hashWithSalt` totalFailures
      `Prelude.hashWithSalt` totalPieces
      `Prelude.hashWithSalt` totalProcessed
      `Prelude.hashWithSalt` jobStatus
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` definition
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData ImportJobResponse where
  rnf ImportJobResponse' {..} =
    Prelude.rnf completedPieces
      `Prelude.seq` Prelude.rnf completionDate
      `Prelude.seq` Prelude.rnf failedPieces
      `Prelude.seq` Prelude.rnf failures
      `Prelude.seq` Prelude.rnf totalFailures
      `Prelude.seq` Prelude.rnf totalPieces
      `Prelude.seq` Prelude.rnf totalProcessed
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf applicationId
