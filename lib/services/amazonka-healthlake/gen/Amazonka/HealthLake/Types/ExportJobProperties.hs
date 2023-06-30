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
-- Module      : Amazonka.HealthLake.Types.ExportJobProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HealthLake.Types.ExportJobProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HealthLake.Types.JobStatus
import Amazonka.HealthLake.Types.OutputDataConfig
import qualified Amazonka.Prelude as Prelude

-- | The properties of a FHIR export job, including the ID, ARN, name, and
-- the status of the job.
--
-- /See:/ 'newExportJobProperties' smart constructor.
data ExportJobProperties = ExportJobProperties'
  { -- | The Amazon Resource Name used during the initiation of the job.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The time an export job completed.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The user generated name for an export job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | An explanation of any errors that may have occurred during the export
    -- job.
    message :: Prelude.Maybe Prelude.Text,
    -- | The AWS generated ID for an export job.
    jobId :: Prelude.Text,
    -- | The status of a FHIR export job. Possible statuses are SUBMITTED,
    -- IN_PROGRESS, COMPLETED, or FAILED.
    jobStatus :: JobStatus,
    -- | The time an export job was initiated.
    submitTime :: Data.POSIX,
    -- | The AWS generated ID for the Data Store from which files are being
    -- exported for an export job.
    datastoreId :: Prelude.Text,
    -- | The output data configuration that was supplied when the export job was
    -- created.
    outputDataConfig :: OutputDataConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportJobProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataAccessRoleArn', 'exportJobProperties_dataAccessRoleArn' - The Amazon Resource Name used during the initiation of the job.
--
-- 'endTime', 'exportJobProperties_endTime' - The time an export job completed.
--
-- 'jobName', 'exportJobProperties_jobName' - The user generated name for an export job.
--
-- 'message', 'exportJobProperties_message' - An explanation of any errors that may have occurred during the export
-- job.
--
-- 'jobId', 'exportJobProperties_jobId' - The AWS generated ID for an export job.
--
-- 'jobStatus', 'exportJobProperties_jobStatus' - The status of a FHIR export job. Possible statuses are SUBMITTED,
-- IN_PROGRESS, COMPLETED, or FAILED.
--
-- 'submitTime', 'exportJobProperties_submitTime' - The time an export job was initiated.
--
-- 'datastoreId', 'exportJobProperties_datastoreId' - The AWS generated ID for the Data Store from which files are being
-- exported for an export job.
--
-- 'outputDataConfig', 'exportJobProperties_outputDataConfig' - The output data configuration that was supplied when the export job was
-- created.
newExportJobProperties ::
  -- | 'jobId'
  Prelude.Text ->
  -- | 'jobStatus'
  JobStatus ->
  -- | 'submitTime'
  Prelude.UTCTime ->
  -- | 'datastoreId'
  Prelude.Text ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  ExportJobProperties
newExportJobProperties
  pJobId_
  pJobStatus_
  pSubmitTime_
  pDatastoreId_
  pOutputDataConfig_ =
    ExportJobProperties'
      { dataAccessRoleArn =
          Prelude.Nothing,
        endTime = Prelude.Nothing,
        jobName = Prelude.Nothing,
        message = Prelude.Nothing,
        jobId = pJobId_,
        jobStatus = pJobStatus_,
        submitTime = Data._Time Lens.# pSubmitTime_,
        datastoreId = pDatastoreId_,
        outputDataConfig = pOutputDataConfig_
      }

-- | The Amazon Resource Name used during the initiation of the job.
exportJobProperties_dataAccessRoleArn :: Lens.Lens' ExportJobProperties (Prelude.Maybe Prelude.Text)
exportJobProperties_dataAccessRoleArn = Lens.lens (\ExportJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@ExportJobProperties' {} a -> s {dataAccessRoleArn = a} :: ExportJobProperties)

-- | The time an export job completed.
exportJobProperties_endTime :: Lens.Lens' ExportJobProperties (Prelude.Maybe Prelude.UTCTime)
exportJobProperties_endTime = Lens.lens (\ExportJobProperties' {endTime} -> endTime) (\s@ExportJobProperties' {} a -> s {endTime = a} :: ExportJobProperties) Prelude.. Lens.mapping Data._Time

-- | The user generated name for an export job.
exportJobProperties_jobName :: Lens.Lens' ExportJobProperties (Prelude.Maybe Prelude.Text)
exportJobProperties_jobName = Lens.lens (\ExportJobProperties' {jobName} -> jobName) (\s@ExportJobProperties' {} a -> s {jobName = a} :: ExportJobProperties)

-- | An explanation of any errors that may have occurred during the export
-- job.
exportJobProperties_message :: Lens.Lens' ExportJobProperties (Prelude.Maybe Prelude.Text)
exportJobProperties_message = Lens.lens (\ExportJobProperties' {message} -> message) (\s@ExportJobProperties' {} a -> s {message = a} :: ExportJobProperties)

-- | The AWS generated ID for an export job.
exportJobProperties_jobId :: Lens.Lens' ExportJobProperties Prelude.Text
exportJobProperties_jobId = Lens.lens (\ExportJobProperties' {jobId} -> jobId) (\s@ExportJobProperties' {} a -> s {jobId = a} :: ExportJobProperties)

-- | The status of a FHIR export job. Possible statuses are SUBMITTED,
-- IN_PROGRESS, COMPLETED, or FAILED.
exportJobProperties_jobStatus :: Lens.Lens' ExportJobProperties JobStatus
exportJobProperties_jobStatus = Lens.lens (\ExportJobProperties' {jobStatus} -> jobStatus) (\s@ExportJobProperties' {} a -> s {jobStatus = a} :: ExportJobProperties)

-- | The time an export job was initiated.
exportJobProperties_submitTime :: Lens.Lens' ExportJobProperties Prelude.UTCTime
exportJobProperties_submitTime = Lens.lens (\ExportJobProperties' {submitTime} -> submitTime) (\s@ExportJobProperties' {} a -> s {submitTime = a} :: ExportJobProperties) Prelude.. Data._Time

-- | The AWS generated ID for the Data Store from which files are being
-- exported for an export job.
exportJobProperties_datastoreId :: Lens.Lens' ExportJobProperties Prelude.Text
exportJobProperties_datastoreId = Lens.lens (\ExportJobProperties' {datastoreId} -> datastoreId) (\s@ExportJobProperties' {} a -> s {datastoreId = a} :: ExportJobProperties)

-- | The output data configuration that was supplied when the export job was
-- created.
exportJobProperties_outputDataConfig :: Lens.Lens' ExportJobProperties OutputDataConfig
exportJobProperties_outputDataConfig = Lens.lens (\ExportJobProperties' {outputDataConfig} -> outputDataConfig) (\s@ExportJobProperties' {} a -> s {outputDataConfig = a} :: ExportJobProperties)

instance Data.FromJSON ExportJobProperties where
  parseJSON =
    Data.withObject
      "ExportJobProperties"
      ( \x ->
          ExportJobProperties'
            Prelude.<$> (x Data..:? "DataAccessRoleArn")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "JobName")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..: "JobId")
            Prelude.<*> (x Data..: "JobStatus")
            Prelude.<*> (x Data..: "SubmitTime")
            Prelude.<*> (x Data..: "DatastoreId")
            Prelude.<*> (x Data..: "OutputDataConfig")
      )

instance Prelude.Hashable ExportJobProperties where
  hashWithSalt _salt ExportJobProperties' {..} =
    _salt
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` jobStatus
      `Prelude.hashWithSalt` submitTime
      `Prelude.hashWithSalt` datastoreId
      `Prelude.hashWithSalt` outputDataConfig

instance Prelude.NFData ExportJobProperties where
  rnf ExportJobProperties' {..} =
    Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf submitTime
      `Prelude.seq` Prelude.rnf datastoreId
      `Prelude.seq` Prelude.rnf outputDataConfig
