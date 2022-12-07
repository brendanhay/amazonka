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
-- Module      : Amazonka.Personalize.Types.DatasetExportJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.DatasetExportJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types.DatasetExportJobOutput
import Amazonka.Personalize.Types.IngestionMode
import qualified Amazonka.Prelude as Prelude

-- | Describes a job that exports a dataset to an Amazon S3 bucket. For more
-- information, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateDatasetExportJob.html CreateDatasetExportJob>.
--
-- A dataset export job can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- /See:/ 'newDatasetExportJob' smart constructor.
data DatasetExportJob = DatasetExportJob'
  { -- | The Amazon Resource Name (ARN) of the IAM service role that has
    -- permissions to add data to your output Amazon S3 bucket.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The path to the Amazon S3 bucket where the job\'s output is stored. For
    -- example:
    --
    -- @s3:\/\/bucket-name\/folder-name\/@
    jobOutput :: Prelude.Maybe DatasetExportJobOutput,
    -- | The creation date and time (in Unix time) of the dataset export job.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the export job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The status of the dataset export job.
    --
    -- A dataset export job can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the dataset to export.
    datasetArn :: Prelude.Maybe Prelude.Text,
    -- | The data to export, based on how you imported the data. You can choose
    -- to export @BULK@ data that you imported using a dataset import job,
    -- @PUT@ data that you imported incrementally (using the console,
    -- PutEvents, PutUsers and PutItems operations), or @ALL@ for both types.
    -- The default value is @PUT@.
    ingestionMode :: Prelude.Maybe IngestionMode,
    -- | The Amazon Resource Name (ARN) of the dataset export job.
    datasetExportJobArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) the status of the dataset export job
    -- was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | If a dataset export job fails, provides the reason why.
    failureReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'datasetExportJob_roleArn' - The Amazon Resource Name (ARN) of the IAM service role that has
-- permissions to add data to your output Amazon S3 bucket.
--
-- 'jobOutput', 'datasetExportJob_jobOutput' - The path to the Amazon S3 bucket where the job\'s output is stored. For
-- example:
--
-- @s3:\/\/bucket-name\/folder-name\/@
--
-- 'creationDateTime', 'datasetExportJob_creationDateTime' - The creation date and time (in Unix time) of the dataset export job.
--
-- 'jobName', 'datasetExportJob_jobName' - The name of the export job.
--
-- 'status', 'datasetExportJob_status' - The status of the dataset export job.
--
-- A dataset export job can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- 'datasetArn', 'datasetExportJob_datasetArn' - The Amazon Resource Name (ARN) of the dataset to export.
--
-- 'ingestionMode', 'datasetExportJob_ingestionMode' - The data to export, based on how you imported the data. You can choose
-- to export @BULK@ data that you imported using a dataset import job,
-- @PUT@ data that you imported incrementally (using the console,
-- PutEvents, PutUsers and PutItems operations), or @ALL@ for both types.
-- The default value is @PUT@.
--
-- 'datasetExportJobArn', 'datasetExportJob_datasetExportJobArn' - The Amazon Resource Name (ARN) of the dataset export job.
--
-- 'lastUpdatedDateTime', 'datasetExportJob_lastUpdatedDateTime' - The date and time (in Unix time) the status of the dataset export job
-- was last updated.
--
-- 'failureReason', 'datasetExportJob_failureReason' - If a dataset export job fails, provides the reason why.
newDatasetExportJob ::
  DatasetExportJob
newDatasetExportJob =
  DatasetExportJob'
    { roleArn = Prelude.Nothing,
      jobOutput = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      jobName = Prelude.Nothing,
      status = Prelude.Nothing,
      datasetArn = Prelude.Nothing,
      ingestionMode = Prelude.Nothing,
      datasetExportJobArn = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the IAM service role that has
-- permissions to add data to your output Amazon S3 bucket.
datasetExportJob_roleArn :: Lens.Lens' DatasetExportJob (Prelude.Maybe Prelude.Text)
datasetExportJob_roleArn = Lens.lens (\DatasetExportJob' {roleArn} -> roleArn) (\s@DatasetExportJob' {} a -> s {roleArn = a} :: DatasetExportJob)

-- | The path to the Amazon S3 bucket where the job\'s output is stored. For
-- example:
--
-- @s3:\/\/bucket-name\/folder-name\/@
datasetExportJob_jobOutput :: Lens.Lens' DatasetExportJob (Prelude.Maybe DatasetExportJobOutput)
datasetExportJob_jobOutput = Lens.lens (\DatasetExportJob' {jobOutput} -> jobOutput) (\s@DatasetExportJob' {} a -> s {jobOutput = a} :: DatasetExportJob)

-- | The creation date and time (in Unix time) of the dataset export job.
datasetExportJob_creationDateTime :: Lens.Lens' DatasetExportJob (Prelude.Maybe Prelude.UTCTime)
datasetExportJob_creationDateTime = Lens.lens (\DatasetExportJob' {creationDateTime} -> creationDateTime) (\s@DatasetExportJob' {} a -> s {creationDateTime = a} :: DatasetExportJob) Prelude.. Lens.mapping Data._Time

-- | The name of the export job.
datasetExportJob_jobName :: Lens.Lens' DatasetExportJob (Prelude.Maybe Prelude.Text)
datasetExportJob_jobName = Lens.lens (\DatasetExportJob' {jobName} -> jobName) (\s@DatasetExportJob' {} a -> s {jobName = a} :: DatasetExportJob)

-- | The status of the dataset export job.
--
-- A dataset export job can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
datasetExportJob_status :: Lens.Lens' DatasetExportJob (Prelude.Maybe Prelude.Text)
datasetExportJob_status = Lens.lens (\DatasetExportJob' {status} -> status) (\s@DatasetExportJob' {} a -> s {status = a} :: DatasetExportJob)

-- | The Amazon Resource Name (ARN) of the dataset to export.
datasetExportJob_datasetArn :: Lens.Lens' DatasetExportJob (Prelude.Maybe Prelude.Text)
datasetExportJob_datasetArn = Lens.lens (\DatasetExportJob' {datasetArn} -> datasetArn) (\s@DatasetExportJob' {} a -> s {datasetArn = a} :: DatasetExportJob)

-- | The data to export, based on how you imported the data. You can choose
-- to export @BULK@ data that you imported using a dataset import job,
-- @PUT@ data that you imported incrementally (using the console,
-- PutEvents, PutUsers and PutItems operations), or @ALL@ for both types.
-- The default value is @PUT@.
datasetExportJob_ingestionMode :: Lens.Lens' DatasetExportJob (Prelude.Maybe IngestionMode)
datasetExportJob_ingestionMode = Lens.lens (\DatasetExportJob' {ingestionMode} -> ingestionMode) (\s@DatasetExportJob' {} a -> s {ingestionMode = a} :: DatasetExportJob)

-- | The Amazon Resource Name (ARN) of the dataset export job.
datasetExportJob_datasetExportJobArn :: Lens.Lens' DatasetExportJob (Prelude.Maybe Prelude.Text)
datasetExportJob_datasetExportJobArn = Lens.lens (\DatasetExportJob' {datasetExportJobArn} -> datasetExportJobArn) (\s@DatasetExportJob' {} a -> s {datasetExportJobArn = a} :: DatasetExportJob)

-- | The date and time (in Unix time) the status of the dataset export job
-- was last updated.
datasetExportJob_lastUpdatedDateTime :: Lens.Lens' DatasetExportJob (Prelude.Maybe Prelude.UTCTime)
datasetExportJob_lastUpdatedDateTime = Lens.lens (\DatasetExportJob' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DatasetExportJob' {} a -> s {lastUpdatedDateTime = a} :: DatasetExportJob) Prelude.. Lens.mapping Data._Time

-- | If a dataset export job fails, provides the reason why.
datasetExportJob_failureReason :: Lens.Lens' DatasetExportJob (Prelude.Maybe Prelude.Text)
datasetExportJob_failureReason = Lens.lens (\DatasetExportJob' {failureReason} -> failureReason) (\s@DatasetExportJob' {} a -> s {failureReason = a} :: DatasetExportJob)

instance Data.FromJSON DatasetExportJob where
  parseJSON =
    Data.withObject
      "DatasetExportJob"
      ( \x ->
          DatasetExportJob'
            Prelude.<$> (x Data..:? "roleArn")
            Prelude.<*> (x Data..:? "jobOutput")
            Prelude.<*> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "jobName")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "datasetArn")
            Prelude.<*> (x Data..:? "ingestionMode")
            Prelude.<*> (x Data..:? "datasetExportJobArn")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "failureReason")
      )

instance Prelude.Hashable DatasetExportJob where
  hashWithSalt _salt DatasetExportJob' {..} =
    _salt `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` jobOutput
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` datasetArn
      `Prelude.hashWithSalt` ingestionMode
      `Prelude.hashWithSalt` datasetExportJobArn
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData DatasetExportJob where
  rnf DatasetExportJob' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf jobOutput
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf ingestionMode
      `Prelude.seq` Prelude.rnf datasetExportJobArn
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf failureReason
