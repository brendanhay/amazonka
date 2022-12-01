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
-- Module      : Amazonka.Personalize.Types.DatasetImportJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.DatasetImportJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Personalize.Types.DataSource
import Amazonka.Personalize.Types.ImportMode
import qualified Amazonka.Prelude as Prelude

-- | Describes a job that imports training data from a data source (Amazon S3
-- bucket) to an Amazon Personalize dataset. For more information, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateDatasetImportJob.html CreateDatasetImportJob>.
--
-- A dataset import job can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- /See:/ 'newDatasetImportJob' smart constructor.
data DatasetImportJob = DatasetImportJob'
  { -- | The ARN of the IAM role that has permissions to read from the Amazon S3
    -- data source.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The creation date and time (in Unix time) of the dataset import job.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the import job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The status of the dataset import job.
    --
    -- A dataset import job can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    status :: Prelude.Maybe Prelude.Text,
    -- | Whether the job publishes metrics to Amazon S3 for a metric attribution.
    publishAttributionMetricsToS3 :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the dataset that receives the imported
    -- data.
    datasetArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the dataset import job.
    datasetImportJobArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket that contains the training data to import.
    dataSource :: Prelude.Maybe DataSource,
    -- | The import mode used by the dataset import job to import new records.
    importMode :: Prelude.Maybe ImportMode,
    -- | The date and time (in Unix time) the dataset was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | If a dataset import job fails, provides the reason why.
    failureReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'datasetImportJob_roleArn' - The ARN of the IAM role that has permissions to read from the Amazon S3
-- data source.
--
-- 'creationDateTime', 'datasetImportJob_creationDateTime' - The creation date and time (in Unix time) of the dataset import job.
--
-- 'jobName', 'datasetImportJob_jobName' - The name of the import job.
--
-- 'status', 'datasetImportJob_status' - The status of the dataset import job.
--
-- A dataset import job can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- 'publishAttributionMetricsToS3', 'datasetImportJob_publishAttributionMetricsToS3' - Whether the job publishes metrics to Amazon S3 for a metric attribution.
--
-- 'datasetArn', 'datasetImportJob_datasetArn' - The Amazon Resource Name (ARN) of the dataset that receives the imported
-- data.
--
-- 'datasetImportJobArn', 'datasetImportJob_datasetImportJobArn' - The ARN of the dataset import job.
--
-- 'dataSource', 'datasetImportJob_dataSource' - The Amazon S3 bucket that contains the training data to import.
--
-- 'importMode', 'datasetImportJob_importMode' - The import mode used by the dataset import job to import new records.
--
-- 'lastUpdatedDateTime', 'datasetImportJob_lastUpdatedDateTime' - The date and time (in Unix time) the dataset was last updated.
--
-- 'failureReason', 'datasetImportJob_failureReason' - If a dataset import job fails, provides the reason why.
newDatasetImportJob ::
  DatasetImportJob
newDatasetImportJob =
  DatasetImportJob'
    { roleArn = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      jobName = Prelude.Nothing,
      status = Prelude.Nothing,
      publishAttributionMetricsToS3 = Prelude.Nothing,
      datasetArn = Prelude.Nothing,
      datasetImportJobArn = Prelude.Nothing,
      dataSource = Prelude.Nothing,
      importMode = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | The ARN of the IAM role that has permissions to read from the Amazon S3
-- data source.
datasetImportJob_roleArn :: Lens.Lens' DatasetImportJob (Prelude.Maybe Prelude.Text)
datasetImportJob_roleArn = Lens.lens (\DatasetImportJob' {roleArn} -> roleArn) (\s@DatasetImportJob' {} a -> s {roleArn = a} :: DatasetImportJob)

-- | The creation date and time (in Unix time) of the dataset import job.
datasetImportJob_creationDateTime :: Lens.Lens' DatasetImportJob (Prelude.Maybe Prelude.UTCTime)
datasetImportJob_creationDateTime = Lens.lens (\DatasetImportJob' {creationDateTime} -> creationDateTime) (\s@DatasetImportJob' {} a -> s {creationDateTime = a} :: DatasetImportJob) Prelude.. Lens.mapping Core._Time

-- | The name of the import job.
datasetImportJob_jobName :: Lens.Lens' DatasetImportJob (Prelude.Maybe Prelude.Text)
datasetImportJob_jobName = Lens.lens (\DatasetImportJob' {jobName} -> jobName) (\s@DatasetImportJob' {} a -> s {jobName = a} :: DatasetImportJob)

-- | The status of the dataset import job.
--
-- A dataset import job can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
datasetImportJob_status :: Lens.Lens' DatasetImportJob (Prelude.Maybe Prelude.Text)
datasetImportJob_status = Lens.lens (\DatasetImportJob' {status} -> status) (\s@DatasetImportJob' {} a -> s {status = a} :: DatasetImportJob)

-- | Whether the job publishes metrics to Amazon S3 for a metric attribution.
datasetImportJob_publishAttributionMetricsToS3 :: Lens.Lens' DatasetImportJob (Prelude.Maybe Prelude.Bool)
datasetImportJob_publishAttributionMetricsToS3 = Lens.lens (\DatasetImportJob' {publishAttributionMetricsToS3} -> publishAttributionMetricsToS3) (\s@DatasetImportJob' {} a -> s {publishAttributionMetricsToS3 = a} :: DatasetImportJob)

-- | The Amazon Resource Name (ARN) of the dataset that receives the imported
-- data.
datasetImportJob_datasetArn :: Lens.Lens' DatasetImportJob (Prelude.Maybe Prelude.Text)
datasetImportJob_datasetArn = Lens.lens (\DatasetImportJob' {datasetArn} -> datasetArn) (\s@DatasetImportJob' {} a -> s {datasetArn = a} :: DatasetImportJob)

-- | The ARN of the dataset import job.
datasetImportJob_datasetImportJobArn :: Lens.Lens' DatasetImportJob (Prelude.Maybe Prelude.Text)
datasetImportJob_datasetImportJobArn = Lens.lens (\DatasetImportJob' {datasetImportJobArn} -> datasetImportJobArn) (\s@DatasetImportJob' {} a -> s {datasetImportJobArn = a} :: DatasetImportJob)

-- | The Amazon S3 bucket that contains the training data to import.
datasetImportJob_dataSource :: Lens.Lens' DatasetImportJob (Prelude.Maybe DataSource)
datasetImportJob_dataSource = Lens.lens (\DatasetImportJob' {dataSource} -> dataSource) (\s@DatasetImportJob' {} a -> s {dataSource = a} :: DatasetImportJob)

-- | The import mode used by the dataset import job to import new records.
datasetImportJob_importMode :: Lens.Lens' DatasetImportJob (Prelude.Maybe ImportMode)
datasetImportJob_importMode = Lens.lens (\DatasetImportJob' {importMode} -> importMode) (\s@DatasetImportJob' {} a -> s {importMode = a} :: DatasetImportJob)

-- | The date and time (in Unix time) the dataset was last updated.
datasetImportJob_lastUpdatedDateTime :: Lens.Lens' DatasetImportJob (Prelude.Maybe Prelude.UTCTime)
datasetImportJob_lastUpdatedDateTime = Lens.lens (\DatasetImportJob' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DatasetImportJob' {} a -> s {lastUpdatedDateTime = a} :: DatasetImportJob) Prelude.. Lens.mapping Core._Time

-- | If a dataset import job fails, provides the reason why.
datasetImportJob_failureReason :: Lens.Lens' DatasetImportJob (Prelude.Maybe Prelude.Text)
datasetImportJob_failureReason = Lens.lens (\DatasetImportJob' {failureReason} -> failureReason) (\s@DatasetImportJob' {} a -> s {failureReason = a} :: DatasetImportJob)

instance Core.FromJSON DatasetImportJob where
  parseJSON =
    Core.withObject
      "DatasetImportJob"
      ( \x ->
          DatasetImportJob'
            Prelude.<$> (x Core..:? "roleArn")
            Prelude.<*> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "jobName")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "publishAttributionMetricsToS3")
            Prelude.<*> (x Core..:? "datasetArn")
            Prelude.<*> (x Core..:? "datasetImportJobArn")
            Prelude.<*> (x Core..:? "dataSource")
            Prelude.<*> (x Core..:? "importMode")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
            Prelude.<*> (x Core..:? "failureReason")
      )

instance Prelude.Hashable DatasetImportJob where
  hashWithSalt _salt DatasetImportJob' {..} =
    _salt `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` publishAttributionMetricsToS3
      `Prelude.hashWithSalt` datasetArn
      `Prelude.hashWithSalt` datasetImportJobArn
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` importMode
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData DatasetImportJob where
  rnf DatasetImportJob' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf publishAttributionMetricsToS3
      `Prelude.seq` Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf datasetImportJobArn
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf importMode
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf failureReason
