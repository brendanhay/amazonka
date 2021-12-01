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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.DatasetImportJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Personalize.Types.DataSource
import qualified Amazonka.Prelude as Prelude

-- | Describes a job that imports training data from a data source (Amazon S3
-- bucket) to an Amazon Personalize dataset. For more information, see
-- CreateDatasetImportJob.
--
-- A dataset import job can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- /See:/ 'newDatasetImportJob' smart constructor.
data DatasetImportJob = DatasetImportJob'
  { -- | If a dataset import job fails, provides the reason why.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The status of the dataset import job.
    --
    -- A dataset import job can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the dataset that receives the imported
    -- data.
    datasetArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the import job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) the dataset was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the dataset import job.
    datasetImportJobArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket that contains the training data to import.
    dataSource :: Prelude.Maybe DataSource,
    -- | The creation date and time (in Unix time) of the dataset import job.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the IAM role that has permissions to read from the Amazon S3
    -- data source.
    roleArn :: Prelude.Maybe Prelude.Text
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
-- 'failureReason', 'datasetImportJob_failureReason' - If a dataset import job fails, provides the reason why.
--
-- 'status', 'datasetImportJob_status' - The status of the dataset import job.
--
-- A dataset import job can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- 'datasetArn', 'datasetImportJob_datasetArn' - The Amazon Resource Name (ARN) of the dataset that receives the imported
-- data.
--
-- 'jobName', 'datasetImportJob_jobName' - The name of the import job.
--
-- 'lastUpdatedDateTime', 'datasetImportJob_lastUpdatedDateTime' - The date and time (in Unix time) the dataset was last updated.
--
-- 'datasetImportJobArn', 'datasetImportJob_datasetImportJobArn' - The ARN of the dataset import job.
--
-- 'dataSource', 'datasetImportJob_dataSource' - The Amazon S3 bucket that contains the training data to import.
--
-- 'creationDateTime', 'datasetImportJob_creationDateTime' - The creation date and time (in Unix time) of the dataset import job.
--
-- 'roleArn', 'datasetImportJob_roleArn' - The ARN of the IAM role that has permissions to read from the Amazon S3
-- data source.
newDatasetImportJob ::
  DatasetImportJob
newDatasetImportJob =
  DatasetImportJob'
    { failureReason = Prelude.Nothing,
      status = Prelude.Nothing,
      datasetArn = Prelude.Nothing,
      jobName = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      datasetImportJobArn = Prelude.Nothing,
      dataSource = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | If a dataset import job fails, provides the reason why.
datasetImportJob_failureReason :: Lens.Lens' DatasetImportJob (Prelude.Maybe Prelude.Text)
datasetImportJob_failureReason = Lens.lens (\DatasetImportJob' {failureReason} -> failureReason) (\s@DatasetImportJob' {} a -> s {failureReason = a} :: DatasetImportJob)

-- | The status of the dataset import job.
--
-- A dataset import job can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
datasetImportJob_status :: Lens.Lens' DatasetImportJob (Prelude.Maybe Prelude.Text)
datasetImportJob_status = Lens.lens (\DatasetImportJob' {status} -> status) (\s@DatasetImportJob' {} a -> s {status = a} :: DatasetImportJob)

-- | The Amazon Resource Name (ARN) of the dataset that receives the imported
-- data.
datasetImportJob_datasetArn :: Lens.Lens' DatasetImportJob (Prelude.Maybe Prelude.Text)
datasetImportJob_datasetArn = Lens.lens (\DatasetImportJob' {datasetArn} -> datasetArn) (\s@DatasetImportJob' {} a -> s {datasetArn = a} :: DatasetImportJob)

-- | The name of the import job.
datasetImportJob_jobName :: Lens.Lens' DatasetImportJob (Prelude.Maybe Prelude.Text)
datasetImportJob_jobName = Lens.lens (\DatasetImportJob' {jobName} -> jobName) (\s@DatasetImportJob' {} a -> s {jobName = a} :: DatasetImportJob)

-- | The date and time (in Unix time) the dataset was last updated.
datasetImportJob_lastUpdatedDateTime :: Lens.Lens' DatasetImportJob (Prelude.Maybe Prelude.UTCTime)
datasetImportJob_lastUpdatedDateTime = Lens.lens (\DatasetImportJob' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DatasetImportJob' {} a -> s {lastUpdatedDateTime = a} :: DatasetImportJob) Prelude.. Lens.mapping Core._Time

-- | The ARN of the dataset import job.
datasetImportJob_datasetImportJobArn :: Lens.Lens' DatasetImportJob (Prelude.Maybe Prelude.Text)
datasetImportJob_datasetImportJobArn = Lens.lens (\DatasetImportJob' {datasetImportJobArn} -> datasetImportJobArn) (\s@DatasetImportJob' {} a -> s {datasetImportJobArn = a} :: DatasetImportJob)

-- | The Amazon S3 bucket that contains the training data to import.
datasetImportJob_dataSource :: Lens.Lens' DatasetImportJob (Prelude.Maybe DataSource)
datasetImportJob_dataSource = Lens.lens (\DatasetImportJob' {dataSource} -> dataSource) (\s@DatasetImportJob' {} a -> s {dataSource = a} :: DatasetImportJob)

-- | The creation date and time (in Unix time) of the dataset import job.
datasetImportJob_creationDateTime :: Lens.Lens' DatasetImportJob (Prelude.Maybe Prelude.UTCTime)
datasetImportJob_creationDateTime = Lens.lens (\DatasetImportJob' {creationDateTime} -> creationDateTime) (\s@DatasetImportJob' {} a -> s {creationDateTime = a} :: DatasetImportJob) Prelude.. Lens.mapping Core._Time

-- | The ARN of the IAM role that has permissions to read from the Amazon S3
-- data source.
datasetImportJob_roleArn :: Lens.Lens' DatasetImportJob (Prelude.Maybe Prelude.Text)
datasetImportJob_roleArn = Lens.lens (\DatasetImportJob' {roleArn} -> roleArn) (\s@DatasetImportJob' {} a -> s {roleArn = a} :: DatasetImportJob)

instance Core.FromJSON DatasetImportJob where
  parseJSON =
    Core.withObject
      "DatasetImportJob"
      ( \x ->
          DatasetImportJob'
            Prelude.<$> (x Core..:? "failureReason")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "datasetArn")
            Prelude.<*> (x Core..:? "jobName")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
            Prelude.<*> (x Core..:? "datasetImportJobArn")
            Prelude.<*> (x Core..:? "dataSource")
            Prelude.<*> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "roleArn")
      )

instance Prelude.Hashable DatasetImportJob where
  hashWithSalt salt' DatasetImportJob' {..} =
    salt' `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` datasetImportJobArn
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` datasetArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData DatasetImportJob where
  rnf DatasetImportJob' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf datasetImportJobArn
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf status
