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
-- Module      : Amazonka.Forecast.Types.DatasetImportJobSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.DatasetImportJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.DataSource
import Amazonka.Forecast.Types.ImportMode
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the dataset import job properties used in the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_ListDatasetImportJobs.html ListDatasetImportJobs>
-- operation. To get the complete set of properties, call the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_DescribeDatasetImportJob.html DescribeDatasetImportJob>
-- operation, and provide the @DatasetImportJobArn@.
--
-- /See:/ 'newDatasetImportJobSummary' smart constructor.
data DatasetImportJobSummary = DatasetImportJobSummary'
  { -- | When the dataset import job was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The location of the training data to import and an Identity and Access
    -- Management (IAM) role that Amazon Forecast can assume to access the
    -- data. The training data must be stored in an Amazon S3 bucket.
    --
    -- If encryption is used, @DataSource@ includes an Key Management Service
    -- (KMS) key.
    dataSource :: Prelude.Maybe DataSource,
    -- | The Amazon Resource Name (ARN) of the dataset import job.
    datasetImportJobArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the dataset import job.
    datasetImportJobName :: Prelude.Maybe Prelude.Text,
    -- | The import mode of the dataset import job, FULL or INCREMENTAL.
    importMode :: Prelude.Maybe ImportMode,
    -- | The last time the resource was modified. The timestamp depends on the
    -- status of the job:
    --
    -- -   @CREATE_PENDING@ - The @CreationTime@.
    --
    -- -   @CREATE_IN_PROGRESS@ - The current timestamp.
    --
    -- -   @CREATE_STOPPING@ - The current timestamp.
    --
    -- -   @CREATE_STOPPED@ - When the job stopped.
    --
    -- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
    lastModificationTime :: Prelude.Maybe Data.POSIX,
    -- | If an error occurred, an informational message about the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status of the dataset import job. States include:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
    --
    -- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
    --
    -- -   @CREATE_STOPPING@, @CREATE_STOPPED@
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetImportJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'datasetImportJobSummary_creationTime' - When the dataset import job was created.
--
-- 'dataSource', 'datasetImportJobSummary_dataSource' - The location of the training data to import and an Identity and Access
-- Management (IAM) role that Amazon Forecast can assume to access the
-- data. The training data must be stored in an Amazon S3 bucket.
--
-- If encryption is used, @DataSource@ includes an Key Management Service
-- (KMS) key.
--
-- 'datasetImportJobArn', 'datasetImportJobSummary_datasetImportJobArn' - The Amazon Resource Name (ARN) of the dataset import job.
--
-- 'datasetImportJobName', 'datasetImportJobSummary_datasetImportJobName' - The name of the dataset import job.
--
-- 'importMode', 'datasetImportJobSummary_importMode' - The import mode of the dataset import job, FULL or INCREMENTAL.
--
-- 'lastModificationTime', 'datasetImportJobSummary_lastModificationTime' - The last time the resource was modified. The timestamp depends on the
-- status of the job:
--
-- -   @CREATE_PENDING@ - The @CreationTime@.
--
-- -   @CREATE_IN_PROGRESS@ - The current timestamp.
--
-- -   @CREATE_STOPPING@ - The current timestamp.
--
-- -   @CREATE_STOPPED@ - When the job stopped.
--
-- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
--
-- 'message', 'datasetImportJobSummary_message' - If an error occurred, an informational message about the error.
--
-- 'status', 'datasetImportJobSummary_status' - The status of the dataset import job. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
newDatasetImportJobSummary ::
  DatasetImportJobSummary
newDatasetImportJobSummary =
  DatasetImportJobSummary'
    { creationTime =
        Prelude.Nothing,
      dataSource = Prelude.Nothing,
      datasetImportJobArn = Prelude.Nothing,
      datasetImportJobName = Prelude.Nothing,
      importMode = Prelude.Nothing,
      lastModificationTime = Prelude.Nothing,
      message = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | When the dataset import job was created.
datasetImportJobSummary_creationTime :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.UTCTime)
datasetImportJobSummary_creationTime = Lens.lens (\DatasetImportJobSummary' {creationTime} -> creationTime) (\s@DatasetImportJobSummary' {} a -> s {creationTime = a} :: DatasetImportJobSummary) Prelude.. Lens.mapping Data._Time

-- | The location of the training data to import and an Identity and Access
-- Management (IAM) role that Amazon Forecast can assume to access the
-- data. The training data must be stored in an Amazon S3 bucket.
--
-- If encryption is used, @DataSource@ includes an Key Management Service
-- (KMS) key.
datasetImportJobSummary_dataSource :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe DataSource)
datasetImportJobSummary_dataSource = Lens.lens (\DatasetImportJobSummary' {dataSource} -> dataSource) (\s@DatasetImportJobSummary' {} a -> s {dataSource = a} :: DatasetImportJobSummary)

-- | The Amazon Resource Name (ARN) of the dataset import job.
datasetImportJobSummary_datasetImportJobArn :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.Text)
datasetImportJobSummary_datasetImportJobArn = Lens.lens (\DatasetImportJobSummary' {datasetImportJobArn} -> datasetImportJobArn) (\s@DatasetImportJobSummary' {} a -> s {datasetImportJobArn = a} :: DatasetImportJobSummary)

-- | The name of the dataset import job.
datasetImportJobSummary_datasetImportJobName :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.Text)
datasetImportJobSummary_datasetImportJobName = Lens.lens (\DatasetImportJobSummary' {datasetImportJobName} -> datasetImportJobName) (\s@DatasetImportJobSummary' {} a -> s {datasetImportJobName = a} :: DatasetImportJobSummary)

-- | The import mode of the dataset import job, FULL or INCREMENTAL.
datasetImportJobSummary_importMode :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe ImportMode)
datasetImportJobSummary_importMode = Lens.lens (\DatasetImportJobSummary' {importMode} -> importMode) (\s@DatasetImportJobSummary' {} a -> s {importMode = a} :: DatasetImportJobSummary)

-- | The last time the resource was modified. The timestamp depends on the
-- status of the job:
--
-- -   @CREATE_PENDING@ - The @CreationTime@.
--
-- -   @CREATE_IN_PROGRESS@ - The current timestamp.
--
-- -   @CREATE_STOPPING@ - The current timestamp.
--
-- -   @CREATE_STOPPED@ - When the job stopped.
--
-- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
datasetImportJobSummary_lastModificationTime :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.UTCTime)
datasetImportJobSummary_lastModificationTime = Lens.lens (\DatasetImportJobSummary' {lastModificationTime} -> lastModificationTime) (\s@DatasetImportJobSummary' {} a -> s {lastModificationTime = a} :: DatasetImportJobSummary) Prelude.. Lens.mapping Data._Time

-- | If an error occurred, an informational message about the error.
datasetImportJobSummary_message :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.Text)
datasetImportJobSummary_message = Lens.lens (\DatasetImportJobSummary' {message} -> message) (\s@DatasetImportJobSummary' {} a -> s {message = a} :: DatasetImportJobSummary)

-- | The status of the dataset import job. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
datasetImportJobSummary_status :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.Text)
datasetImportJobSummary_status = Lens.lens (\DatasetImportJobSummary' {status} -> status) (\s@DatasetImportJobSummary' {} a -> s {status = a} :: DatasetImportJobSummary)

instance Data.FromJSON DatasetImportJobSummary where
  parseJSON =
    Data.withObject
      "DatasetImportJobSummary"
      ( \x ->
          DatasetImportJobSummary'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DataSource")
            Prelude.<*> (x Data..:? "DatasetImportJobArn")
            Prelude.<*> (x Data..:? "DatasetImportJobName")
            Prelude.<*> (x Data..:? "ImportMode")
            Prelude.<*> (x Data..:? "LastModificationTime")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable DatasetImportJobSummary where
  hashWithSalt _salt DatasetImportJobSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` datasetImportJobArn
      `Prelude.hashWithSalt` datasetImportJobName
      `Prelude.hashWithSalt` importMode
      `Prelude.hashWithSalt` lastModificationTime
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` status

instance Prelude.NFData DatasetImportJobSummary where
  rnf DatasetImportJobSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf datasetImportJobArn
      `Prelude.seq` Prelude.rnf datasetImportJobName
      `Prelude.seq` Prelude.rnf importMode
      `Prelude.seq` Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf status
