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
-- Module      : Amazonka.Personalize.Types.DatasetImportJobSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.DatasetImportJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types.ImportMode
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the properties of a dataset import job. For a
-- complete listing, call the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeDatasetImportJob.html DescribeDatasetImportJob>
-- API.
--
-- /See:/ 'newDatasetImportJobSummary' smart constructor.
data DatasetImportJobSummary = DatasetImportJobSummary'
  { -- | The date and time (in Unix time) that the dataset import job was
    -- created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the dataset import job.
    datasetImportJobArn :: Prelude.Maybe Prelude.Text,
    -- | If a dataset import job fails, the reason behind the failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The import mode the dataset import job used to update the data in the
    -- dataset. For more information see
    -- <https://docs.aws.amazon.com/personalize/latest/dg/updating-existing-bulk-data.html Updating existing bulk data>.
    importMode :: Prelude.Maybe ImportMode,
    -- | The name of the dataset import job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the dataset import job status was
    -- last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the dataset import job.
    --
    -- A dataset import job can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
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
-- 'creationDateTime', 'datasetImportJobSummary_creationDateTime' - The date and time (in Unix time) that the dataset import job was
-- created.
--
-- 'datasetImportJobArn', 'datasetImportJobSummary_datasetImportJobArn' - The Amazon Resource Name (ARN) of the dataset import job.
--
-- 'failureReason', 'datasetImportJobSummary_failureReason' - If a dataset import job fails, the reason behind the failure.
--
-- 'importMode', 'datasetImportJobSummary_importMode' - The import mode the dataset import job used to update the data in the
-- dataset. For more information see
-- <https://docs.aws.amazon.com/personalize/latest/dg/updating-existing-bulk-data.html Updating existing bulk data>.
--
-- 'jobName', 'datasetImportJobSummary_jobName' - The name of the dataset import job.
--
-- 'lastUpdatedDateTime', 'datasetImportJobSummary_lastUpdatedDateTime' - The date and time (in Unix time) that the dataset import job status was
-- last updated.
--
-- 'status', 'datasetImportJobSummary_status' - The status of the dataset import job.
--
-- A dataset import job can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
newDatasetImportJobSummary ::
  DatasetImportJobSummary
newDatasetImportJobSummary =
  DatasetImportJobSummary'
    { creationDateTime =
        Prelude.Nothing,
      datasetImportJobArn = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      importMode = Prelude.Nothing,
      jobName = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The date and time (in Unix time) that the dataset import job was
-- created.
datasetImportJobSummary_creationDateTime :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.UTCTime)
datasetImportJobSummary_creationDateTime = Lens.lens (\DatasetImportJobSummary' {creationDateTime} -> creationDateTime) (\s@DatasetImportJobSummary' {} a -> s {creationDateTime = a} :: DatasetImportJobSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the dataset import job.
datasetImportJobSummary_datasetImportJobArn :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.Text)
datasetImportJobSummary_datasetImportJobArn = Lens.lens (\DatasetImportJobSummary' {datasetImportJobArn} -> datasetImportJobArn) (\s@DatasetImportJobSummary' {} a -> s {datasetImportJobArn = a} :: DatasetImportJobSummary)

-- | If a dataset import job fails, the reason behind the failure.
datasetImportJobSummary_failureReason :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.Text)
datasetImportJobSummary_failureReason = Lens.lens (\DatasetImportJobSummary' {failureReason} -> failureReason) (\s@DatasetImportJobSummary' {} a -> s {failureReason = a} :: DatasetImportJobSummary)

-- | The import mode the dataset import job used to update the data in the
-- dataset. For more information see
-- <https://docs.aws.amazon.com/personalize/latest/dg/updating-existing-bulk-data.html Updating existing bulk data>.
datasetImportJobSummary_importMode :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe ImportMode)
datasetImportJobSummary_importMode = Lens.lens (\DatasetImportJobSummary' {importMode} -> importMode) (\s@DatasetImportJobSummary' {} a -> s {importMode = a} :: DatasetImportJobSummary)

-- | The name of the dataset import job.
datasetImportJobSummary_jobName :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.Text)
datasetImportJobSummary_jobName = Lens.lens (\DatasetImportJobSummary' {jobName} -> jobName) (\s@DatasetImportJobSummary' {} a -> s {jobName = a} :: DatasetImportJobSummary)

-- | The date and time (in Unix time) that the dataset import job status was
-- last updated.
datasetImportJobSummary_lastUpdatedDateTime :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.UTCTime)
datasetImportJobSummary_lastUpdatedDateTime = Lens.lens (\DatasetImportJobSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DatasetImportJobSummary' {} a -> s {lastUpdatedDateTime = a} :: DatasetImportJobSummary) Prelude.. Lens.mapping Data._Time

-- | The status of the dataset import job.
--
-- A dataset import job can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
datasetImportJobSummary_status :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.Text)
datasetImportJobSummary_status = Lens.lens (\DatasetImportJobSummary' {status} -> status) (\s@DatasetImportJobSummary' {} a -> s {status = a} :: DatasetImportJobSummary)

instance Data.FromJSON DatasetImportJobSummary where
  parseJSON =
    Data.withObject
      "DatasetImportJobSummary"
      ( \x ->
          DatasetImportJobSummary'
            Prelude.<$> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "datasetImportJobArn")
            Prelude.<*> (x Data..:? "failureReason")
            Prelude.<*> (x Data..:? "importMode")
            Prelude.<*> (x Data..:? "jobName")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable DatasetImportJobSummary where
  hashWithSalt _salt DatasetImportJobSummary' {..} =
    _salt `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` datasetImportJobArn
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` importMode
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` status

instance Prelude.NFData DatasetImportJobSummary where
  rnf DatasetImportJobSummary' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf datasetImportJobArn
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf importMode
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf status
