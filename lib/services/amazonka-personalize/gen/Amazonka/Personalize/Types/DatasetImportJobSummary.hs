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
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the dataset import job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The status of the dataset import job.
    --
    -- A dataset import job can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the dataset import job.
    datasetImportJobArn :: Prelude.Maybe Prelude.Text,
    -- | The import mode the dataset import job used to update the data in the
    -- dataset. For more information see
    -- <https://docs.aws.amazon.com/personalize/latest/dg/updating-existing-bulk-data.html Updating existing bulk data>.
    importMode :: Prelude.Maybe ImportMode,
    -- | The date and time (in Unix time) that the dataset import job status was
    -- last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | If a dataset import job fails, the reason behind the failure.
    failureReason :: Prelude.Maybe Prelude.Text
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
-- 'jobName', 'datasetImportJobSummary_jobName' - The name of the dataset import job.
--
-- 'status', 'datasetImportJobSummary_status' - The status of the dataset import job.
--
-- A dataset import job can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- 'datasetImportJobArn', 'datasetImportJobSummary_datasetImportJobArn' - The Amazon Resource Name (ARN) of the dataset import job.
--
-- 'importMode', 'datasetImportJobSummary_importMode' - The import mode the dataset import job used to update the data in the
-- dataset. For more information see
-- <https://docs.aws.amazon.com/personalize/latest/dg/updating-existing-bulk-data.html Updating existing bulk data>.
--
-- 'lastUpdatedDateTime', 'datasetImportJobSummary_lastUpdatedDateTime' - The date and time (in Unix time) that the dataset import job status was
-- last updated.
--
-- 'failureReason', 'datasetImportJobSummary_failureReason' - If a dataset import job fails, the reason behind the failure.
newDatasetImportJobSummary ::
  DatasetImportJobSummary
newDatasetImportJobSummary =
  DatasetImportJobSummary'
    { creationDateTime =
        Prelude.Nothing,
      jobName = Prelude.Nothing,
      status = Prelude.Nothing,
      datasetImportJobArn = Prelude.Nothing,
      importMode = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | The date and time (in Unix time) that the dataset import job was
-- created.
datasetImportJobSummary_creationDateTime :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.UTCTime)
datasetImportJobSummary_creationDateTime = Lens.lens (\DatasetImportJobSummary' {creationDateTime} -> creationDateTime) (\s@DatasetImportJobSummary' {} a -> s {creationDateTime = a} :: DatasetImportJobSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the dataset import job.
datasetImportJobSummary_jobName :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.Text)
datasetImportJobSummary_jobName = Lens.lens (\DatasetImportJobSummary' {jobName} -> jobName) (\s@DatasetImportJobSummary' {} a -> s {jobName = a} :: DatasetImportJobSummary)

-- | The status of the dataset import job.
--
-- A dataset import job can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
datasetImportJobSummary_status :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.Text)
datasetImportJobSummary_status = Lens.lens (\DatasetImportJobSummary' {status} -> status) (\s@DatasetImportJobSummary' {} a -> s {status = a} :: DatasetImportJobSummary)

-- | The Amazon Resource Name (ARN) of the dataset import job.
datasetImportJobSummary_datasetImportJobArn :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.Text)
datasetImportJobSummary_datasetImportJobArn = Lens.lens (\DatasetImportJobSummary' {datasetImportJobArn} -> datasetImportJobArn) (\s@DatasetImportJobSummary' {} a -> s {datasetImportJobArn = a} :: DatasetImportJobSummary)

-- | The import mode the dataset import job used to update the data in the
-- dataset. For more information see
-- <https://docs.aws.amazon.com/personalize/latest/dg/updating-existing-bulk-data.html Updating existing bulk data>.
datasetImportJobSummary_importMode :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe ImportMode)
datasetImportJobSummary_importMode = Lens.lens (\DatasetImportJobSummary' {importMode} -> importMode) (\s@DatasetImportJobSummary' {} a -> s {importMode = a} :: DatasetImportJobSummary)

-- | The date and time (in Unix time) that the dataset import job status was
-- last updated.
datasetImportJobSummary_lastUpdatedDateTime :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.UTCTime)
datasetImportJobSummary_lastUpdatedDateTime = Lens.lens (\DatasetImportJobSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DatasetImportJobSummary' {} a -> s {lastUpdatedDateTime = a} :: DatasetImportJobSummary) Prelude.. Lens.mapping Core._Time

-- | If a dataset import job fails, the reason behind the failure.
datasetImportJobSummary_failureReason :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.Text)
datasetImportJobSummary_failureReason = Lens.lens (\DatasetImportJobSummary' {failureReason} -> failureReason) (\s@DatasetImportJobSummary' {} a -> s {failureReason = a} :: DatasetImportJobSummary)

instance Core.FromJSON DatasetImportJobSummary where
  parseJSON =
    Core.withObject
      "DatasetImportJobSummary"
      ( \x ->
          DatasetImportJobSummary'
            Prelude.<$> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "jobName")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "datasetImportJobArn")
            Prelude.<*> (x Core..:? "importMode")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
            Prelude.<*> (x Core..:? "failureReason")
      )

instance Prelude.Hashable DatasetImportJobSummary where
  hashWithSalt _salt DatasetImportJobSummary' {..} =
    _salt `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` datasetImportJobArn
      `Prelude.hashWithSalt` importMode
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData DatasetImportJobSummary where
  rnf DatasetImportJobSummary' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf datasetImportJobArn
      `Prelude.seq` Prelude.rnf importMode
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf failureReason
