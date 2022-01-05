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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.DatasetImportJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the properties of a dataset import job. For a
-- complete listing, call the DescribeDatasetImportJob API.
--
-- /See:/ 'newDatasetImportJobSummary' smart constructor.
data DatasetImportJobSummary = DatasetImportJobSummary'
  { -- | If a dataset import job fails, the reason behind the failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The status of the dataset import job.
    --
    -- A dataset import job can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    status :: Prelude.Maybe Prelude.Text,
    -- | The name of the dataset import job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the dataset import job status was
    -- last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the dataset import job.
    datasetImportJobArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the dataset import job was
    -- created.
    creationDateTime :: Prelude.Maybe Core.POSIX
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
-- 'failureReason', 'datasetImportJobSummary_failureReason' - If a dataset import job fails, the reason behind the failure.
--
-- 'status', 'datasetImportJobSummary_status' - The status of the dataset import job.
--
-- A dataset import job can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- 'jobName', 'datasetImportJobSummary_jobName' - The name of the dataset import job.
--
-- 'lastUpdatedDateTime', 'datasetImportJobSummary_lastUpdatedDateTime' - The date and time (in Unix time) that the dataset import job status was
-- last updated.
--
-- 'datasetImportJobArn', 'datasetImportJobSummary_datasetImportJobArn' - The Amazon Resource Name (ARN) of the dataset import job.
--
-- 'creationDateTime', 'datasetImportJobSummary_creationDateTime' - The date and time (in Unix time) that the dataset import job was
-- created.
newDatasetImportJobSummary ::
  DatasetImportJobSummary
newDatasetImportJobSummary =
  DatasetImportJobSummary'
    { failureReason =
        Prelude.Nothing,
      status = Prelude.Nothing,
      jobName = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      datasetImportJobArn = Prelude.Nothing,
      creationDateTime = Prelude.Nothing
    }

-- | If a dataset import job fails, the reason behind the failure.
datasetImportJobSummary_failureReason :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.Text)
datasetImportJobSummary_failureReason = Lens.lens (\DatasetImportJobSummary' {failureReason} -> failureReason) (\s@DatasetImportJobSummary' {} a -> s {failureReason = a} :: DatasetImportJobSummary)

-- | The status of the dataset import job.
--
-- A dataset import job can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
datasetImportJobSummary_status :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.Text)
datasetImportJobSummary_status = Lens.lens (\DatasetImportJobSummary' {status} -> status) (\s@DatasetImportJobSummary' {} a -> s {status = a} :: DatasetImportJobSummary)

-- | The name of the dataset import job.
datasetImportJobSummary_jobName :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.Text)
datasetImportJobSummary_jobName = Lens.lens (\DatasetImportJobSummary' {jobName} -> jobName) (\s@DatasetImportJobSummary' {} a -> s {jobName = a} :: DatasetImportJobSummary)

-- | The date and time (in Unix time) that the dataset import job status was
-- last updated.
datasetImportJobSummary_lastUpdatedDateTime :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.UTCTime)
datasetImportJobSummary_lastUpdatedDateTime = Lens.lens (\DatasetImportJobSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DatasetImportJobSummary' {} a -> s {lastUpdatedDateTime = a} :: DatasetImportJobSummary) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the dataset import job.
datasetImportJobSummary_datasetImportJobArn :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.Text)
datasetImportJobSummary_datasetImportJobArn = Lens.lens (\DatasetImportJobSummary' {datasetImportJobArn} -> datasetImportJobArn) (\s@DatasetImportJobSummary' {} a -> s {datasetImportJobArn = a} :: DatasetImportJobSummary)

-- | The date and time (in Unix time) that the dataset import job was
-- created.
datasetImportJobSummary_creationDateTime :: Lens.Lens' DatasetImportJobSummary (Prelude.Maybe Prelude.UTCTime)
datasetImportJobSummary_creationDateTime = Lens.lens (\DatasetImportJobSummary' {creationDateTime} -> creationDateTime) (\s@DatasetImportJobSummary' {} a -> s {creationDateTime = a} :: DatasetImportJobSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON DatasetImportJobSummary where
  parseJSON =
    Core.withObject
      "DatasetImportJobSummary"
      ( \x ->
          DatasetImportJobSummary'
            Prelude.<$> (x Core..:? "failureReason")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "jobName")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
            Prelude.<*> (x Core..:? "datasetImportJobArn")
            Prelude.<*> (x Core..:? "creationDateTime")
      )

instance Prelude.Hashable DatasetImportJobSummary where
  hashWithSalt _salt DatasetImportJobSummary' {..} =
    _salt `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` datasetImportJobArn
      `Prelude.hashWithSalt` creationDateTime

instance Prelude.NFData DatasetImportJobSummary where
  rnf DatasetImportJobSummary' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf datasetImportJobArn
      `Prelude.seq` Prelude.rnf creationDateTime
