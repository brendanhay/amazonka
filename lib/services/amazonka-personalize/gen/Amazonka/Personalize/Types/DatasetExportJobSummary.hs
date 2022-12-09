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
-- Module      : Amazonka.Personalize.Types.DatasetExportJobSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.DatasetExportJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the properties of a dataset export job. For a
-- complete listing, call the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeDatasetExportJob.html DescribeDatasetExportJob>
-- API.
--
-- /See:/ 'newDatasetExportJobSummary' smart constructor.
data DatasetExportJobSummary = DatasetExportJobSummary'
  { -- | The date and time (in Unix time) that the dataset export job was
    -- created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the dataset export job.
    datasetExportJobArn :: Prelude.Maybe Prelude.Text,
    -- | If a dataset export job fails, the reason behind the failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The name of the dataset export job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the dataset export job status was
    -- last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the dataset export job.
    --
    -- A dataset export job can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetExportJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'datasetExportJobSummary_creationDateTime' - The date and time (in Unix time) that the dataset export job was
-- created.
--
-- 'datasetExportJobArn', 'datasetExportJobSummary_datasetExportJobArn' - The Amazon Resource Name (ARN) of the dataset export job.
--
-- 'failureReason', 'datasetExportJobSummary_failureReason' - If a dataset export job fails, the reason behind the failure.
--
-- 'jobName', 'datasetExportJobSummary_jobName' - The name of the dataset export job.
--
-- 'lastUpdatedDateTime', 'datasetExportJobSummary_lastUpdatedDateTime' - The date and time (in Unix time) that the dataset export job status was
-- last updated.
--
-- 'status', 'datasetExportJobSummary_status' - The status of the dataset export job.
--
-- A dataset export job can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
newDatasetExportJobSummary ::
  DatasetExportJobSummary
newDatasetExportJobSummary =
  DatasetExportJobSummary'
    { creationDateTime =
        Prelude.Nothing,
      datasetExportJobArn = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      jobName = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The date and time (in Unix time) that the dataset export job was
-- created.
datasetExportJobSummary_creationDateTime :: Lens.Lens' DatasetExportJobSummary (Prelude.Maybe Prelude.UTCTime)
datasetExportJobSummary_creationDateTime = Lens.lens (\DatasetExportJobSummary' {creationDateTime} -> creationDateTime) (\s@DatasetExportJobSummary' {} a -> s {creationDateTime = a} :: DatasetExportJobSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the dataset export job.
datasetExportJobSummary_datasetExportJobArn :: Lens.Lens' DatasetExportJobSummary (Prelude.Maybe Prelude.Text)
datasetExportJobSummary_datasetExportJobArn = Lens.lens (\DatasetExportJobSummary' {datasetExportJobArn} -> datasetExportJobArn) (\s@DatasetExportJobSummary' {} a -> s {datasetExportJobArn = a} :: DatasetExportJobSummary)

-- | If a dataset export job fails, the reason behind the failure.
datasetExportJobSummary_failureReason :: Lens.Lens' DatasetExportJobSummary (Prelude.Maybe Prelude.Text)
datasetExportJobSummary_failureReason = Lens.lens (\DatasetExportJobSummary' {failureReason} -> failureReason) (\s@DatasetExportJobSummary' {} a -> s {failureReason = a} :: DatasetExportJobSummary)

-- | The name of the dataset export job.
datasetExportJobSummary_jobName :: Lens.Lens' DatasetExportJobSummary (Prelude.Maybe Prelude.Text)
datasetExportJobSummary_jobName = Lens.lens (\DatasetExportJobSummary' {jobName} -> jobName) (\s@DatasetExportJobSummary' {} a -> s {jobName = a} :: DatasetExportJobSummary)

-- | The date and time (in Unix time) that the dataset export job status was
-- last updated.
datasetExportJobSummary_lastUpdatedDateTime :: Lens.Lens' DatasetExportJobSummary (Prelude.Maybe Prelude.UTCTime)
datasetExportJobSummary_lastUpdatedDateTime = Lens.lens (\DatasetExportJobSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DatasetExportJobSummary' {} a -> s {lastUpdatedDateTime = a} :: DatasetExportJobSummary) Prelude.. Lens.mapping Data._Time

-- | The status of the dataset export job.
--
-- A dataset export job can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
datasetExportJobSummary_status :: Lens.Lens' DatasetExportJobSummary (Prelude.Maybe Prelude.Text)
datasetExportJobSummary_status = Lens.lens (\DatasetExportJobSummary' {status} -> status) (\s@DatasetExportJobSummary' {} a -> s {status = a} :: DatasetExportJobSummary)

instance Data.FromJSON DatasetExportJobSummary where
  parseJSON =
    Data.withObject
      "DatasetExportJobSummary"
      ( \x ->
          DatasetExportJobSummary'
            Prelude.<$> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "datasetExportJobArn")
            Prelude.<*> (x Data..:? "failureReason")
            Prelude.<*> (x Data..:? "jobName")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable DatasetExportJobSummary where
  hashWithSalt _salt DatasetExportJobSummary' {..} =
    _salt `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` datasetExportJobArn
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` status

instance Prelude.NFData DatasetExportJobSummary where
  rnf DatasetExportJobSummary' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf datasetExportJobArn
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf status
