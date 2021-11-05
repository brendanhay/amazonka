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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.DatasetExportJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the properties of a dataset export job. For a
-- complete listing, call the DescribeDatasetExportJob API.
--
-- /See:/ 'newDatasetExportJobSummary' smart constructor.
data DatasetExportJobSummary = DatasetExportJobSummary'
  { -- | If a dataset export job fails, the reason behind the failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The status of the dataset export job.
    --
    -- A dataset export job can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the dataset export job.
    datasetExportJobArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the dataset export job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the dataset export job status was
    -- last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The date and time (in Unix time) that the dataset export job was
    -- created.
    creationDateTime :: Prelude.Maybe Core.POSIX
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
-- 'failureReason', 'datasetExportJobSummary_failureReason' - If a dataset export job fails, the reason behind the failure.
--
-- 'status', 'datasetExportJobSummary_status' - The status of the dataset export job.
--
-- A dataset export job can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- 'datasetExportJobArn', 'datasetExportJobSummary_datasetExportJobArn' - The Amazon Resource Name (ARN) of the dataset export job.
--
-- 'jobName', 'datasetExportJobSummary_jobName' - The name of the dataset export job.
--
-- 'lastUpdatedDateTime', 'datasetExportJobSummary_lastUpdatedDateTime' - The date and time (in Unix time) that the dataset export job status was
-- last updated.
--
-- 'creationDateTime', 'datasetExportJobSummary_creationDateTime' - The date and time (in Unix time) that the dataset export job was
-- created.
newDatasetExportJobSummary ::
  DatasetExportJobSummary
newDatasetExportJobSummary =
  DatasetExportJobSummary'
    { failureReason =
        Prelude.Nothing,
      status = Prelude.Nothing,
      datasetExportJobArn = Prelude.Nothing,
      jobName = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      creationDateTime = Prelude.Nothing
    }

-- | If a dataset export job fails, the reason behind the failure.
datasetExportJobSummary_failureReason :: Lens.Lens' DatasetExportJobSummary (Prelude.Maybe Prelude.Text)
datasetExportJobSummary_failureReason = Lens.lens (\DatasetExportJobSummary' {failureReason} -> failureReason) (\s@DatasetExportJobSummary' {} a -> s {failureReason = a} :: DatasetExportJobSummary)

-- | The status of the dataset export job.
--
-- A dataset export job can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
datasetExportJobSummary_status :: Lens.Lens' DatasetExportJobSummary (Prelude.Maybe Prelude.Text)
datasetExportJobSummary_status = Lens.lens (\DatasetExportJobSummary' {status} -> status) (\s@DatasetExportJobSummary' {} a -> s {status = a} :: DatasetExportJobSummary)

-- | The Amazon Resource Name (ARN) of the dataset export job.
datasetExportJobSummary_datasetExportJobArn :: Lens.Lens' DatasetExportJobSummary (Prelude.Maybe Prelude.Text)
datasetExportJobSummary_datasetExportJobArn = Lens.lens (\DatasetExportJobSummary' {datasetExportJobArn} -> datasetExportJobArn) (\s@DatasetExportJobSummary' {} a -> s {datasetExportJobArn = a} :: DatasetExportJobSummary)

-- | The name of the dataset export job.
datasetExportJobSummary_jobName :: Lens.Lens' DatasetExportJobSummary (Prelude.Maybe Prelude.Text)
datasetExportJobSummary_jobName = Lens.lens (\DatasetExportJobSummary' {jobName} -> jobName) (\s@DatasetExportJobSummary' {} a -> s {jobName = a} :: DatasetExportJobSummary)

-- | The date and time (in Unix time) that the dataset export job status was
-- last updated.
datasetExportJobSummary_lastUpdatedDateTime :: Lens.Lens' DatasetExportJobSummary (Prelude.Maybe Prelude.UTCTime)
datasetExportJobSummary_lastUpdatedDateTime = Lens.lens (\DatasetExportJobSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DatasetExportJobSummary' {} a -> s {lastUpdatedDateTime = a} :: DatasetExportJobSummary) Prelude.. Lens.mapping Core._Time

-- | The date and time (in Unix time) that the dataset export job was
-- created.
datasetExportJobSummary_creationDateTime :: Lens.Lens' DatasetExportJobSummary (Prelude.Maybe Prelude.UTCTime)
datasetExportJobSummary_creationDateTime = Lens.lens (\DatasetExportJobSummary' {creationDateTime} -> creationDateTime) (\s@DatasetExportJobSummary' {} a -> s {creationDateTime = a} :: DatasetExportJobSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON DatasetExportJobSummary where
  parseJSON =
    Core.withObject
      "DatasetExportJobSummary"
      ( \x ->
          DatasetExportJobSummary'
            Prelude.<$> (x Core..:? "failureReason")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "datasetExportJobArn")
            Prelude.<*> (x Core..:? "jobName")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
            Prelude.<*> (x Core..:? "creationDateTime")
      )

instance Prelude.Hashable DatasetExportJobSummary

instance Prelude.NFData DatasetExportJobSummary
