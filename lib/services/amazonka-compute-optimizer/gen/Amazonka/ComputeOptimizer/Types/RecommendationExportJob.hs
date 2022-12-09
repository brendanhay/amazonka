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
-- Module      : Amazonka.ComputeOptimizer.Types.RecommendationExportJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.RecommendationExportJob where

import Amazonka.ComputeOptimizer.Types.ExportDestination
import Amazonka.ComputeOptimizer.Types.JobStatus
import Amazonka.ComputeOptimizer.Types.ResourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a recommendation export job.
--
-- Use the DescribeRecommendationExportJobs action to view your
-- recommendation export jobs.
--
-- Use the ExportAutoScalingGroupRecommendations or
-- ExportEC2InstanceRecommendations actions to request an export of your
-- recommendations.
--
-- /See:/ 'newRecommendationExportJob' smart constructor.
data RecommendationExportJob = RecommendationExportJob'
  { -- | The timestamp of when the export job was created.
    creationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | An object that describes the destination of the export file.
    destination :: Prelude.Maybe ExportDestination,
    -- | The reason for an export job failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The identification number of the export job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the export job was last updated.
    lastUpdatedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The resource type of the exported recommendations.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The status of the export job.
    status :: Prelude.Maybe JobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'recommendationExportJob_creationTimestamp' - The timestamp of when the export job was created.
--
-- 'destination', 'recommendationExportJob_destination' - An object that describes the destination of the export file.
--
-- 'failureReason', 'recommendationExportJob_failureReason' - The reason for an export job failure.
--
-- 'jobId', 'recommendationExportJob_jobId' - The identification number of the export job.
--
-- 'lastUpdatedTimestamp', 'recommendationExportJob_lastUpdatedTimestamp' - The timestamp of when the export job was last updated.
--
-- 'resourceType', 'recommendationExportJob_resourceType' - The resource type of the exported recommendations.
--
-- 'status', 'recommendationExportJob_status' - The status of the export job.
newRecommendationExportJob ::
  RecommendationExportJob
newRecommendationExportJob =
  RecommendationExportJob'
    { creationTimestamp =
        Prelude.Nothing,
      destination = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      jobId = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The timestamp of when the export job was created.
recommendationExportJob_creationTimestamp :: Lens.Lens' RecommendationExportJob (Prelude.Maybe Prelude.UTCTime)
recommendationExportJob_creationTimestamp = Lens.lens (\RecommendationExportJob' {creationTimestamp} -> creationTimestamp) (\s@RecommendationExportJob' {} a -> s {creationTimestamp = a} :: RecommendationExportJob) Prelude.. Lens.mapping Data._Time

-- | An object that describes the destination of the export file.
recommendationExportJob_destination :: Lens.Lens' RecommendationExportJob (Prelude.Maybe ExportDestination)
recommendationExportJob_destination = Lens.lens (\RecommendationExportJob' {destination} -> destination) (\s@RecommendationExportJob' {} a -> s {destination = a} :: RecommendationExportJob)

-- | The reason for an export job failure.
recommendationExportJob_failureReason :: Lens.Lens' RecommendationExportJob (Prelude.Maybe Prelude.Text)
recommendationExportJob_failureReason = Lens.lens (\RecommendationExportJob' {failureReason} -> failureReason) (\s@RecommendationExportJob' {} a -> s {failureReason = a} :: RecommendationExportJob)

-- | The identification number of the export job.
recommendationExportJob_jobId :: Lens.Lens' RecommendationExportJob (Prelude.Maybe Prelude.Text)
recommendationExportJob_jobId = Lens.lens (\RecommendationExportJob' {jobId} -> jobId) (\s@RecommendationExportJob' {} a -> s {jobId = a} :: RecommendationExportJob)

-- | The timestamp of when the export job was last updated.
recommendationExportJob_lastUpdatedTimestamp :: Lens.Lens' RecommendationExportJob (Prelude.Maybe Prelude.UTCTime)
recommendationExportJob_lastUpdatedTimestamp = Lens.lens (\RecommendationExportJob' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@RecommendationExportJob' {} a -> s {lastUpdatedTimestamp = a} :: RecommendationExportJob) Prelude.. Lens.mapping Data._Time

-- | The resource type of the exported recommendations.
recommendationExportJob_resourceType :: Lens.Lens' RecommendationExportJob (Prelude.Maybe ResourceType)
recommendationExportJob_resourceType = Lens.lens (\RecommendationExportJob' {resourceType} -> resourceType) (\s@RecommendationExportJob' {} a -> s {resourceType = a} :: RecommendationExportJob)

-- | The status of the export job.
recommendationExportJob_status :: Lens.Lens' RecommendationExportJob (Prelude.Maybe JobStatus)
recommendationExportJob_status = Lens.lens (\RecommendationExportJob' {status} -> status) (\s@RecommendationExportJob' {} a -> s {status = a} :: RecommendationExportJob)

instance Data.FromJSON RecommendationExportJob where
  parseJSON =
    Data.withObject
      "RecommendationExportJob"
      ( \x ->
          RecommendationExportJob'
            Prelude.<$> (x Data..:? "creationTimestamp")
            Prelude.<*> (x Data..:? "destination")
            Prelude.<*> (x Data..:? "failureReason")
            Prelude.<*> (x Data..:? "jobId")
            Prelude.<*> (x Data..:? "lastUpdatedTimestamp")
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable RecommendationExportJob where
  hashWithSalt _salt RecommendationExportJob' {..} =
    _salt `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` status

instance Prelude.NFData RecommendationExportJob where
  rnf RecommendationExportJob' {..} =
    Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf status
