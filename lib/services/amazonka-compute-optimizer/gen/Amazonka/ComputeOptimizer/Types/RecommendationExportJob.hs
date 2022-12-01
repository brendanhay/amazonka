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
  { -- | An object that describes the destination of the export file.
    destination :: Prelude.Maybe ExportDestination,
    -- | The timestamp of when the export job was last updated.
    lastUpdatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The resource type of the exported recommendations.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The identification number of the export job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The status of the export job.
    status :: Prelude.Maybe JobStatus,
    -- | The timestamp of when the export job was created.
    creationTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The reason for an export job failure.
    failureReason :: Prelude.Maybe Prelude.Text
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
-- 'destination', 'recommendationExportJob_destination' - An object that describes the destination of the export file.
--
-- 'lastUpdatedTimestamp', 'recommendationExportJob_lastUpdatedTimestamp' - The timestamp of when the export job was last updated.
--
-- 'resourceType', 'recommendationExportJob_resourceType' - The resource type of the exported recommendations.
--
-- 'jobId', 'recommendationExportJob_jobId' - The identification number of the export job.
--
-- 'status', 'recommendationExportJob_status' - The status of the export job.
--
-- 'creationTimestamp', 'recommendationExportJob_creationTimestamp' - The timestamp of when the export job was created.
--
-- 'failureReason', 'recommendationExportJob_failureReason' - The reason for an export job failure.
newRecommendationExportJob ::
  RecommendationExportJob
newRecommendationExportJob =
  RecommendationExportJob'
    { destination =
        Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      jobId = Prelude.Nothing,
      status = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | An object that describes the destination of the export file.
recommendationExportJob_destination :: Lens.Lens' RecommendationExportJob (Prelude.Maybe ExportDestination)
recommendationExportJob_destination = Lens.lens (\RecommendationExportJob' {destination} -> destination) (\s@RecommendationExportJob' {} a -> s {destination = a} :: RecommendationExportJob)

-- | The timestamp of when the export job was last updated.
recommendationExportJob_lastUpdatedTimestamp :: Lens.Lens' RecommendationExportJob (Prelude.Maybe Prelude.UTCTime)
recommendationExportJob_lastUpdatedTimestamp = Lens.lens (\RecommendationExportJob' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@RecommendationExportJob' {} a -> s {lastUpdatedTimestamp = a} :: RecommendationExportJob) Prelude.. Lens.mapping Core._Time

-- | The resource type of the exported recommendations.
recommendationExportJob_resourceType :: Lens.Lens' RecommendationExportJob (Prelude.Maybe ResourceType)
recommendationExportJob_resourceType = Lens.lens (\RecommendationExportJob' {resourceType} -> resourceType) (\s@RecommendationExportJob' {} a -> s {resourceType = a} :: RecommendationExportJob)

-- | The identification number of the export job.
recommendationExportJob_jobId :: Lens.Lens' RecommendationExportJob (Prelude.Maybe Prelude.Text)
recommendationExportJob_jobId = Lens.lens (\RecommendationExportJob' {jobId} -> jobId) (\s@RecommendationExportJob' {} a -> s {jobId = a} :: RecommendationExportJob)

-- | The status of the export job.
recommendationExportJob_status :: Lens.Lens' RecommendationExportJob (Prelude.Maybe JobStatus)
recommendationExportJob_status = Lens.lens (\RecommendationExportJob' {status} -> status) (\s@RecommendationExportJob' {} a -> s {status = a} :: RecommendationExportJob)

-- | The timestamp of when the export job was created.
recommendationExportJob_creationTimestamp :: Lens.Lens' RecommendationExportJob (Prelude.Maybe Prelude.UTCTime)
recommendationExportJob_creationTimestamp = Lens.lens (\RecommendationExportJob' {creationTimestamp} -> creationTimestamp) (\s@RecommendationExportJob' {} a -> s {creationTimestamp = a} :: RecommendationExportJob) Prelude.. Lens.mapping Core._Time

-- | The reason for an export job failure.
recommendationExportJob_failureReason :: Lens.Lens' RecommendationExportJob (Prelude.Maybe Prelude.Text)
recommendationExportJob_failureReason = Lens.lens (\RecommendationExportJob' {failureReason} -> failureReason) (\s@RecommendationExportJob' {} a -> s {failureReason = a} :: RecommendationExportJob)

instance Core.FromJSON RecommendationExportJob where
  parseJSON =
    Core.withObject
      "RecommendationExportJob"
      ( \x ->
          RecommendationExportJob'
            Prelude.<$> (x Core..:? "destination")
            Prelude.<*> (x Core..:? "lastUpdatedTimestamp")
            Prelude.<*> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "jobId")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "creationTimestamp")
            Prelude.<*> (x Core..:? "failureReason")
      )

instance Prelude.Hashable RecommendationExportJob where
  hashWithSalt _salt RecommendationExportJob' {..} =
    _salt `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData RecommendationExportJob where
  rnf RecommendationExportJob' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf failureReason
