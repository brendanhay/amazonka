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
-- Module      : Network.AWS.IoT.Types.JobExecutionSummaryForThing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobExecutionSummaryForThing where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.JobExecutionSummary
import qualified Network.AWS.Lens as Lens

-- | The job execution summary for a thing.
--
-- /See:/ 'newJobExecutionSummaryForThing' smart constructor.
data JobExecutionSummaryForThing = JobExecutionSummaryForThing'
  { -- | Contains a subset of information about a job execution.
    jobExecutionSummary :: Core.Maybe JobExecutionSummary,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JobExecutionSummaryForThing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobExecutionSummary', 'jobExecutionSummaryForThing_jobExecutionSummary' - Contains a subset of information about a job execution.
--
-- 'jobId', 'jobExecutionSummaryForThing_jobId' - The unique identifier you assigned to this job when it was created.
newJobExecutionSummaryForThing ::
  JobExecutionSummaryForThing
newJobExecutionSummaryForThing =
  JobExecutionSummaryForThing'
    { jobExecutionSummary =
        Core.Nothing,
      jobId = Core.Nothing
    }

-- | Contains a subset of information about a job execution.
jobExecutionSummaryForThing_jobExecutionSummary :: Lens.Lens' JobExecutionSummaryForThing (Core.Maybe JobExecutionSummary)
jobExecutionSummaryForThing_jobExecutionSummary = Lens.lens (\JobExecutionSummaryForThing' {jobExecutionSummary} -> jobExecutionSummary) (\s@JobExecutionSummaryForThing' {} a -> s {jobExecutionSummary = a} :: JobExecutionSummaryForThing)

-- | The unique identifier you assigned to this job when it was created.
jobExecutionSummaryForThing_jobId :: Lens.Lens' JobExecutionSummaryForThing (Core.Maybe Core.Text)
jobExecutionSummaryForThing_jobId = Lens.lens (\JobExecutionSummaryForThing' {jobId} -> jobId) (\s@JobExecutionSummaryForThing' {} a -> s {jobId = a} :: JobExecutionSummaryForThing)

instance Core.FromJSON JobExecutionSummaryForThing where
  parseJSON =
    Core.withObject
      "JobExecutionSummaryForThing"
      ( \x ->
          JobExecutionSummaryForThing'
            Core.<$> (x Core..:? "jobExecutionSummary")
            Core.<*> (x Core..:? "jobId")
      )

instance Core.Hashable JobExecutionSummaryForThing

instance Core.NFData JobExecutionSummaryForThing
