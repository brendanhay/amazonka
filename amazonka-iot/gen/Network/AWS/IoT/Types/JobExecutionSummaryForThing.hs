{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.IoT.Types.JobExecutionSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The job execution summary for a thing.
--
-- /See:/ 'newJobExecutionSummaryForThing' smart constructor.
data JobExecutionSummaryForThing = JobExecutionSummaryForThing'
  { -- | Contains a subset of information about a job execution.
    jobExecutionSummary :: Prelude.Maybe JobExecutionSummary,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      jobId = Prelude.Nothing
    }

-- | Contains a subset of information about a job execution.
jobExecutionSummaryForThing_jobExecutionSummary :: Lens.Lens' JobExecutionSummaryForThing (Prelude.Maybe JobExecutionSummary)
jobExecutionSummaryForThing_jobExecutionSummary = Lens.lens (\JobExecutionSummaryForThing' {jobExecutionSummary} -> jobExecutionSummary) (\s@JobExecutionSummaryForThing' {} a -> s {jobExecutionSummary = a} :: JobExecutionSummaryForThing)

-- | The unique identifier you assigned to this job when it was created.
jobExecutionSummaryForThing_jobId :: Lens.Lens' JobExecutionSummaryForThing (Prelude.Maybe Prelude.Text)
jobExecutionSummaryForThing_jobId = Lens.lens (\JobExecutionSummaryForThing' {jobId} -> jobId) (\s@JobExecutionSummaryForThing' {} a -> s {jobId = a} :: JobExecutionSummaryForThing)

instance Prelude.FromJSON JobExecutionSummaryForThing where
  parseJSON =
    Prelude.withObject
      "JobExecutionSummaryForThing"
      ( \x ->
          JobExecutionSummaryForThing'
            Prelude.<$> (x Prelude..:? "jobExecutionSummary")
            Prelude.<*> (x Prelude..:? "jobId")
      )

instance Prelude.Hashable JobExecutionSummaryForThing

instance Prelude.NFData JobExecutionSummaryForThing
