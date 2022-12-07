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
-- Module      : Amazonka.IoT.Types.JobExecutionSummaryForThing
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.JobExecutionSummaryForThing where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.JobExecutionSummary
import qualified Amazonka.Prelude as Prelude

-- | The job execution summary for a thing.
--
-- /See:/ 'newJobExecutionSummaryForThing' smart constructor.
data JobExecutionSummaryForThing = JobExecutionSummaryForThing'
  { -- | The unique identifier you assigned to this job when it was created.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | Contains a subset of information about a job execution.
    jobExecutionSummary :: Prelude.Maybe JobExecutionSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobExecutionSummaryForThing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'jobExecutionSummaryForThing_jobId' - The unique identifier you assigned to this job when it was created.
--
-- 'jobExecutionSummary', 'jobExecutionSummaryForThing_jobExecutionSummary' - Contains a subset of information about a job execution.
newJobExecutionSummaryForThing ::
  JobExecutionSummaryForThing
newJobExecutionSummaryForThing =
  JobExecutionSummaryForThing'
    { jobId =
        Prelude.Nothing,
      jobExecutionSummary = Prelude.Nothing
    }

-- | The unique identifier you assigned to this job when it was created.
jobExecutionSummaryForThing_jobId :: Lens.Lens' JobExecutionSummaryForThing (Prelude.Maybe Prelude.Text)
jobExecutionSummaryForThing_jobId = Lens.lens (\JobExecutionSummaryForThing' {jobId} -> jobId) (\s@JobExecutionSummaryForThing' {} a -> s {jobId = a} :: JobExecutionSummaryForThing)

-- | Contains a subset of information about a job execution.
jobExecutionSummaryForThing_jobExecutionSummary :: Lens.Lens' JobExecutionSummaryForThing (Prelude.Maybe JobExecutionSummary)
jobExecutionSummaryForThing_jobExecutionSummary = Lens.lens (\JobExecutionSummaryForThing' {jobExecutionSummary} -> jobExecutionSummary) (\s@JobExecutionSummaryForThing' {} a -> s {jobExecutionSummary = a} :: JobExecutionSummaryForThing)

instance Data.FromJSON JobExecutionSummaryForThing where
  parseJSON =
    Data.withObject
      "JobExecutionSummaryForThing"
      ( \x ->
          JobExecutionSummaryForThing'
            Prelude.<$> (x Data..:? "jobId")
            Prelude.<*> (x Data..:? "jobExecutionSummary")
      )

instance Prelude.Hashable JobExecutionSummaryForThing where
  hashWithSalt _salt JobExecutionSummaryForThing' {..} =
    _salt `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` jobExecutionSummary

instance Prelude.NFData JobExecutionSummaryForThing where
  rnf JobExecutionSummaryForThing' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobExecutionSummary
