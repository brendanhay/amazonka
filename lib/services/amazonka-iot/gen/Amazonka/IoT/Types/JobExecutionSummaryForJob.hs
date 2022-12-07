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
-- Module      : Amazonka.IoT.Types.JobExecutionSummaryForJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.JobExecutionSummaryForJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.JobExecutionSummary
import qualified Amazonka.Prelude as Prelude

-- | Contains a summary of information about job executions for a specific
-- job.
--
-- /See:/ 'newJobExecutionSummaryForJob' smart constructor.
data JobExecutionSummaryForJob = JobExecutionSummaryForJob'
  { -- | The ARN of the thing on which the job execution is running.
    thingArn :: Prelude.Maybe Prelude.Text,
    -- | Contains a subset of information about a job execution.
    jobExecutionSummary :: Prelude.Maybe JobExecutionSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobExecutionSummaryForJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingArn', 'jobExecutionSummaryForJob_thingArn' - The ARN of the thing on which the job execution is running.
--
-- 'jobExecutionSummary', 'jobExecutionSummaryForJob_jobExecutionSummary' - Contains a subset of information about a job execution.
newJobExecutionSummaryForJob ::
  JobExecutionSummaryForJob
newJobExecutionSummaryForJob =
  JobExecutionSummaryForJob'
    { thingArn =
        Prelude.Nothing,
      jobExecutionSummary = Prelude.Nothing
    }

-- | The ARN of the thing on which the job execution is running.
jobExecutionSummaryForJob_thingArn :: Lens.Lens' JobExecutionSummaryForJob (Prelude.Maybe Prelude.Text)
jobExecutionSummaryForJob_thingArn = Lens.lens (\JobExecutionSummaryForJob' {thingArn} -> thingArn) (\s@JobExecutionSummaryForJob' {} a -> s {thingArn = a} :: JobExecutionSummaryForJob)

-- | Contains a subset of information about a job execution.
jobExecutionSummaryForJob_jobExecutionSummary :: Lens.Lens' JobExecutionSummaryForJob (Prelude.Maybe JobExecutionSummary)
jobExecutionSummaryForJob_jobExecutionSummary = Lens.lens (\JobExecutionSummaryForJob' {jobExecutionSummary} -> jobExecutionSummary) (\s@JobExecutionSummaryForJob' {} a -> s {jobExecutionSummary = a} :: JobExecutionSummaryForJob)

instance Data.FromJSON JobExecutionSummaryForJob where
  parseJSON =
    Data.withObject
      "JobExecutionSummaryForJob"
      ( \x ->
          JobExecutionSummaryForJob'
            Prelude.<$> (x Data..:? "thingArn")
            Prelude.<*> (x Data..:? "jobExecutionSummary")
      )

instance Prelude.Hashable JobExecutionSummaryForJob where
  hashWithSalt _salt JobExecutionSummaryForJob' {..} =
    _salt `Prelude.hashWithSalt` thingArn
      `Prelude.hashWithSalt` jobExecutionSummary

instance Prelude.NFData JobExecutionSummaryForJob where
  rnf JobExecutionSummaryForJob' {..} =
    Prelude.rnf thingArn
      `Prelude.seq` Prelude.rnf jobExecutionSummary
