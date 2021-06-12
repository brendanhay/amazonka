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
-- Module      : Network.AWS.Glue.Types.JobNodeDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JobNodeDetails where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.JobRun
import qualified Network.AWS.Lens as Lens

-- | The details of a Job node present in the workflow.
--
-- /See:/ 'newJobNodeDetails' smart constructor.
data JobNodeDetails = JobNodeDetails'
  { -- | The information for the job runs represented by the job node.
    jobRuns :: Core.Maybe [JobRun]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JobNodeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobRuns', 'jobNodeDetails_jobRuns' - The information for the job runs represented by the job node.
newJobNodeDetails ::
  JobNodeDetails
newJobNodeDetails =
  JobNodeDetails' {jobRuns = Core.Nothing}

-- | The information for the job runs represented by the job node.
jobNodeDetails_jobRuns :: Lens.Lens' JobNodeDetails (Core.Maybe [JobRun])
jobNodeDetails_jobRuns = Lens.lens (\JobNodeDetails' {jobRuns} -> jobRuns) (\s@JobNodeDetails' {} a -> s {jobRuns = a} :: JobNodeDetails) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON JobNodeDetails where
  parseJSON =
    Core.withObject
      "JobNodeDetails"
      ( \x ->
          JobNodeDetails'
            Core.<$> (x Core..:? "JobRuns" Core..!= Core.mempty)
      )

instance Core.Hashable JobNodeDetails

instance Core.NFData JobNodeDetails
