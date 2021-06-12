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
-- Module      : Network.AWS.Glue.Types.JobBookmarkEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JobBookmarkEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Defines a point that a job can resume processing.
--
-- /See:/ 'newJobBookmarkEntry' smart constructor.
data JobBookmarkEntry = JobBookmarkEntry'
  { -- | The run ID number.
    runId :: Core.Maybe Core.Text,
    -- | The bookmark itself.
    jobBookmark :: Core.Maybe Core.Text,
    -- | The version of the job.
    version :: Core.Maybe Core.Int,
    -- | The run ID number.
    run :: Core.Maybe Core.Int,
    -- | The name of the job in question.
    jobName :: Core.Maybe Core.Text,
    -- | The unique run identifier associated with the previous job run.
    previousRunId :: Core.Maybe Core.Text,
    -- | The attempt ID number.
    attempt :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JobBookmarkEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runId', 'jobBookmarkEntry_runId' - The run ID number.
--
-- 'jobBookmark', 'jobBookmarkEntry_jobBookmark' - The bookmark itself.
--
-- 'version', 'jobBookmarkEntry_version' - The version of the job.
--
-- 'run', 'jobBookmarkEntry_run' - The run ID number.
--
-- 'jobName', 'jobBookmarkEntry_jobName' - The name of the job in question.
--
-- 'previousRunId', 'jobBookmarkEntry_previousRunId' - The unique run identifier associated with the previous job run.
--
-- 'attempt', 'jobBookmarkEntry_attempt' - The attempt ID number.
newJobBookmarkEntry ::
  JobBookmarkEntry
newJobBookmarkEntry =
  JobBookmarkEntry'
    { runId = Core.Nothing,
      jobBookmark = Core.Nothing,
      version = Core.Nothing,
      run = Core.Nothing,
      jobName = Core.Nothing,
      previousRunId = Core.Nothing,
      attempt = Core.Nothing
    }

-- | The run ID number.
jobBookmarkEntry_runId :: Lens.Lens' JobBookmarkEntry (Core.Maybe Core.Text)
jobBookmarkEntry_runId = Lens.lens (\JobBookmarkEntry' {runId} -> runId) (\s@JobBookmarkEntry' {} a -> s {runId = a} :: JobBookmarkEntry)

-- | The bookmark itself.
jobBookmarkEntry_jobBookmark :: Lens.Lens' JobBookmarkEntry (Core.Maybe Core.Text)
jobBookmarkEntry_jobBookmark = Lens.lens (\JobBookmarkEntry' {jobBookmark} -> jobBookmark) (\s@JobBookmarkEntry' {} a -> s {jobBookmark = a} :: JobBookmarkEntry)

-- | The version of the job.
jobBookmarkEntry_version :: Lens.Lens' JobBookmarkEntry (Core.Maybe Core.Int)
jobBookmarkEntry_version = Lens.lens (\JobBookmarkEntry' {version} -> version) (\s@JobBookmarkEntry' {} a -> s {version = a} :: JobBookmarkEntry)

-- | The run ID number.
jobBookmarkEntry_run :: Lens.Lens' JobBookmarkEntry (Core.Maybe Core.Int)
jobBookmarkEntry_run = Lens.lens (\JobBookmarkEntry' {run} -> run) (\s@JobBookmarkEntry' {} a -> s {run = a} :: JobBookmarkEntry)

-- | The name of the job in question.
jobBookmarkEntry_jobName :: Lens.Lens' JobBookmarkEntry (Core.Maybe Core.Text)
jobBookmarkEntry_jobName = Lens.lens (\JobBookmarkEntry' {jobName} -> jobName) (\s@JobBookmarkEntry' {} a -> s {jobName = a} :: JobBookmarkEntry)

-- | The unique run identifier associated with the previous job run.
jobBookmarkEntry_previousRunId :: Lens.Lens' JobBookmarkEntry (Core.Maybe Core.Text)
jobBookmarkEntry_previousRunId = Lens.lens (\JobBookmarkEntry' {previousRunId} -> previousRunId) (\s@JobBookmarkEntry' {} a -> s {previousRunId = a} :: JobBookmarkEntry)

-- | The attempt ID number.
jobBookmarkEntry_attempt :: Lens.Lens' JobBookmarkEntry (Core.Maybe Core.Int)
jobBookmarkEntry_attempt = Lens.lens (\JobBookmarkEntry' {attempt} -> attempt) (\s@JobBookmarkEntry' {} a -> s {attempt = a} :: JobBookmarkEntry)

instance Core.FromJSON JobBookmarkEntry where
  parseJSON =
    Core.withObject
      "JobBookmarkEntry"
      ( \x ->
          JobBookmarkEntry'
            Core.<$> (x Core..:? "RunId")
            Core.<*> (x Core..:? "JobBookmark")
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "Run")
            Core.<*> (x Core..:? "JobName")
            Core.<*> (x Core..:? "PreviousRunId")
            Core.<*> (x Core..:? "Attempt")
      )

instance Core.Hashable JobBookmarkEntry

instance Core.NFData JobBookmarkEntry
