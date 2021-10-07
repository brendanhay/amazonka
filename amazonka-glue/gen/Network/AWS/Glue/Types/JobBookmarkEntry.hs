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
import qualified Network.AWS.Prelude as Prelude

-- | Defines a point that a job can resume processing.
--
-- /See:/ 'newJobBookmarkEntry' smart constructor.
data JobBookmarkEntry = JobBookmarkEntry'
  { -- | The run ID number.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The bookmark itself.
    jobBookmark :: Prelude.Maybe Prelude.Text,
    -- | The version of the job.
    version :: Prelude.Maybe Prelude.Int,
    -- | The run ID number.
    run :: Prelude.Maybe Prelude.Int,
    -- | The name of the job in question.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The attempt ID number.
    attempt :: Prelude.Maybe Prelude.Int,
    -- | The unique run identifier associated with the previous job run.
    previousRunId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'attempt', 'jobBookmarkEntry_attempt' - The attempt ID number.
--
-- 'previousRunId', 'jobBookmarkEntry_previousRunId' - The unique run identifier associated with the previous job run.
newJobBookmarkEntry ::
  JobBookmarkEntry
newJobBookmarkEntry =
  JobBookmarkEntry'
    { runId = Prelude.Nothing,
      jobBookmark = Prelude.Nothing,
      version = Prelude.Nothing,
      run = Prelude.Nothing,
      jobName = Prelude.Nothing,
      attempt = Prelude.Nothing,
      previousRunId = Prelude.Nothing
    }

-- | The run ID number.
jobBookmarkEntry_runId :: Lens.Lens' JobBookmarkEntry (Prelude.Maybe Prelude.Text)
jobBookmarkEntry_runId = Lens.lens (\JobBookmarkEntry' {runId} -> runId) (\s@JobBookmarkEntry' {} a -> s {runId = a} :: JobBookmarkEntry)

-- | The bookmark itself.
jobBookmarkEntry_jobBookmark :: Lens.Lens' JobBookmarkEntry (Prelude.Maybe Prelude.Text)
jobBookmarkEntry_jobBookmark = Lens.lens (\JobBookmarkEntry' {jobBookmark} -> jobBookmark) (\s@JobBookmarkEntry' {} a -> s {jobBookmark = a} :: JobBookmarkEntry)

-- | The version of the job.
jobBookmarkEntry_version :: Lens.Lens' JobBookmarkEntry (Prelude.Maybe Prelude.Int)
jobBookmarkEntry_version = Lens.lens (\JobBookmarkEntry' {version} -> version) (\s@JobBookmarkEntry' {} a -> s {version = a} :: JobBookmarkEntry)

-- | The run ID number.
jobBookmarkEntry_run :: Lens.Lens' JobBookmarkEntry (Prelude.Maybe Prelude.Int)
jobBookmarkEntry_run = Lens.lens (\JobBookmarkEntry' {run} -> run) (\s@JobBookmarkEntry' {} a -> s {run = a} :: JobBookmarkEntry)

-- | The name of the job in question.
jobBookmarkEntry_jobName :: Lens.Lens' JobBookmarkEntry (Prelude.Maybe Prelude.Text)
jobBookmarkEntry_jobName = Lens.lens (\JobBookmarkEntry' {jobName} -> jobName) (\s@JobBookmarkEntry' {} a -> s {jobName = a} :: JobBookmarkEntry)

-- | The attempt ID number.
jobBookmarkEntry_attempt :: Lens.Lens' JobBookmarkEntry (Prelude.Maybe Prelude.Int)
jobBookmarkEntry_attempt = Lens.lens (\JobBookmarkEntry' {attempt} -> attempt) (\s@JobBookmarkEntry' {} a -> s {attempt = a} :: JobBookmarkEntry)

-- | The unique run identifier associated with the previous job run.
jobBookmarkEntry_previousRunId :: Lens.Lens' JobBookmarkEntry (Prelude.Maybe Prelude.Text)
jobBookmarkEntry_previousRunId = Lens.lens (\JobBookmarkEntry' {previousRunId} -> previousRunId) (\s@JobBookmarkEntry' {} a -> s {previousRunId = a} :: JobBookmarkEntry)

instance Core.FromJSON JobBookmarkEntry where
  parseJSON =
    Core.withObject
      "JobBookmarkEntry"
      ( \x ->
          JobBookmarkEntry'
            Prelude.<$> (x Core..:? "RunId")
            Prelude.<*> (x Core..:? "JobBookmark")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "Run")
            Prelude.<*> (x Core..:? "JobName")
            Prelude.<*> (x Core..:? "Attempt")
            Prelude.<*> (x Core..:? "PreviousRunId")
      )

instance Prelude.Hashable JobBookmarkEntry

instance Prelude.NFData JobBookmarkEntry
