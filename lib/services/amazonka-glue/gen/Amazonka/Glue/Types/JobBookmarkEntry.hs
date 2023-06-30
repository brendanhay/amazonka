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
-- Module      : Amazonka.Glue.Types.JobBookmarkEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.JobBookmarkEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines a point that a job can resume processing.
--
-- /See:/ 'newJobBookmarkEntry' smart constructor.
data JobBookmarkEntry = JobBookmarkEntry'
  { -- | The attempt ID number.
    attempt :: Prelude.Maybe Prelude.Int,
    -- | The bookmark itself.
    jobBookmark :: Prelude.Maybe Prelude.Text,
    -- | The name of the job in question.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The unique run identifier associated with the previous job run.
    previousRunId :: Prelude.Maybe Prelude.Text,
    -- | The run ID number.
    run :: Prelude.Maybe Prelude.Int,
    -- | The run ID number.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The version of the job.
    version :: Prelude.Maybe Prelude.Int
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
-- 'attempt', 'jobBookmarkEntry_attempt' - The attempt ID number.
--
-- 'jobBookmark', 'jobBookmarkEntry_jobBookmark' - The bookmark itself.
--
-- 'jobName', 'jobBookmarkEntry_jobName' - The name of the job in question.
--
-- 'previousRunId', 'jobBookmarkEntry_previousRunId' - The unique run identifier associated with the previous job run.
--
-- 'run', 'jobBookmarkEntry_run' - The run ID number.
--
-- 'runId', 'jobBookmarkEntry_runId' - The run ID number.
--
-- 'version', 'jobBookmarkEntry_version' - The version of the job.
newJobBookmarkEntry ::
  JobBookmarkEntry
newJobBookmarkEntry =
  JobBookmarkEntry'
    { attempt = Prelude.Nothing,
      jobBookmark = Prelude.Nothing,
      jobName = Prelude.Nothing,
      previousRunId = Prelude.Nothing,
      run = Prelude.Nothing,
      runId = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The attempt ID number.
jobBookmarkEntry_attempt :: Lens.Lens' JobBookmarkEntry (Prelude.Maybe Prelude.Int)
jobBookmarkEntry_attempt = Lens.lens (\JobBookmarkEntry' {attempt} -> attempt) (\s@JobBookmarkEntry' {} a -> s {attempt = a} :: JobBookmarkEntry)

-- | The bookmark itself.
jobBookmarkEntry_jobBookmark :: Lens.Lens' JobBookmarkEntry (Prelude.Maybe Prelude.Text)
jobBookmarkEntry_jobBookmark = Lens.lens (\JobBookmarkEntry' {jobBookmark} -> jobBookmark) (\s@JobBookmarkEntry' {} a -> s {jobBookmark = a} :: JobBookmarkEntry)

-- | The name of the job in question.
jobBookmarkEntry_jobName :: Lens.Lens' JobBookmarkEntry (Prelude.Maybe Prelude.Text)
jobBookmarkEntry_jobName = Lens.lens (\JobBookmarkEntry' {jobName} -> jobName) (\s@JobBookmarkEntry' {} a -> s {jobName = a} :: JobBookmarkEntry)

-- | The unique run identifier associated with the previous job run.
jobBookmarkEntry_previousRunId :: Lens.Lens' JobBookmarkEntry (Prelude.Maybe Prelude.Text)
jobBookmarkEntry_previousRunId = Lens.lens (\JobBookmarkEntry' {previousRunId} -> previousRunId) (\s@JobBookmarkEntry' {} a -> s {previousRunId = a} :: JobBookmarkEntry)

-- | The run ID number.
jobBookmarkEntry_run :: Lens.Lens' JobBookmarkEntry (Prelude.Maybe Prelude.Int)
jobBookmarkEntry_run = Lens.lens (\JobBookmarkEntry' {run} -> run) (\s@JobBookmarkEntry' {} a -> s {run = a} :: JobBookmarkEntry)

-- | The run ID number.
jobBookmarkEntry_runId :: Lens.Lens' JobBookmarkEntry (Prelude.Maybe Prelude.Text)
jobBookmarkEntry_runId = Lens.lens (\JobBookmarkEntry' {runId} -> runId) (\s@JobBookmarkEntry' {} a -> s {runId = a} :: JobBookmarkEntry)

-- | The version of the job.
jobBookmarkEntry_version :: Lens.Lens' JobBookmarkEntry (Prelude.Maybe Prelude.Int)
jobBookmarkEntry_version = Lens.lens (\JobBookmarkEntry' {version} -> version) (\s@JobBookmarkEntry' {} a -> s {version = a} :: JobBookmarkEntry)

instance Data.FromJSON JobBookmarkEntry where
  parseJSON =
    Data.withObject
      "JobBookmarkEntry"
      ( \x ->
          JobBookmarkEntry'
            Prelude.<$> (x Data..:? "Attempt")
            Prelude.<*> (x Data..:? "JobBookmark")
            Prelude.<*> (x Data..:? "JobName")
            Prelude.<*> (x Data..:? "PreviousRunId")
            Prelude.<*> (x Data..:? "Run")
            Prelude.<*> (x Data..:? "RunId")
            Prelude.<*> (x Data..:? "Version")
      )

instance Prelude.Hashable JobBookmarkEntry where
  hashWithSalt _salt JobBookmarkEntry' {..} =
    _salt
      `Prelude.hashWithSalt` attempt
      `Prelude.hashWithSalt` jobBookmark
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` previousRunId
      `Prelude.hashWithSalt` run
      `Prelude.hashWithSalt` runId
      `Prelude.hashWithSalt` version

instance Prelude.NFData JobBookmarkEntry where
  rnf JobBookmarkEntry' {..} =
    Prelude.rnf attempt
      `Prelude.seq` Prelude.rnf jobBookmark
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf previousRunId
      `Prelude.seq` Prelude.rnf run
      `Prelude.seq` Prelude.rnf runId
      `Prelude.seq` Prelude.rnf version
