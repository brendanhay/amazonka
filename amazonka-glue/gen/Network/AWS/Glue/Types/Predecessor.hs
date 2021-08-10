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
-- Module      : Network.AWS.Glue.Types.Predecessor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Predecessor where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A job run that was used in the predicate of a conditional trigger that
-- triggered this job run.
--
-- /See:/ 'newPredecessor' smart constructor.
data Predecessor = Predecessor'
  { -- | The job-run ID of the predecessor job run.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The name of the job definition used by the predecessor job run.
    jobName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Predecessor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runId', 'predecessor_runId' - The job-run ID of the predecessor job run.
--
-- 'jobName', 'predecessor_jobName' - The name of the job definition used by the predecessor job run.
newPredecessor ::
  Predecessor
newPredecessor =
  Predecessor'
    { runId = Prelude.Nothing,
      jobName = Prelude.Nothing
    }

-- | The job-run ID of the predecessor job run.
predecessor_runId :: Lens.Lens' Predecessor (Prelude.Maybe Prelude.Text)
predecessor_runId = Lens.lens (\Predecessor' {runId} -> runId) (\s@Predecessor' {} a -> s {runId = a} :: Predecessor)

-- | The name of the job definition used by the predecessor job run.
predecessor_jobName :: Lens.Lens' Predecessor (Prelude.Maybe Prelude.Text)
predecessor_jobName = Lens.lens (\Predecessor' {jobName} -> jobName) (\s@Predecessor' {} a -> s {jobName = a} :: Predecessor)

instance Core.FromJSON Predecessor where
  parseJSON =
    Core.withObject
      "Predecessor"
      ( \x ->
          Predecessor'
            Prelude.<$> (x Core..:? "RunId")
            Prelude.<*> (x Core..:? "JobName")
      )

instance Prelude.Hashable Predecessor

instance Prelude.NFData Predecessor
