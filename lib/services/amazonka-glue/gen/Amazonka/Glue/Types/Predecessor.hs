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
-- Module      : Amazonka.Glue.Types.Predecessor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Predecessor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A job run that was used in the predicate of a conditional trigger that
-- triggered this job run.
--
-- /See:/ 'newPredecessor' smart constructor.
data Predecessor = Predecessor'
  { -- | The name of the job definition used by the predecessor job run.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The job-run ID of the predecessor job run.
    runId :: Prelude.Maybe Prelude.Text
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
-- 'jobName', 'predecessor_jobName' - The name of the job definition used by the predecessor job run.
--
-- 'runId', 'predecessor_runId' - The job-run ID of the predecessor job run.
newPredecessor ::
  Predecessor
newPredecessor =
  Predecessor'
    { jobName = Prelude.Nothing,
      runId = Prelude.Nothing
    }

-- | The name of the job definition used by the predecessor job run.
predecessor_jobName :: Lens.Lens' Predecessor (Prelude.Maybe Prelude.Text)
predecessor_jobName = Lens.lens (\Predecessor' {jobName} -> jobName) (\s@Predecessor' {} a -> s {jobName = a} :: Predecessor)

-- | The job-run ID of the predecessor job run.
predecessor_runId :: Lens.Lens' Predecessor (Prelude.Maybe Prelude.Text)
predecessor_runId = Lens.lens (\Predecessor' {runId} -> runId) (\s@Predecessor' {} a -> s {runId = a} :: Predecessor)

instance Data.FromJSON Predecessor where
  parseJSON =
    Data.withObject
      "Predecessor"
      ( \x ->
          Predecessor'
            Prelude.<$> (x Data..:? "JobName")
            Prelude.<*> (x Data..:? "RunId")
      )

instance Prelude.Hashable Predecessor where
  hashWithSalt _salt Predecessor' {..} =
    _salt
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` runId

instance Prelude.NFData Predecessor where
  rnf Predecessor' {..} =
    Prelude.rnf jobName `Prelude.seq` Prelude.rnf runId
