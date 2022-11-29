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
-- Module      : Amazonka.Glue.Types.FindMatchesTaskRunProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.FindMatchesTaskRunProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies configuration properties for a Find Matches task run.
--
-- /See:/ 'newFindMatchesTaskRunProperties' smart constructor.
data FindMatchesTaskRunProperties = FindMatchesTaskRunProperties'
  { -- | The name assigned to the job for the Find Matches task run.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The job run ID for the Find Matches task run.
    jobRunId :: Prelude.Maybe Prelude.Text,
    -- | The job ID for the Find Matches task run.
    jobId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FindMatchesTaskRunProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'findMatchesTaskRunProperties_jobName' - The name assigned to the job for the Find Matches task run.
--
-- 'jobRunId', 'findMatchesTaskRunProperties_jobRunId' - The job run ID for the Find Matches task run.
--
-- 'jobId', 'findMatchesTaskRunProperties_jobId' - The job ID for the Find Matches task run.
newFindMatchesTaskRunProperties ::
  FindMatchesTaskRunProperties
newFindMatchesTaskRunProperties =
  FindMatchesTaskRunProperties'
    { jobName =
        Prelude.Nothing,
      jobRunId = Prelude.Nothing,
      jobId = Prelude.Nothing
    }

-- | The name assigned to the job for the Find Matches task run.
findMatchesTaskRunProperties_jobName :: Lens.Lens' FindMatchesTaskRunProperties (Prelude.Maybe Prelude.Text)
findMatchesTaskRunProperties_jobName = Lens.lens (\FindMatchesTaskRunProperties' {jobName} -> jobName) (\s@FindMatchesTaskRunProperties' {} a -> s {jobName = a} :: FindMatchesTaskRunProperties)

-- | The job run ID for the Find Matches task run.
findMatchesTaskRunProperties_jobRunId :: Lens.Lens' FindMatchesTaskRunProperties (Prelude.Maybe Prelude.Text)
findMatchesTaskRunProperties_jobRunId = Lens.lens (\FindMatchesTaskRunProperties' {jobRunId} -> jobRunId) (\s@FindMatchesTaskRunProperties' {} a -> s {jobRunId = a} :: FindMatchesTaskRunProperties)

-- | The job ID for the Find Matches task run.
findMatchesTaskRunProperties_jobId :: Lens.Lens' FindMatchesTaskRunProperties (Prelude.Maybe Prelude.Text)
findMatchesTaskRunProperties_jobId = Lens.lens (\FindMatchesTaskRunProperties' {jobId} -> jobId) (\s@FindMatchesTaskRunProperties' {} a -> s {jobId = a} :: FindMatchesTaskRunProperties)

instance Core.FromJSON FindMatchesTaskRunProperties where
  parseJSON =
    Core.withObject
      "FindMatchesTaskRunProperties"
      ( \x ->
          FindMatchesTaskRunProperties'
            Prelude.<$> (x Core..:? "JobName")
            Prelude.<*> (x Core..:? "JobRunId")
            Prelude.<*> (x Core..:? "JobId")
      )

instance
  Prelude.Hashable
    FindMatchesTaskRunProperties
  where
  hashWithSalt _salt FindMatchesTaskRunProperties' {..} =
    _salt `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` jobRunId
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData FindMatchesTaskRunProperties where
  rnf FindMatchesTaskRunProperties' {..} =
    Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobRunId
      `Prelude.seq` Prelude.rnf jobId
