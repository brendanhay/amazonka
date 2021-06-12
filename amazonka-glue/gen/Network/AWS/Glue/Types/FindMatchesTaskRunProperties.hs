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
-- Module      : Network.AWS.Glue.Types.FindMatchesTaskRunProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.FindMatchesTaskRunProperties where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies configuration properties for a Find Matches task run.
--
-- /See:/ 'newFindMatchesTaskRunProperties' smart constructor.
data FindMatchesTaskRunProperties = FindMatchesTaskRunProperties'
  { -- | The job run ID for the Find Matches task run.
    jobRunId :: Core.Maybe Core.Text,
    -- | The name assigned to the job for the Find Matches task run.
    jobName :: Core.Maybe Core.Text,
    -- | The job ID for the Find Matches task run.
    jobId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FindMatchesTaskRunProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobRunId', 'findMatchesTaskRunProperties_jobRunId' - The job run ID for the Find Matches task run.
--
-- 'jobName', 'findMatchesTaskRunProperties_jobName' - The name assigned to the job for the Find Matches task run.
--
-- 'jobId', 'findMatchesTaskRunProperties_jobId' - The job ID for the Find Matches task run.
newFindMatchesTaskRunProperties ::
  FindMatchesTaskRunProperties
newFindMatchesTaskRunProperties =
  FindMatchesTaskRunProperties'
    { jobRunId =
        Core.Nothing,
      jobName = Core.Nothing,
      jobId = Core.Nothing
    }

-- | The job run ID for the Find Matches task run.
findMatchesTaskRunProperties_jobRunId :: Lens.Lens' FindMatchesTaskRunProperties (Core.Maybe Core.Text)
findMatchesTaskRunProperties_jobRunId = Lens.lens (\FindMatchesTaskRunProperties' {jobRunId} -> jobRunId) (\s@FindMatchesTaskRunProperties' {} a -> s {jobRunId = a} :: FindMatchesTaskRunProperties)

-- | The name assigned to the job for the Find Matches task run.
findMatchesTaskRunProperties_jobName :: Lens.Lens' FindMatchesTaskRunProperties (Core.Maybe Core.Text)
findMatchesTaskRunProperties_jobName = Lens.lens (\FindMatchesTaskRunProperties' {jobName} -> jobName) (\s@FindMatchesTaskRunProperties' {} a -> s {jobName = a} :: FindMatchesTaskRunProperties)

-- | The job ID for the Find Matches task run.
findMatchesTaskRunProperties_jobId :: Lens.Lens' FindMatchesTaskRunProperties (Core.Maybe Core.Text)
findMatchesTaskRunProperties_jobId = Lens.lens (\FindMatchesTaskRunProperties' {jobId} -> jobId) (\s@FindMatchesTaskRunProperties' {} a -> s {jobId = a} :: FindMatchesTaskRunProperties)

instance Core.FromJSON FindMatchesTaskRunProperties where
  parseJSON =
    Core.withObject
      "FindMatchesTaskRunProperties"
      ( \x ->
          FindMatchesTaskRunProperties'
            Core.<$> (x Core..:? "JobRunId")
            Core.<*> (x Core..:? "JobName")
            Core.<*> (x Core..:? "JobId")
      )

instance Core.Hashable FindMatchesTaskRunProperties

instance Core.NFData FindMatchesTaskRunProperties
