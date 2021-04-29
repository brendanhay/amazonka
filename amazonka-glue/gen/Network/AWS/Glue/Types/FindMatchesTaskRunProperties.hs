{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies configuration properties for a Find Matches task run.
--
-- /See:/ 'newFindMatchesTaskRunProperties' smart constructor.
data FindMatchesTaskRunProperties = FindMatchesTaskRunProperties'
  { -- | The job run ID for the Find Matches task run.
    jobRunId :: Prelude.Maybe Prelude.Text,
    -- | The name assigned to the job for the Find Matches task run.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The job ID for the Find Matches task run.
    jobId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      jobName = Prelude.Nothing,
      jobId = Prelude.Nothing
    }

-- | The job run ID for the Find Matches task run.
findMatchesTaskRunProperties_jobRunId :: Lens.Lens' FindMatchesTaskRunProperties (Prelude.Maybe Prelude.Text)
findMatchesTaskRunProperties_jobRunId = Lens.lens (\FindMatchesTaskRunProperties' {jobRunId} -> jobRunId) (\s@FindMatchesTaskRunProperties' {} a -> s {jobRunId = a} :: FindMatchesTaskRunProperties)

-- | The name assigned to the job for the Find Matches task run.
findMatchesTaskRunProperties_jobName :: Lens.Lens' FindMatchesTaskRunProperties (Prelude.Maybe Prelude.Text)
findMatchesTaskRunProperties_jobName = Lens.lens (\FindMatchesTaskRunProperties' {jobName} -> jobName) (\s@FindMatchesTaskRunProperties' {} a -> s {jobName = a} :: FindMatchesTaskRunProperties)

-- | The job ID for the Find Matches task run.
findMatchesTaskRunProperties_jobId :: Lens.Lens' FindMatchesTaskRunProperties (Prelude.Maybe Prelude.Text)
findMatchesTaskRunProperties_jobId = Lens.lens (\FindMatchesTaskRunProperties' {jobId} -> jobId) (\s@FindMatchesTaskRunProperties' {} a -> s {jobId = a} :: FindMatchesTaskRunProperties)

instance
  Prelude.FromJSON
    FindMatchesTaskRunProperties
  where
  parseJSON =
    Prelude.withObject
      "FindMatchesTaskRunProperties"
      ( \x ->
          FindMatchesTaskRunProperties'
            Prelude.<$> (x Prelude..:? "JobRunId")
            Prelude.<*> (x Prelude..:? "JobName")
            Prelude.<*> (x Prelude..:? "JobId")
      )

instance
  Prelude.Hashable
    FindMatchesTaskRunProperties

instance Prelude.NFData FindMatchesTaskRunProperties
