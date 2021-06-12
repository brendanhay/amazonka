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
-- Module      : Network.AWS.Batch.Types.JobDependency
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobDependency where

import Network.AWS.Batch.Types.ArrayJobDependency
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object representing an AWS Batch job dependency.
--
-- /See:/ 'newJobDependency' smart constructor.
data JobDependency = JobDependency'
  { -- | The type of the job dependency.
    type' :: Core.Maybe ArrayJobDependency,
    -- | The job ID of the AWS Batch job associated with this dependency.
    jobId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JobDependency' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'jobDependency_type' - The type of the job dependency.
--
-- 'jobId', 'jobDependency_jobId' - The job ID of the AWS Batch job associated with this dependency.
newJobDependency ::
  JobDependency
newJobDependency =
  JobDependency'
    { type' = Core.Nothing,
      jobId = Core.Nothing
    }

-- | The type of the job dependency.
jobDependency_type :: Lens.Lens' JobDependency (Core.Maybe ArrayJobDependency)
jobDependency_type = Lens.lens (\JobDependency' {type'} -> type') (\s@JobDependency' {} a -> s {type' = a} :: JobDependency)

-- | The job ID of the AWS Batch job associated with this dependency.
jobDependency_jobId :: Lens.Lens' JobDependency (Core.Maybe Core.Text)
jobDependency_jobId = Lens.lens (\JobDependency' {jobId} -> jobId) (\s@JobDependency' {} a -> s {jobId = a} :: JobDependency)

instance Core.FromJSON JobDependency where
  parseJSON =
    Core.withObject
      "JobDependency"
      ( \x ->
          JobDependency'
            Core.<$> (x Core..:? "type") Core.<*> (x Core..:? "jobId")
      )

instance Core.Hashable JobDependency

instance Core.NFData JobDependency

instance Core.ToJSON JobDependency where
  toJSON JobDependency' {..} =
    Core.object
      ( Core.catMaybes
          [ ("type" Core..=) Core.<$> type',
            ("jobId" Core..=) Core.<$> jobId
          ]
      )
