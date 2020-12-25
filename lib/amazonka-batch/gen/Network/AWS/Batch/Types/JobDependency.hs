{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobDependency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobDependency
  ( JobDependency (..),

    -- * Smart constructor
    mkJobDependency,

    -- * Lenses
    jJobId,
    jType,
  )
where

import qualified Network.AWS.Batch.Types.ArrayJobDependency as Types
import qualified Network.AWS.Batch.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing an AWS Batch job dependency.
--
-- /See:/ 'mkJobDependency' smart constructor.
data JobDependency = JobDependency'
  { -- | The job ID of the AWS Batch job associated with this dependency.
    jobId :: Core.Maybe Types.String,
    -- | The type of the job dependency.
    type' :: Core.Maybe Types.ArrayJobDependency
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobDependency' value with any optional fields omitted.
mkJobDependency ::
  JobDependency
mkJobDependency =
  JobDependency' {jobId = Core.Nothing, type' = Core.Nothing}

-- | The job ID of the AWS Batch job associated with this dependency.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jJobId :: Lens.Lens' JobDependency (Core.Maybe Types.String)
jJobId = Lens.field @"jobId"
{-# DEPRECATED jJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The type of the job dependency.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jType :: Lens.Lens' JobDependency (Core.Maybe Types.ArrayJobDependency)
jType = Lens.field @"type'"
{-# DEPRECATED jType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON JobDependency where
  toJSON JobDependency {..} =
    Core.object
      ( Core.catMaybes
          [ ("jobId" Core..=) Core.<$> jobId,
            ("type" Core..=) Core.<$> type'
          ]
      )

instance Core.FromJSON JobDependency where
  parseJSON =
    Core.withObject "JobDependency" Core.$
      \x ->
        JobDependency'
          Core.<$> (x Core..:? "jobId") Core.<*> (x Core..:? "type")
