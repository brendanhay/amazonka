{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.FindMatchesTaskRunProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.FindMatchesTaskRunProperties
  ( FindMatchesTaskRunProperties (..),

    -- * Smart constructor
    mkFindMatchesTaskRunProperties,

    -- * Lenses
    fmtrpJobId,
    fmtrpJobName,
    fmtrpJobRunId,
  )
where

import qualified Network.AWS.Glue.Types.HashString as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies configuration properties for a Find Matches task run.
--
-- /See:/ 'mkFindMatchesTaskRunProperties' smart constructor.
data FindMatchesTaskRunProperties = FindMatchesTaskRunProperties'
  { -- | The job ID for the Find Matches task run.
    jobId :: Core.Maybe Types.HashString,
    -- | The name assigned to the job for the Find Matches task run.
    jobName :: Core.Maybe Types.NameString,
    -- | The job run ID for the Find Matches task run.
    jobRunId :: Core.Maybe Types.HashString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FindMatchesTaskRunProperties' value with any optional fields omitted.
mkFindMatchesTaskRunProperties ::
  FindMatchesTaskRunProperties
mkFindMatchesTaskRunProperties =
  FindMatchesTaskRunProperties'
    { jobId = Core.Nothing,
      jobName = Core.Nothing,
      jobRunId = Core.Nothing
    }

-- | The job ID for the Find Matches task run.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmtrpJobId :: Lens.Lens' FindMatchesTaskRunProperties (Core.Maybe Types.HashString)
fmtrpJobId = Lens.field @"jobId"
{-# DEPRECATED fmtrpJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The name assigned to the job for the Find Matches task run.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmtrpJobName :: Lens.Lens' FindMatchesTaskRunProperties (Core.Maybe Types.NameString)
fmtrpJobName = Lens.field @"jobName"
{-# DEPRECATED fmtrpJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The job run ID for the Find Matches task run.
--
-- /Note:/ Consider using 'jobRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmtrpJobRunId :: Lens.Lens' FindMatchesTaskRunProperties (Core.Maybe Types.HashString)
fmtrpJobRunId = Lens.field @"jobRunId"
{-# DEPRECATED fmtrpJobRunId "Use generic-lens or generic-optics with 'jobRunId' instead." #-}

instance Core.FromJSON FindMatchesTaskRunProperties where
  parseJSON =
    Core.withObject "FindMatchesTaskRunProperties" Core.$
      \x ->
        FindMatchesTaskRunProperties'
          Core.<$> (x Core..:? "JobId")
          Core.<*> (x Core..:? "JobName")
          Core.<*> (x Core..:? "JobRunId")
