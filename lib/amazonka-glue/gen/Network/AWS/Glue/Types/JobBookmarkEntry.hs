{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JobBookmarkEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JobBookmarkEntry
  ( JobBookmarkEntry (..),

    -- * Smart constructor
    mkJobBookmarkEntry,

    -- * Lenses
    jbeAttempt,
    jbeJobBookmark,
    jbeJobName,
    jbePreviousRunId,
    jbeRun,
    jbeRunId,
    jbeVersion,
  )
where

import qualified Network.AWS.Glue.Types.JobName as Types
import qualified Network.AWS.Glue.Types.JsonValue as Types
import qualified Network.AWS.Glue.Types.RunId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Defines a point that a job can resume processing.
--
-- /See:/ 'mkJobBookmarkEntry' smart constructor.
data JobBookmarkEntry = JobBookmarkEntry'
  { -- | The attempt ID number.
    attempt :: Core.Maybe Core.Int,
    -- | The bookmark itself.
    jobBookmark :: Core.Maybe Types.JsonValue,
    -- | The name of the job in question.
    jobName :: Core.Maybe Types.JobName,
    -- | The unique run identifier associated with the previous job run.
    previousRunId :: Core.Maybe Types.RunId,
    -- | The run ID number.
    run :: Core.Maybe Core.Int,
    -- | The run ID number.
    runId :: Core.Maybe Types.RunId,
    -- | The version of the job.
    version :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobBookmarkEntry' value with any optional fields omitted.
mkJobBookmarkEntry ::
  JobBookmarkEntry
mkJobBookmarkEntry =
  JobBookmarkEntry'
    { attempt = Core.Nothing,
      jobBookmark = Core.Nothing,
      jobName = Core.Nothing,
      previousRunId = Core.Nothing,
      run = Core.Nothing,
      runId = Core.Nothing,
      version = Core.Nothing
    }

-- | The attempt ID number.
--
-- /Note:/ Consider using 'attempt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jbeAttempt :: Lens.Lens' JobBookmarkEntry (Core.Maybe Core.Int)
jbeAttempt = Lens.field @"attempt"
{-# DEPRECATED jbeAttempt "Use generic-lens or generic-optics with 'attempt' instead." #-}

-- | The bookmark itself.
--
-- /Note:/ Consider using 'jobBookmark' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jbeJobBookmark :: Lens.Lens' JobBookmarkEntry (Core.Maybe Types.JsonValue)
jbeJobBookmark = Lens.field @"jobBookmark"
{-# DEPRECATED jbeJobBookmark "Use generic-lens or generic-optics with 'jobBookmark' instead." #-}

-- | The name of the job in question.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jbeJobName :: Lens.Lens' JobBookmarkEntry (Core.Maybe Types.JobName)
jbeJobName = Lens.field @"jobName"
{-# DEPRECATED jbeJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The unique run identifier associated with the previous job run.
--
-- /Note:/ Consider using 'previousRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jbePreviousRunId :: Lens.Lens' JobBookmarkEntry (Core.Maybe Types.RunId)
jbePreviousRunId = Lens.field @"previousRunId"
{-# DEPRECATED jbePreviousRunId "Use generic-lens or generic-optics with 'previousRunId' instead." #-}

-- | The run ID number.
--
-- /Note:/ Consider using 'run' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jbeRun :: Lens.Lens' JobBookmarkEntry (Core.Maybe Core.Int)
jbeRun = Lens.field @"run"
{-# DEPRECATED jbeRun "Use generic-lens or generic-optics with 'run' instead." #-}

-- | The run ID number.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jbeRunId :: Lens.Lens' JobBookmarkEntry (Core.Maybe Types.RunId)
jbeRunId = Lens.field @"runId"
{-# DEPRECATED jbeRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

-- | The version of the job.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jbeVersion :: Lens.Lens' JobBookmarkEntry (Core.Maybe Core.Int)
jbeVersion = Lens.field @"version"
{-# DEPRECATED jbeVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON JobBookmarkEntry where
  parseJSON =
    Core.withObject "JobBookmarkEntry" Core.$
      \x ->
        JobBookmarkEntry'
          Core.<$> (x Core..:? "Attempt")
          Core.<*> (x Core..:? "JobBookmark")
          Core.<*> (x Core..:? "JobName")
          Core.<*> (x Core..:? "PreviousRunId")
          Core.<*> (x Core..:? "Run")
          Core.<*> (x Core..:? "RunId")
          Core.<*> (x Core..:? "Version")
