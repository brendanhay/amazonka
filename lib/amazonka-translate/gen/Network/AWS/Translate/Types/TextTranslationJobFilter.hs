{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.TextTranslationJobFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Translate.Types.TextTranslationJobFilter
  ( TextTranslationJobFilter (..)
  -- * Smart constructor
  , mkTextTranslationJobFilter
  -- * Lenses
  , ttjfJobName
  , ttjfJobStatus
  , ttjfSubmittedAfterTime
  , ttjfSubmittedBeforeTime
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Translate.Types.JobName as Types
import qualified Network.AWS.Translate.Types.JobStatus as Types

-- | Provides information for filtering a list of translation jobs. For more information, see 'ListTextTranslationJobs' .
--
-- /See:/ 'mkTextTranslationJobFilter' smart constructor.
data TextTranslationJobFilter = TextTranslationJobFilter'
  { jobName :: Core.Maybe Types.JobName
    -- ^ Filters the list of jobs by name.
  , jobStatus :: Core.Maybe Types.JobStatus
    -- ^ Filters the list of jobs based by job status.
  , submittedAfterTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Filters the list of jobs based on the time that the job was submitted for processing and returns only the jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
  , submittedBeforeTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Filters the list of jobs based on the time that the job was submitted for processing and returns only the jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TextTranslationJobFilter' value with any optional fields omitted.
mkTextTranslationJobFilter
    :: TextTranslationJobFilter
mkTextTranslationJobFilter
  = TextTranslationJobFilter'{jobName = Core.Nothing,
                              jobStatus = Core.Nothing, submittedAfterTime = Core.Nothing,
                              submittedBeforeTime = Core.Nothing}

-- | Filters the list of jobs by name.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttjfJobName :: Lens.Lens' TextTranslationJobFilter (Core.Maybe Types.JobName)
ttjfJobName = Lens.field @"jobName"
{-# INLINEABLE ttjfJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | Filters the list of jobs based by job status.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttjfJobStatus :: Lens.Lens' TextTranslationJobFilter (Core.Maybe Types.JobStatus)
ttjfJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE ttjfJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | Filters the list of jobs based on the time that the job was submitted for processing and returns only the jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
--
-- /Note:/ Consider using 'submittedAfterTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttjfSubmittedAfterTime :: Lens.Lens' TextTranslationJobFilter (Core.Maybe Core.NominalDiffTime)
ttjfSubmittedAfterTime = Lens.field @"submittedAfterTime"
{-# INLINEABLE ttjfSubmittedAfterTime #-}
{-# DEPRECATED submittedAfterTime "Use generic-lens or generic-optics with 'submittedAfterTime' instead"  #-}

-- | Filters the list of jobs based on the time that the job was submitted for processing and returns only the jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- /Note:/ Consider using 'submittedBeforeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttjfSubmittedBeforeTime :: Lens.Lens' TextTranslationJobFilter (Core.Maybe Core.NominalDiffTime)
ttjfSubmittedBeforeTime = Lens.field @"submittedBeforeTime"
{-# INLINEABLE ttjfSubmittedBeforeTime #-}
{-# DEPRECATED submittedBeforeTime "Use generic-lens or generic-optics with 'submittedBeforeTime' instead"  #-}

instance Core.FromJSON TextTranslationJobFilter where
        toJSON TextTranslationJobFilter{..}
          = Core.object
              (Core.catMaybes
                 [("JobName" Core..=) Core.<$> jobName,
                  ("JobStatus" Core..=) Core.<$> jobStatus,
                  ("SubmittedAfterTime" Core..=) Core.<$> submittedAfterTime,
                  ("SubmittedBeforeTime" Core..=) Core.<$> submittedBeforeTime])
