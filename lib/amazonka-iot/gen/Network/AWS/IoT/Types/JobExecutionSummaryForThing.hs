{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobExecutionSummaryForThing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.JobExecutionSummaryForThing
  ( JobExecutionSummaryForThing (..)
  -- * Smart constructor
  , mkJobExecutionSummaryForThing
  -- * Lenses
  , jesftJobExecutionSummary
  , jesftJobId
  ) where

import qualified Network.AWS.IoT.Types.JobExecutionSummary as Types
import qualified Network.AWS.IoT.Types.JobId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The job execution summary for a thing.
--
-- /See:/ 'mkJobExecutionSummaryForThing' smart constructor.
data JobExecutionSummaryForThing = JobExecutionSummaryForThing'
  { jobExecutionSummary :: Core.Maybe Types.JobExecutionSummary
    -- ^ Contains a subset of information about a job execution.
  , jobId :: Core.Maybe Types.JobId
    -- ^ The unique identifier you assigned to this job when it was created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'JobExecutionSummaryForThing' value with any optional fields omitted.
mkJobExecutionSummaryForThing
    :: JobExecutionSummaryForThing
mkJobExecutionSummaryForThing
  = JobExecutionSummaryForThing'{jobExecutionSummary = Core.Nothing,
                                 jobId = Core.Nothing}

-- | Contains a subset of information about a job execution.
--
-- /Note:/ Consider using 'jobExecutionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesftJobExecutionSummary :: Lens.Lens' JobExecutionSummaryForThing (Core.Maybe Types.JobExecutionSummary)
jesftJobExecutionSummary = Lens.field @"jobExecutionSummary"
{-# INLINEABLE jesftJobExecutionSummary #-}
{-# DEPRECATED jobExecutionSummary "Use generic-lens or generic-optics with 'jobExecutionSummary' instead"  #-}

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesftJobId :: Lens.Lens' JobExecutionSummaryForThing (Core.Maybe Types.JobId)
jesftJobId = Lens.field @"jobId"
{-# INLINEABLE jesftJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

instance Core.FromJSON JobExecutionSummaryForThing where
        parseJSON
          = Core.withObject "JobExecutionSummaryForThing" Core.$
              \ x ->
                JobExecutionSummaryForThing' Core.<$>
                  (x Core..:? "jobExecutionSummary") Core.<*> x Core..:? "jobId"
