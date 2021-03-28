{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobExecutionSummaryForJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.JobExecutionSummaryForJob
  ( JobExecutionSummaryForJob (..)
  -- * Smart constructor
  , mkJobExecutionSummaryForJob
  -- * Lenses
  , jesfjJobExecutionSummary
  , jesfjThingArn
  ) where

import qualified Network.AWS.IoT.Types.JobExecutionSummary as Types
import qualified Network.AWS.IoT.Types.ThingArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains a summary of information about job executions for a specific job.
--
-- /See:/ 'mkJobExecutionSummaryForJob' smart constructor.
data JobExecutionSummaryForJob = JobExecutionSummaryForJob'
  { jobExecutionSummary :: Core.Maybe Types.JobExecutionSummary
    -- ^ Contains a subset of information about a job execution.
  , thingArn :: Core.Maybe Types.ThingArn
    -- ^ The ARN of the thing on which the job execution is running.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'JobExecutionSummaryForJob' value with any optional fields omitted.
mkJobExecutionSummaryForJob
    :: JobExecutionSummaryForJob
mkJobExecutionSummaryForJob
  = JobExecutionSummaryForJob'{jobExecutionSummary = Core.Nothing,
                               thingArn = Core.Nothing}

-- | Contains a subset of information about a job execution.
--
-- /Note:/ Consider using 'jobExecutionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesfjJobExecutionSummary :: Lens.Lens' JobExecutionSummaryForJob (Core.Maybe Types.JobExecutionSummary)
jesfjJobExecutionSummary = Lens.field @"jobExecutionSummary"
{-# INLINEABLE jesfjJobExecutionSummary #-}
{-# DEPRECATED jobExecutionSummary "Use generic-lens or generic-optics with 'jobExecutionSummary' instead"  #-}

-- | The ARN of the thing on which the job execution is running.
--
-- /Note:/ Consider using 'thingArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesfjThingArn :: Lens.Lens' JobExecutionSummaryForJob (Core.Maybe Types.ThingArn)
jesfjThingArn = Lens.field @"thingArn"
{-# INLINEABLE jesfjThingArn #-}
{-# DEPRECATED thingArn "Use generic-lens or generic-optics with 'thingArn' instead"  #-}

instance Core.FromJSON JobExecutionSummaryForJob where
        parseJSON
          = Core.withObject "JobExecutionSummaryForJob" Core.$
              \ x ->
                JobExecutionSummaryForJob' Core.<$>
                  (x Core..:? "jobExecutionSummary") Core.<*> x Core..:? "thingArn"
