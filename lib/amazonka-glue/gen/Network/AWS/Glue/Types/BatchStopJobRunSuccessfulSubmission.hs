{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.BatchStopJobRunSuccessfulSubmission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.BatchStopJobRunSuccessfulSubmission
  ( BatchStopJobRunSuccessfulSubmission (..)
  -- * Smart constructor
  , mkBatchStopJobRunSuccessfulSubmission
  -- * Lenses
  , bsjrssJobName
  , bsjrssJobRunId
  ) where

import qualified Network.AWS.Glue.Types.JobName as Types
import qualified Network.AWS.Glue.Types.JobRunId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Records a successful request to stop a specified @JobRun@ .
--
-- /See:/ 'mkBatchStopJobRunSuccessfulSubmission' smart constructor.
data BatchStopJobRunSuccessfulSubmission = BatchStopJobRunSuccessfulSubmission'
  { jobName :: Core.Maybe Types.JobName
    -- ^ The name of the job definition used in the job run that was stopped.
  , jobRunId :: Core.Maybe Types.JobRunId
    -- ^ The @JobRunId@ of the job run that was stopped.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchStopJobRunSuccessfulSubmission' value with any optional fields omitted.
mkBatchStopJobRunSuccessfulSubmission
    :: BatchStopJobRunSuccessfulSubmission
mkBatchStopJobRunSuccessfulSubmission
  = BatchStopJobRunSuccessfulSubmission'{jobName = Core.Nothing,
                                         jobRunId = Core.Nothing}

-- | The name of the job definition used in the job run that was stopped.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjrssJobName :: Lens.Lens' BatchStopJobRunSuccessfulSubmission (Core.Maybe Types.JobName)
bsjrssJobName = Lens.field @"jobName"
{-# INLINEABLE bsjrssJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | The @JobRunId@ of the job run that was stopped.
--
-- /Note:/ Consider using 'jobRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjrssJobRunId :: Lens.Lens' BatchStopJobRunSuccessfulSubmission (Core.Maybe Types.JobRunId)
bsjrssJobRunId = Lens.field @"jobRunId"
{-# INLINEABLE bsjrssJobRunId #-}
{-# DEPRECATED jobRunId "Use generic-lens or generic-optics with 'jobRunId' instead"  #-}

instance Core.FromJSON BatchStopJobRunSuccessfulSubmission where
        parseJSON
          = Core.withObject "BatchStopJobRunSuccessfulSubmission" Core.$
              \ x ->
                BatchStopJobRunSuccessfulSubmission' Core.<$>
                  (x Core..:? "JobName") Core.<*> x Core..:? "JobRunId"
