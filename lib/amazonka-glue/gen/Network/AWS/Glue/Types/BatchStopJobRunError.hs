{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.BatchStopJobRunError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BatchStopJobRunError
  ( BatchStopJobRunError (..),

    -- * Smart constructor
    mkBatchStopJobRunError,

    -- * Lenses
    bsjreErrorDetail,
    bsjreJobName,
    bsjreJobRunId,
  )
where

import qualified Network.AWS.Glue.Types.ErrorDetail as Types
import qualified Network.AWS.Glue.Types.JobRunId as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Records an error that occurred when attempting to stop a specified job run.
--
-- /See:/ 'mkBatchStopJobRunError' smart constructor.
data BatchStopJobRunError = BatchStopJobRunError'
  { -- | Specifies details about the error that was encountered.
    errorDetail :: Core.Maybe Types.ErrorDetail,
    -- | The name of the job definition that is used in the job run in question.
    jobName :: Core.Maybe Types.NameString,
    -- | The @JobRunId@ of the job run in question.
    jobRunId :: Core.Maybe Types.JobRunId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchStopJobRunError' value with any optional fields omitted.
mkBatchStopJobRunError ::
  BatchStopJobRunError
mkBatchStopJobRunError =
  BatchStopJobRunError'
    { errorDetail = Core.Nothing,
      jobName = Core.Nothing,
      jobRunId = Core.Nothing
    }

-- | Specifies details about the error that was encountered.
--
-- /Note:/ Consider using 'errorDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjreErrorDetail :: Lens.Lens' BatchStopJobRunError (Core.Maybe Types.ErrorDetail)
bsjreErrorDetail = Lens.field @"errorDetail"
{-# DEPRECATED bsjreErrorDetail "Use generic-lens or generic-optics with 'errorDetail' instead." #-}

-- | The name of the job definition that is used in the job run in question.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjreJobName :: Lens.Lens' BatchStopJobRunError (Core.Maybe Types.NameString)
bsjreJobName = Lens.field @"jobName"
{-# DEPRECATED bsjreJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The @JobRunId@ of the job run in question.
--
-- /Note:/ Consider using 'jobRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjreJobRunId :: Lens.Lens' BatchStopJobRunError (Core.Maybe Types.JobRunId)
bsjreJobRunId = Lens.field @"jobRunId"
{-# DEPRECATED bsjreJobRunId "Use generic-lens or generic-optics with 'jobRunId' instead." #-}

instance Core.FromJSON BatchStopJobRunError where
  parseJSON =
    Core.withObject "BatchStopJobRunError" Core.$
      \x ->
        BatchStopJobRunError'
          Core.<$> (x Core..:? "ErrorDetail")
          Core.<*> (x Core..:? "JobName")
          Core.<*> (x Core..:? "JobRunId")
