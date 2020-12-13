{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.BatchStopJobRunSuccessfulSubmission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BatchStopJobRunSuccessfulSubmission
  ( BatchStopJobRunSuccessfulSubmission (..),

    -- * Smart constructor
    mkBatchStopJobRunSuccessfulSubmission,

    -- * Lenses
    bsjrssJobName,
    bsjrssJobRunId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Records a successful request to stop a specified @JobRun@ .
--
-- /See:/ 'mkBatchStopJobRunSuccessfulSubmission' smart constructor.
data BatchStopJobRunSuccessfulSubmission = BatchStopJobRunSuccessfulSubmission'
  { -- | The name of the job definition used in the job run that was stopped.
    jobName :: Lude.Maybe Lude.Text,
    -- | The @JobRunId@ of the job run that was stopped.
    jobRunId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchStopJobRunSuccessfulSubmission' with the minimum fields required to make a request.
--
-- * 'jobName' - The name of the job definition used in the job run that was stopped.
-- * 'jobRunId' - The @JobRunId@ of the job run that was stopped.
mkBatchStopJobRunSuccessfulSubmission ::
  BatchStopJobRunSuccessfulSubmission
mkBatchStopJobRunSuccessfulSubmission =
  BatchStopJobRunSuccessfulSubmission'
    { jobName = Lude.Nothing,
      jobRunId = Lude.Nothing
    }

-- | The name of the job definition used in the job run that was stopped.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjrssJobName :: Lens.Lens' BatchStopJobRunSuccessfulSubmission (Lude.Maybe Lude.Text)
bsjrssJobName = Lens.lens (jobName :: BatchStopJobRunSuccessfulSubmission -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: BatchStopJobRunSuccessfulSubmission)
{-# DEPRECATED bsjrssJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The @JobRunId@ of the job run that was stopped.
--
-- /Note:/ Consider using 'jobRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjrssJobRunId :: Lens.Lens' BatchStopJobRunSuccessfulSubmission (Lude.Maybe Lude.Text)
bsjrssJobRunId = Lens.lens (jobRunId :: BatchStopJobRunSuccessfulSubmission -> Lude.Maybe Lude.Text) (\s a -> s {jobRunId = a} :: BatchStopJobRunSuccessfulSubmission)
{-# DEPRECATED bsjrssJobRunId "Use generic-lens or generic-optics with 'jobRunId' instead." #-}

instance Lude.FromJSON BatchStopJobRunSuccessfulSubmission where
  parseJSON =
    Lude.withObject
      "BatchStopJobRunSuccessfulSubmission"
      ( \x ->
          BatchStopJobRunSuccessfulSubmission'
            Lude.<$> (x Lude..:? "JobName") Lude.<*> (x Lude..:? "JobRunId")
      )
