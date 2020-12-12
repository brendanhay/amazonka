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
    bsjreJobName,
    bsjreJobRunId,
    bsjreErrorDetail,
  )
where

import Network.AWS.Glue.Types.ErrorDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Records an error that occurred when attempting to stop a specified job run.
--
-- /See:/ 'mkBatchStopJobRunError' smart constructor.
data BatchStopJobRunError = BatchStopJobRunError'
  { jobName ::
      Lude.Maybe Lude.Text,
    jobRunId :: Lude.Maybe Lude.Text,
    errorDetail :: Lude.Maybe ErrorDetail
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchStopJobRunError' with the minimum fields required to make a request.
--
-- * 'errorDetail' - Specifies details about the error that was encountered.
-- * 'jobName' - The name of the job definition that is used in the job run in question.
-- * 'jobRunId' - The @JobRunId@ of the job run in question.
mkBatchStopJobRunError ::
  BatchStopJobRunError
mkBatchStopJobRunError =
  BatchStopJobRunError'
    { jobName = Lude.Nothing,
      jobRunId = Lude.Nothing,
      errorDetail = Lude.Nothing
    }

-- | The name of the job definition that is used in the job run in question.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjreJobName :: Lens.Lens' BatchStopJobRunError (Lude.Maybe Lude.Text)
bsjreJobName = Lens.lens (jobName :: BatchStopJobRunError -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: BatchStopJobRunError)
{-# DEPRECATED bsjreJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The @JobRunId@ of the job run in question.
--
-- /Note:/ Consider using 'jobRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjreJobRunId :: Lens.Lens' BatchStopJobRunError (Lude.Maybe Lude.Text)
bsjreJobRunId = Lens.lens (jobRunId :: BatchStopJobRunError -> Lude.Maybe Lude.Text) (\s a -> s {jobRunId = a} :: BatchStopJobRunError)
{-# DEPRECATED bsjreJobRunId "Use generic-lens or generic-optics with 'jobRunId' instead." #-}

-- | Specifies details about the error that was encountered.
--
-- /Note:/ Consider using 'errorDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjreErrorDetail :: Lens.Lens' BatchStopJobRunError (Lude.Maybe ErrorDetail)
bsjreErrorDetail = Lens.lens (errorDetail :: BatchStopJobRunError -> Lude.Maybe ErrorDetail) (\s a -> s {errorDetail = a} :: BatchStopJobRunError)
{-# DEPRECATED bsjreErrorDetail "Use generic-lens or generic-optics with 'errorDetail' instead." #-}

instance Lude.FromJSON BatchStopJobRunError where
  parseJSON =
    Lude.withObject
      "BatchStopJobRunError"
      ( \x ->
          BatchStopJobRunError'
            Lude.<$> (x Lude..:? "JobName")
            Lude.<*> (x Lude..:? "JobRunId")
            Lude.<*> (x Lude..:? "ErrorDetail")
      )
