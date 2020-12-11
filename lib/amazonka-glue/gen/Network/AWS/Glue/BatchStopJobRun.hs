{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchStopJobRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops one or more job runs for a specified job definition.
module Network.AWS.Glue.BatchStopJobRun
  ( -- * Creating a request
    BatchStopJobRun (..),
    mkBatchStopJobRun,

    -- ** Request lenses
    bsjrJobName,
    bsjrJobRunIds,

    -- * Destructuring the response
    BatchStopJobRunResponse (..),
    mkBatchStopJobRunResponse,

    -- ** Response lenses
    bsjrrsSuccessfulSubmissions,
    bsjrrsErrors,
    bsjrrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchStopJobRun' smart constructor.
data BatchStopJobRun = BatchStopJobRun'
  { jobName :: Lude.Text,
    jobRunIds :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchStopJobRun' with the minimum fields required to make a request.
--
-- * 'jobName' - The name of the job definition for which to stop job runs.
-- * 'jobRunIds' - A list of the @JobRunIds@ that should be stopped for that job definition.
mkBatchStopJobRun ::
  -- | 'jobName'
  Lude.Text ->
  -- | 'jobRunIds'
  Lude.NonEmpty Lude.Text ->
  BatchStopJobRun
mkBatchStopJobRun pJobName_ pJobRunIds_ =
  BatchStopJobRun' {jobName = pJobName_, jobRunIds = pJobRunIds_}

-- | The name of the job definition for which to stop job runs.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjrJobName :: Lens.Lens' BatchStopJobRun Lude.Text
bsjrJobName = Lens.lens (jobName :: BatchStopJobRun -> Lude.Text) (\s a -> s {jobName = a} :: BatchStopJobRun)
{-# DEPRECATED bsjrJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | A list of the @JobRunIds@ that should be stopped for that job definition.
--
-- /Note:/ Consider using 'jobRunIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjrJobRunIds :: Lens.Lens' BatchStopJobRun (Lude.NonEmpty Lude.Text)
bsjrJobRunIds = Lens.lens (jobRunIds :: BatchStopJobRun -> Lude.NonEmpty Lude.Text) (\s a -> s {jobRunIds = a} :: BatchStopJobRun)
{-# DEPRECATED bsjrJobRunIds "Use generic-lens or generic-optics with 'jobRunIds' instead." #-}

instance Lude.AWSRequest BatchStopJobRun where
  type Rs BatchStopJobRun = BatchStopJobRunResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchStopJobRunResponse'
            Lude.<$> (x Lude..?> "SuccessfulSubmissions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Errors" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchStopJobRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.BatchStopJobRun" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchStopJobRun where
  toJSON BatchStopJobRun' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("JobName" Lude..= jobName),
            Lude.Just ("JobRunIds" Lude..= jobRunIds)
          ]
      )

instance Lude.ToPath BatchStopJobRun where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchStopJobRun where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchStopJobRunResponse' smart constructor.
data BatchStopJobRunResponse = BatchStopJobRunResponse'
  { successfulSubmissions ::
      Lude.Maybe
        [BatchStopJobRunSuccessfulSubmission],
    errors :: Lude.Maybe [BatchStopJobRunError],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchStopJobRunResponse' with the minimum fields required to make a request.
--
-- * 'errors' - A list of the errors that were encountered in trying to stop @JobRuns@ , including the @JobRunId@ for which each error was encountered and details about the error.
-- * 'responseStatus' - The response status code.
-- * 'successfulSubmissions' - A list of the JobRuns that were successfully submitted for stopping.
mkBatchStopJobRunResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchStopJobRunResponse
mkBatchStopJobRunResponse pResponseStatus_ =
  BatchStopJobRunResponse'
    { successfulSubmissions = Lude.Nothing,
      errors = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the JobRuns that were successfully submitted for stopping.
--
-- /Note:/ Consider using 'successfulSubmissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjrrsSuccessfulSubmissions :: Lens.Lens' BatchStopJobRunResponse (Lude.Maybe [BatchStopJobRunSuccessfulSubmission])
bsjrrsSuccessfulSubmissions = Lens.lens (successfulSubmissions :: BatchStopJobRunResponse -> Lude.Maybe [BatchStopJobRunSuccessfulSubmission]) (\s a -> s {successfulSubmissions = a} :: BatchStopJobRunResponse)
{-# DEPRECATED bsjrrsSuccessfulSubmissions "Use generic-lens or generic-optics with 'successfulSubmissions' instead." #-}

-- | A list of the errors that were encountered in trying to stop @JobRuns@ , including the @JobRunId@ for which each error was encountered and details about the error.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjrrsErrors :: Lens.Lens' BatchStopJobRunResponse (Lude.Maybe [BatchStopJobRunError])
bsjrrsErrors = Lens.lens (errors :: BatchStopJobRunResponse -> Lude.Maybe [BatchStopJobRunError]) (\s a -> s {errors = a} :: BatchStopJobRunResponse)
{-# DEPRECATED bsjrrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjrrsResponseStatus :: Lens.Lens' BatchStopJobRunResponse Lude.Int
bsjrrsResponseStatus = Lens.lens (responseStatus :: BatchStopJobRunResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchStopJobRunResponse)
{-# DEPRECATED bsjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
