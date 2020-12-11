{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.CancelJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a job in an AWS Batch job queue. Jobs that are in the @SUBMITTED@ , @PENDING@ , or @RUNNABLE@ state are cancelled. Jobs that have progressed to @STARTING@ or @RUNNING@ are not cancelled (but the API operation still succeeds, even if no job is cancelled); these jobs must be terminated with the 'TerminateJob' operation.
module Network.AWS.Batch.CancelJob
  ( -- * Creating a request
    CancelJob (..),
    mkCancelJob,

    -- ** Request lenses
    cjJobId,
    cjReason,

    -- * Destructuring the response
    CancelJobResponse (..),
    mkCancelJobResponse,

    -- ** Response lenses
    cjrsResponseStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCancelJob' smart constructor.
data CancelJob = CancelJob'
  { jobId :: Lude.Text,
    reason :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The AWS Batch job ID of the job to cancel.
-- * 'reason' - A message to attach to the job that explains the reason for canceling it. This message is returned by future 'DescribeJobs' operations on the job. This message is also recorded in the AWS Batch activity logs.
mkCancelJob ::
  -- | 'jobId'
  Lude.Text ->
  -- | 'reason'
  Lude.Text ->
  CancelJob
mkCancelJob pJobId_ pReason_ =
  CancelJob' {jobId = pJobId_, reason = pReason_}

-- | The AWS Batch job ID of the job to cancel.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjJobId :: Lens.Lens' CancelJob Lude.Text
cjJobId = Lens.lens (jobId :: CancelJob -> Lude.Text) (\s a -> s {jobId = a} :: CancelJob)
{-# DEPRECATED cjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | A message to attach to the job that explains the reason for canceling it. This message is returned by future 'DescribeJobs' operations on the job. This message is also recorded in the AWS Batch activity logs.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjReason :: Lens.Lens' CancelJob Lude.Text
cjReason = Lens.lens (reason :: CancelJob -> Lude.Text) (\s a -> s {reason = a} :: CancelJob)
{-# DEPRECATED cjReason "Use generic-lens or generic-optics with 'reason' instead." #-}

instance Lude.AWSRequest CancelJob where
  type Rs CancelJob = CancelJobResponse
  request = Req.postJSON batchService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CancelJobResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CancelJob where
  toJSON CancelJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("jobId" Lude..= jobId),
            Lude.Just ("reason" Lude..= reason)
          ]
      )

instance Lude.ToPath CancelJob where
  toPath = Lude.const "/v1/canceljob"

instance Lude.ToQuery CancelJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCancelJobResponse' smart constructor.
newtype CancelJobResponse = CancelJobResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelJobResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCancelJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelJobResponse
mkCancelJobResponse pResponseStatus_ =
  CancelJobResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrsResponseStatus :: Lens.Lens' CancelJobResponse Lude.Int
cjrsResponseStatus = Lens.lens (responseStatus :: CancelJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelJobResponse)
{-# DEPRECATED cjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
