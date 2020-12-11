{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.TerminateJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates a job in a job queue. Jobs that are in the @STARTING@ or @RUNNING@ state are terminated, which causes them to transition to @FAILED@ . Jobs that have not progressed to the @STARTING@ state are cancelled.
module Network.AWS.Batch.TerminateJob
  ( -- * Creating a request
    TerminateJob (..),
    mkTerminateJob,

    -- ** Request lenses
    tjJobId,
    tjReason,

    -- * Destructuring the response
    TerminateJobResponse (..),
    mkTerminateJobResponse,

    -- ** Response lenses
    tjrsResponseStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTerminateJob' smart constructor.
data TerminateJob = TerminateJob'
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

-- | Creates a value of 'TerminateJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The AWS Batch job ID of the job to terminate.
-- * 'reason' - A message to attach to the job that explains the reason for canceling it. This message is returned by future 'DescribeJobs' operations on the job. This message is also recorded in the AWS Batch activity logs.
mkTerminateJob ::
  -- | 'jobId'
  Lude.Text ->
  -- | 'reason'
  Lude.Text ->
  TerminateJob
mkTerminateJob pJobId_ pReason_ =
  TerminateJob' {jobId = pJobId_, reason = pReason_}

-- | The AWS Batch job ID of the job to terminate.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjJobId :: Lens.Lens' TerminateJob Lude.Text
tjJobId = Lens.lens (jobId :: TerminateJob -> Lude.Text) (\s a -> s {jobId = a} :: TerminateJob)
{-# DEPRECATED tjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | A message to attach to the job that explains the reason for canceling it. This message is returned by future 'DescribeJobs' operations on the job. This message is also recorded in the AWS Batch activity logs.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjReason :: Lens.Lens' TerminateJob Lude.Text
tjReason = Lens.lens (reason :: TerminateJob -> Lude.Text) (\s a -> s {reason = a} :: TerminateJob)
{-# DEPRECATED tjReason "Use generic-lens or generic-optics with 'reason' instead." #-}

instance Lude.AWSRequest TerminateJob where
  type Rs TerminateJob = TerminateJobResponse
  request = Req.postJSON batchService
  response =
    Res.receiveEmpty
      ( \s h x ->
          TerminateJobResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TerminateJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TerminateJob where
  toJSON TerminateJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("jobId" Lude..= jobId),
            Lude.Just ("reason" Lude..= reason)
          ]
      )

instance Lude.ToPath TerminateJob where
  toPath = Lude.const "/v1/terminatejob"

instance Lude.ToQuery TerminateJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTerminateJobResponse' smart constructor.
newtype TerminateJobResponse = TerminateJobResponse'
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

-- | Creates a value of 'TerminateJobResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkTerminateJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TerminateJobResponse
mkTerminateJobResponse pResponseStatus_ =
  TerminateJobResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjrsResponseStatus :: Lens.Lens' TerminateJobResponse Lude.Int
tjrsResponseStatus = Lens.lens (responseStatus :: TerminateJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TerminateJobResponse)
{-# DEPRECATED tjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
