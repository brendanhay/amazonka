{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CancelJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a job.
module Network.AWS.IoT.CancelJob
  ( -- * Creating a request
    CancelJob (..),
    mkCancelJob,

    -- ** Request lenses
    cForce,
    cReasonCode,
    cComment,
    cJobId,

    -- * Destructuring the response
    CancelJobResponse (..),
    mkCancelJobResponse,

    -- ** Response lenses
    canrsJobId,
    canrsJobARN,
    canrsDescription,
    canrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCancelJob' smart constructor.
data CancelJob = CancelJob'
  { force :: Lude.Maybe Lude.Bool,
    reasonCode :: Lude.Maybe Lude.Text,
    comment :: Lude.Maybe Lude.Text,
    jobId :: Lude.Text
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
-- * 'comment' - An optional comment string describing why the job was canceled.
-- * 'force' - (Optional) If @true@ job executions with status "IN_PROGRESS" and "QUEUED" are canceled, otherwise only job executions with status "QUEUED" are canceled. The default is @false@ .
--
-- Canceling a job which is "IN_PROGRESS", will cause a device which is executing the job to be unable to update the job execution status. Use caution and ensure that each device executing a job which is canceled is able to recover to a valid state.
-- * 'jobId' - The unique identifier you assigned to this job when it was created.
-- * 'reasonCode' - (Optional)A reason code string that explains why the job was canceled.
mkCancelJob ::
  -- | 'jobId'
  Lude.Text ->
  CancelJob
mkCancelJob pJobId_ =
  CancelJob'
    { force = Lude.Nothing,
      reasonCode = Lude.Nothing,
      comment = Lude.Nothing,
      jobId = pJobId_
    }

-- | (Optional) If @true@ job executions with status "IN_PROGRESS" and "QUEUED" are canceled, otherwise only job executions with status "QUEUED" are canceled. The default is @false@ .
--
-- Canceling a job which is "IN_PROGRESS", will cause a device which is executing the job to be unable to update the job execution status. Use caution and ensure that each device executing a job which is canceled is able to recover to a valid state.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cForce :: Lens.Lens' CancelJob (Lude.Maybe Lude.Bool)
cForce = Lens.lens (force :: CancelJob -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: CancelJob)
{-# DEPRECATED cForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | (Optional)A reason code string that explains why the job was canceled.
--
-- /Note:/ Consider using 'reasonCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cReasonCode :: Lens.Lens' CancelJob (Lude.Maybe Lude.Text)
cReasonCode = Lens.lens (reasonCode :: CancelJob -> Lude.Maybe Lude.Text) (\s a -> s {reasonCode = a} :: CancelJob)
{-# DEPRECATED cReasonCode "Use generic-lens or generic-optics with 'reasonCode' instead." #-}

-- | An optional comment string describing why the job was canceled.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cComment :: Lens.Lens' CancelJob (Lude.Maybe Lude.Text)
cComment = Lens.lens (comment :: CancelJob -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: CancelJob)
{-# DEPRECATED cComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cJobId :: Lens.Lens' CancelJob Lude.Text
cJobId = Lens.lens (jobId :: CancelJob -> Lude.Text) (\s a -> s {jobId = a} :: CancelJob)
{-# DEPRECATED cJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest CancelJob where
  type Rs CancelJob = CancelJobResponse
  request = Req.putJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CancelJobResponse'
            Lude.<$> (x Lude..?> "jobId")
            Lude.<*> (x Lude..?> "jobArn")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelJob where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CancelJob where
  toJSON CancelJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("reasonCode" Lude..=) Lude.<$> reasonCode,
            ("comment" Lude..=) Lude.<$> comment
          ]
      )

instance Lude.ToPath CancelJob where
  toPath CancelJob' {..} =
    Lude.mconcat ["/jobs/", Lude.toBS jobId, "/cancel"]

instance Lude.ToQuery CancelJob where
  toQuery CancelJob' {..} = Lude.mconcat ["force" Lude.=: force]

-- | /See:/ 'mkCancelJobResponse' smart constructor.
data CancelJobResponse = CancelJobResponse'
  { jobId ::
      Lude.Maybe Lude.Text,
    jobARN :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CancelJobResponse' with the minimum fields required to make a request.
--
-- * 'description' - A short text description of the job.
-- * 'jobARN' - The job ARN.
-- * 'jobId' - The unique identifier you assigned to this job when it was created.
-- * 'responseStatus' - The response status code.
mkCancelJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelJobResponse
mkCancelJobResponse pResponseStatus_ =
  CancelJobResponse'
    { jobId = Lude.Nothing,
      jobARN = Lude.Nothing,
      description = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canrsJobId :: Lens.Lens' CancelJobResponse (Lude.Maybe Lude.Text)
canrsJobId = Lens.lens (jobId :: CancelJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: CancelJobResponse)
{-# DEPRECATED canrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The job ARN.
--
-- /Note:/ Consider using 'jobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canrsJobARN :: Lens.Lens' CancelJobResponse (Lude.Maybe Lude.Text)
canrsJobARN = Lens.lens (jobARN :: CancelJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobARN = a} :: CancelJobResponse)
{-# DEPRECATED canrsJobARN "Use generic-lens or generic-optics with 'jobARN' instead." #-}

-- | A short text description of the job.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canrsDescription :: Lens.Lens' CancelJobResponse (Lude.Maybe Lude.Text)
canrsDescription = Lens.lens (description :: CancelJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CancelJobResponse)
{-# DEPRECATED canrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canrsResponseStatus :: Lens.Lens' CancelJobResponse Lude.Int
canrsResponseStatus = Lens.lens (responseStatus :: CancelJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelJobResponse)
{-# DEPRECATED canrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
