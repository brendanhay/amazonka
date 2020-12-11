{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.CancelJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified job. You can only cancel a job before its @JobState@ value changes to @PreparingAppliance@ . Requesting the @ListJobs@ or @DescribeJob@ action returns a job's @JobState@ as part of the response element data returned.
module Network.AWS.Snowball.CancelJob
  ( -- * Creating a request
    CancelJob (..),
    mkCancelJob,

    -- ** Request lenses
    cjJobId,

    -- * Destructuring the response
    CancelJobResponse (..),
    mkCancelJobResponse,

    -- ** Response lenses
    crsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Snowball.Types

-- | /See:/ 'mkCancelJob' smart constructor.
newtype CancelJob = CancelJob' {jobId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The 39-character job ID for the job that you want to cancel, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
mkCancelJob ::
  -- | 'jobId'
  Lude.Text ->
  CancelJob
mkCancelJob pJobId_ = CancelJob' {jobId = pJobId_}

-- | The 39-character job ID for the job that you want to cancel, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjJobId :: Lens.Lens' CancelJob Lude.Text
cjJobId = Lens.lens (jobId :: CancelJob -> Lude.Text) (\s a -> s {jobId = a} :: CancelJob)
{-# DEPRECATED cjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest CancelJob where
  type Rs CancelJob = CancelJobResponse
  request = Req.postJSON snowballService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CancelJobResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSIESnowballJobManagementService.CancelJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CancelJob where
  toJSON CancelJob' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("JobId" Lude..= jobId)])

instance Lude.ToPath CancelJob where
  toPath = Lude.const "/"

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
crsResponseStatus :: Lens.Lens' CancelJobResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: CancelJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelJobResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
