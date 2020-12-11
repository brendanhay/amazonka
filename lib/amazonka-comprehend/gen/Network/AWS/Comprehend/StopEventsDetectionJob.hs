{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StopEventsDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an events detection job in progress.
module Network.AWS.Comprehend.StopEventsDetectionJob
  ( -- * Creating a request
    StopEventsDetectionJob (..),
    mkStopEventsDetectionJob,

    -- ** Request lenses
    sedjJobId,

    -- * Destructuring the response
    StopEventsDetectionJobResponse (..),
    mkStopEventsDetectionJobResponse,

    -- ** Response lenses
    sedjrsJobId,
    sedjrsJobStatus,
    sedjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopEventsDetectionJob' smart constructor.
newtype StopEventsDetectionJob = StopEventsDetectionJob'
  { jobId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopEventsDetectionJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier of the events detection job to stop.
mkStopEventsDetectionJob ::
  -- | 'jobId'
  Lude.Text ->
  StopEventsDetectionJob
mkStopEventsDetectionJob pJobId_ =
  StopEventsDetectionJob' {jobId = pJobId_}

-- | The identifier of the events detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjJobId :: Lens.Lens' StopEventsDetectionJob Lude.Text
sedjJobId = Lens.lens (jobId :: StopEventsDetectionJob -> Lude.Text) (\s a -> s {jobId = a} :: StopEventsDetectionJob)
{-# DEPRECATED sedjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest StopEventsDetectionJob where
  type Rs StopEventsDetectionJob = StopEventsDetectionJobResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopEventsDetectionJobResponse'
            Lude.<$> (x Lude..?> "JobId")
            Lude.<*> (x Lude..?> "JobStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopEventsDetectionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.StopEventsDetectionJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopEventsDetectionJob where
  toJSON StopEventsDetectionJob' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("JobId" Lude..= jobId)])

instance Lude.ToPath StopEventsDetectionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StopEventsDetectionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopEventsDetectionJobResponse' smart constructor.
data StopEventsDetectionJobResponse = StopEventsDetectionJobResponse'
  { jobId ::
      Lude.Maybe Lude.Text,
    jobStatus ::
      Lude.Maybe JobStatus,
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

-- | Creates a value of 'StopEventsDetectionJobResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier of the events detection job to stop.
-- * 'jobStatus' - The status of the events detection job.
-- * 'responseStatus' - The response status code.
mkStopEventsDetectionJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopEventsDetectionJobResponse
mkStopEventsDetectionJobResponse pResponseStatus_ =
  StopEventsDetectionJobResponse'
    { jobId = Lude.Nothing,
      jobStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the events detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjrsJobId :: Lens.Lens' StopEventsDetectionJobResponse (Lude.Maybe Lude.Text)
sedjrsJobId = Lens.lens (jobId :: StopEventsDetectionJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StopEventsDetectionJobResponse)
{-# DEPRECATED sedjrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The status of the events detection job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjrsJobStatus :: Lens.Lens' StopEventsDetectionJobResponse (Lude.Maybe JobStatus)
sedjrsJobStatus = Lens.lens (jobStatus :: StopEventsDetectionJobResponse -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: StopEventsDetectionJobResponse)
{-# DEPRECATED sedjrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjrsResponseStatus :: Lens.Lens' StopEventsDetectionJobResponse Lude.Int
sedjrsResponseStatus = Lens.lens (responseStatus :: StopEventsDetectionJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopEventsDetectionJobResponse)
{-# DEPRECATED sedjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
