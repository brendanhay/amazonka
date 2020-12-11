{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StopSentimentDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a sentiment detection job in progress.
--
-- If the job state is @IN_PROGRESS@ the job is marked for termination and put into the @STOP_REQUESTED@ state. If the job completes before it can be stopped, it is put into the @COMPLETED@ state; otherwise the job is be stopped and put into the @STOPPED@ state.
-- If the job is in the @COMPLETED@ or @FAILED@ state when you call the @StopDominantLanguageDetectionJob@ operation, the operation returns a 400 Internal Request Exception.
-- When a job is stopped, any documents already processed are written to the output location.
module Network.AWS.Comprehend.StopSentimentDetectionJob
  ( -- * Creating a request
    StopSentimentDetectionJob (..),
    mkStopSentimentDetectionJob,

    -- ** Request lenses
    ssdjJobId,

    -- * Destructuring the response
    StopSentimentDetectionJobResponse (..),
    mkStopSentimentDetectionJobResponse,

    -- ** Response lenses
    storsJobId,
    storsJobStatus,
    storsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopSentimentDetectionJob' smart constructor.
newtype StopSentimentDetectionJob = StopSentimentDetectionJob'
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

-- | Creates a value of 'StopSentimentDetectionJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier of the sentiment detection job to stop.
mkStopSentimentDetectionJob ::
  -- | 'jobId'
  Lude.Text ->
  StopSentimentDetectionJob
mkStopSentimentDetectionJob pJobId_ =
  StopSentimentDetectionJob' {jobId = pJobId_}

-- | The identifier of the sentiment detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdjJobId :: Lens.Lens' StopSentimentDetectionJob Lude.Text
ssdjJobId = Lens.lens (jobId :: StopSentimentDetectionJob -> Lude.Text) (\s a -> s {jobId = a} :: StopSentimentDetectionJob)
{-# DEPRECATED ssdjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest StopSentimentDetectionJob where
  type
    Rs StopSentimentDetectionJob =
      StopSentimentDetectionJobResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopSentimentDetectionJobResponse'
            Lude.<$> (x Lude..?> "JobId")
            Lude.<*> (x Lude..?> "JobStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopSentimentDetectionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.StopSentimentDetectionJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopSentimentDetectionJob where
  toJSON StopSentimentDetectionJob' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("JobId" Lude..= jobId)])

instance Lude.ToPath StopSentimentDetectionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StopSentimentDetectionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopSentimentDetectionJobResponse' smart constructor.
data StopSentimentDetectionJobResponse = StopSentimentDetectionJobResponse'
  { jobId ::
      Lude.Maybe Lude.Text,
    jobStatus ::
      Lude.Maybe JobStatus,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopSentimentDetectionJobResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier of the sentiment detection job to stop.
-- * 'jobStatus' - Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopSentimentDetectionJob@ operation.
-- * 'responseStatus' - The response status code.
mkStopSentimentDetectionJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopSentimentDetectionJobResponse
mkStopSentimentDetectionJobResponse pResponseStatus_ =
  StopSentimentDetectionJobResponse'
    { jobId = Lude.Nothing,
      jobStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the sentiment detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
storsJobId :: Lens.Lens' StopSentimentDetectionJobResponse (Lude.Maybe Lude.Text)
storsJobId = Lens.lens (jobId :: StopSentimentDetectionJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StopSentimentDetectionJobResponse)
{-# DEPRECATED storsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopSentimentDetectionJob@ operation.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
storsJobStatus :: Lens.Lens' StopSentimentDetectionJobResponse (Lude.Maybe JobStatus)
storsJobStatus = Lens.lens (jobStatus :: StopSentimentDetectionJobResponse -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: StopSentimentDetectionJobResponse)
{-# DEPRECATED storsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
storsResponseStatus :: Lens.Lens' StopSentimentDetectionJobResponse Lude.Int
storsResponseStatus = Lens.lens (responseStatus :: StopSentimentDetectionJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopSentimentDetectionJobResponse)
{-# DEPRECATED storsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
