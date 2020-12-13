{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StopKeyPhrasesDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a key phrases detection job in progress.
--
-- If the job state is @IN_PROGRESS@ the job is marked for termination and put into the @STOP_REQUESTED@ state. If the job completes before it can be stopped, it is put into the @COMPLETED@ state; otherwise the job is stopped and put into the @STOPPED@ state.
-- If the job is in the @COMPLETED@ or @FAILED@ state when you call the @StopDominantLanguageDetectionJob@ operation, the operation returns a 400 Internal Request Exception.
-- When a job is stopped, any documents already processed are written to the output location.
module Network.AWS.Comprehend.StopKeyPhrasesDetectionJob
  ( -- * Creating a request
    StopKeyPhrasesDetectionJob (..),
    mkStopKeyPhrasesDetectionJob,

    -- ** Request lenses
    skpdjJobId,

    -- * Destructuring the response
    StopKeyPhrasesDetectionJobResponse (..),
    mkStopKeyPhrasesDetectionJobResponse,

    -- ** Response lenses
    skpdjrsJobId,
    skpdjrsJobStatus,
    skpdjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopKeyPhrasesDetectionJob' smart constructor.
newtype StopKeyPhrasesDetectionJob = StopKeyPhrasesDetectionJob'
  { -- | The identifier of the key phrases detection job to stop.
    jobId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopKeyPhrasesDetectionJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier of the key phrases detection job to stop.
mkStopKeyPhrasesDetectionJob ::
  -- | 'jobId'
  Lude.Text ->
  StopKeyPhrasesDetectionJob
mkStopKeyPhrasesDetectionJob pJobId_ =
  StopKeyPhrasesDetectionJob' {jobId = pJobId_}

-- | The identifier of the key phrases detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjJobId :: Lens.Lens' StopKeyPhrasesDetectionJob Lude.Text
skpdjJobId = Lens.lens (jobId :: StopKeyPhrasesDetectionJob -> Lude.Text) (\s a -> s {jobId = a} :: StopKeyPhrasesDetectionJob)
{-# DEPRECATED skpdjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest StopKeyPhrasesDetectionJob where
  type
    Rs StopKeyPhrasesDetectionJob =
      StopKeyPhrasesDetectionJobResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopKeyPhrasesDetectionJobResponse'
            Lude.<$> (x Lude..?> "JobId")
            Lude.<*> (x Lude..?> "JobStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopKeyPhrasesDetectionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.StopKeyPhrasesDetectionJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopKeyPhrasesDetectionJob where
  toJSON StopKeyPhrasesDetectionJob' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("JobId" Lude..= jobId)])

instance Lude.ToPath StopKeyPhrasesDetectionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StopKeyPhrasesDetectionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopKeyPhrasesDetectionJobResponse' smart constructor.
data StopKeyPhrasesDetectionJobResponse = StopKeyPhrasesDetectionJobResponse'
  { -- | The identifier of the key phrases detection job to stop.
    jobId :: Lude.Maybe Lude.Text,
    -- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopKeyPhrasesDetectionJob@ operation.
    jobStatus :: Lude.Maybe JobStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopKeyPhrasesDetectionJobResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier of the key phrases detection job to stop.
-- * 'jobStatus' - Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopKeyPhrasesDetectionJob@ operation.
-- * 'responseStatus' - The response status code.
mkStopKeyPhrasesDetectionJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopKeyPhrasesDetectionJobResponse
mkStopKeyPhrasesDetectionJobResponse pResponseStatus_ =
  StopKeyPhrasesDetectionJobResponse'
    { jobId = Lude.Nothing,
      jobStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the key phrases detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjrsJobId :: Lens.Lens' StopKeyPhrasesDetectionJobResponse (Lude.Maybe Lude.Text)
skpdjrsJobId = Lens.lens (jobId :: StopKeyPhrasesDetectionJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StopKeyPhrasesDetectionJobResponse)
{-# DEPRECATED skpdjrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopKeyPhrasesDetectionJob@ operation.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjrsJobStatus :: Lens.Lens' StopKeyPhrasesDetectionJobResponse (Lude.Maybe JobStatus)
skpdjrsJobStatus = Lens.lens (jobStatus :: StopKeyPhrasesDetectionJobResponse -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: StopKeyPhrasesDetectionJobResponse)
{-# DEPRECATED skpdjrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpdjrsResponseStatus :: Lens.Lens' StopKeyPhrasesDetectionJobResponse Lude.Int
skpdjrsResponseStatus = Lens.lens (responseStatus :: StopKeyPhrasesDetectionJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopKeyPhrasesDetectionJobResponse)
{-# DEPRECATED skpdjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
