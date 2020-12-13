{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StopDominantLanguageDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a dominant language detection job in progress.
--
-- If the job state is @IN_PROGRESS@ the job is marked for termination and put into the @STOP_REQUESTED@ state. If the job completes before it can be stopped, it is put into the @COMPLETED@ state; otherwise the job is stopped and put into the @STOPPED@ state.
-- If the job is in the @COMPLETED@ or @FAILED@ state when you call the @StopDominantLanguageDetectionJob@ operation, the operation returns a 400 Internal Request Exception.
-- When a job is stopped, any documents already processed are written to the output location.
module Network.AWS.Comprehend.StopDominantLanguageDetectionJob
  ( -- * Creating a request
    StopDominantLanguageDetectionJob (..),
    mkStopDominantLanguageDetectionJob,

    -- ** Request lenses
    sdldjJobId,

    -- * Destructuring the response
    StopDominantLanguageDetectionJobResponse (..),
    mkStopDominantLanguageDetectionJobResponse,

    -- ** Response lenses
    sdldjfrsJobId,
    sdldjfrsJobStatus,
    sdldjfrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopDominantLanguageDetectionJob' smart constructor.
newtype StopDominantLanguageDetectionJob = StopDominantLanguageDetectionJob'
  { -- | The identifier of the dominant language detection job to stop.
    jobId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopDominantLanguageDetectionJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier of the dominant language detection job to stop.
mkStopDominantLanguageDetectionJob ::
  -- | 'jobId'
  Lude.Text ->
  StopDominantLanguageDetectionJob
mkStopDominantLanguageDetectionJob pJobId_ =
  StopDominantLanguageDetectionJob' {jobId = pJobId_}

-- | The identifier of the dominant language detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjJobId :: Lens.Lens' StopDominantLanguageDetectionJob Lude.Text
sdldjJobId = Lens.lens (jobId :: StopDominantLanguageDetectionJob -> Lude.Text) (\s a -> s {jobId = a} :: StopDominantLanguageDetectionJob)
{-# DEPRECATED sdldjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest StopDominantLanguageDetectionJob where
  type
    Rs StopDominantLanguageDetectionJob =
      StopDominantLanguageDetectionJobResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopDominantLanguageDetectionJobResponse'
            Lude.<$> (x Lude..?> "JobId")
            Lude.<*> (x Lude..?> "JobStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopDominantLanguageDetectionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.StopDominantLanguageDetectionJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopDominantLanguageDetectionJob where
  toJSON StopDominantLanguageDetectionJob' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("JobId" Lude..= jobId)])

instance Lude.ToPath StopDominantLanguageDetectionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StopDominantLanguageDetectionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopDominantLanguageDetectionJobResponse' smart constructor.
data StopDominantLanguageDetectionJobResponse = StopDominantLanguageDetectionJobResponse'
  { -- | The identifier of the dominant language detection job to stop.
    jobId :: Lude.Maybe Lude.Text,
    -- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopDominantLanguageDetectionJob@ operation.
    jobStatus :: Lude.Maybe JobStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopDominantLanguageDetectionJobResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier of the dominant language detection job to stop.
-- * 'jobStatus' - Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopDominantLanguageDetectionJob@ operation.
-- * 'responseStatus' - The response status code.
mkStopDominantLanguageDetectionJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopDominantLanguageDetectionJobResponse
mkStopDominantLanguageDetectionJobResponse pResponseStatus_ =
  StopDominantLanguageDetectionJobResponse'
    { jobId = Lude.Nothing,
      jobStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the dominant language detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjfrsJobId :: Lens.Lens' StopDominantLanguageDetectionJobResponse (Lude.Maybe Lude.Text)
sdldjfrsJobId = Lens.lens (jobId :: StopDominantLanguageDetectionJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StopDominantLanguageDetectionJobResponse)
{-# DEPRECATED sdldjfrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopDominantLanguageDetectionJob@ operation.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjfrsJobStatus :: Lens.Lens' StopDominantLanguageDetectionJobResponse (Lude.Maybe JobStatus)
sdldjfrsJobStatus = Lens.lens (jobStatus :: StopDominantLanguageDetectionJobResponse -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: StopDominantLanguageDetectionJobResponse)
{-# DEPRECATED sdldjfrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdldjfrsResponseStatus :: Lens.Lens' StopDominantLanguageDetectionJobResponse Lude.Int
sdldjfrsResponseStatus = Lens.lens (responseStatus :: StopDominantLanguageDetectionJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopDominantLanguageDetectionJobResponse)
{-# DEPRECATED sdldjfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
