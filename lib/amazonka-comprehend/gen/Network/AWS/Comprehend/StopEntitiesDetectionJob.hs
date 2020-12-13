{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StopEntitiesDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an entities detection job in progress.
--
-- If the job state is @IN_PROGRESS@ the job is marked for termination and put into the @STOP_REQUESTED@ state. If the job completes before it can be stopped, it is put into the @COMPLETED@ state; otherwise the job is stopped and put into the @STOPPED@ state.
-- If the job is in the @COMPLETED@ or @FAILED@ state when you call the @StopDominantLanguageDetectionJob@ operation, the operation returns a 400 Internal Request Exception.
-- When a job is stopped, any documents already processed are written to the output location.
module Network.AWS.Comprehend.StopEntitiesDetectionJob
  ( -- * Creating a request
    StopEntitiesDetectionJob (..),
    mkStopEntitiesDetectionJob,

    -- ** Request lenses
    sedjJobId,

    -- * Destructuring the response
    StopEntitiesDetectionJobResponse (..),
    mkStopEntitiesDetectionJobResponse,

    -- ** Response lenses
    srsJobId,
    srsJobStatus,
    srsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopEntitiesDetectionJob' smart constructor.
newtype StopEntitiesDetectionJob = StopEntitiesDetectionJob'
  { -- | The identifier of the entities detection job to stop.
    jobId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopEntitiesDetectionJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier of the entities detection job to stop.
mkStopEntitiesDetectionJob ::
  -- | 'jobId'
  Lude.Text ->
  StopEntitiesDetectionJob
mkStopEntitiesDetectionJob pJobId_ =
  StopEntitiesDetectionJob' {jobId = pJobId_}

-- | The identifier of the entities detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sedjJobId :: Lens.Lens' StopEntitiesDetectionJob Lude.Text
sedjJobId = Lens.lens (jobId :: StopEntitiesDetectionJob -> Lude.Text) (\s a -> s {jobId = a} :: StopEntitiesDetectionJob)
{-# DEPRECATED sedjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest StopEntitiesDetectionJob where
  type Rs StopEntitiesDetectionJob = StopEntitiesDetectionJobResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopEntitiesDetectionJobResponse'
            Lude.<$> (x Lude..?> "JobId")
            Lude.<*> (x Lude..?> "JobStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopEntitiesDetectionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.StopEntitiesDetectionJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopEntitiesDetectionJob where
  toJSON StopEntitiesDetectionJob' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("JobId" Lude..= jobId)])

instance Lude.ToPath StopEntitiesDetectionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StopEntitiesDetectionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopEntitiesDetectionJobResponse' smart constructor.
data StopEntitiesDetectionJobResponse = StopEntitiesDetectionJobResponse'
  { -- | The identifier of the entities detection job to stop.
    jobId :: Lude.Maybe Lude.Text,
    -- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopEntitiesDetectionJob@ operation.
    jobStatus :: Lude.Maybe JobStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopEntitiesDetectionJobResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier of the entities detection job to stop.
-- * 'jobStatus' - Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopEntitiesDetectionJob@ operation.
-- * 'responseStatus' - The response status code.
mkStopEntitiesDetectionJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopEntitiesDetectionJobResponse
mkStopEntitiesDetectionJobResponse pResponseStatus_ =
  StopEntitiesDetectionJobResponse'
    { jobId = Lude.Nothing,
      jobStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the entities detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsJobId :: Lens.Lens' StopEntitiesDetectionJobResponse (Lude.Maybe Lude.Text)
srsJobId = Lens.lens (jobId :: StopEntitiesDetectionJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StopEntitiesDetectionJobResponse)
{-# DEPRECATED srsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if the job was previously stopped with the @StopEntitiesDetectionJob@ operation.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsJobStatus :: Lens.Lens' StopEntitiesDetectionJobResponse (Lude.Maybe JobStatus)
srsJobStatus = Lens.lens (jobStatus :: StopEntitiesDetectionJobResponse -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: StopEntitiesDetectionJobResponse)
{-# DEPRECATED srsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopEntitiesDetectionJobResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StopEntitiesDetectionJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopEntitiesDetectionJobResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
