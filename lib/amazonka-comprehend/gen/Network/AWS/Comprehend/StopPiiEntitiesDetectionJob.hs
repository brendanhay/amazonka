{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StopPiiEntitiesDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a PII entities detection job in progress.
module Network.AWS.Comprehend.StopPiiEntitiesDetectionJob
  ( -- * Creating a request
    StopPiiEntitiesDetectionJob (..),
    mkStopPiiEntitiesDetectionJob,

    -- ** Request lenses
    spedjJobId,

    -- * Destructuring the response
    StopPiiEntitiesDetectionJobResponse (..),
    mkStopPiiEntitiesDetectionJobResponse,

    -- ** Response lenses
    spedjprsJobId,
    spedjprsJobStatus,
    spedjprsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopPiiEntitiesDetectionJob' smart constructor.
newtype StopPiiEntitiesDetectionJob = StopPiiEntitiesDetectionJob'
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

-- | Creates a value of 'StopPiiEntitiesDetectionJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier of the PII entities detection job to stop.
mkStopPiiEntitiesDetectionJob ::
  -- | 'jobId'
  Lude.Text ->
  StopPiiEntitiesDetectionJob
mkStopPiiEntitiesDetectionJob pJobId_ =
  StopPiiEntitiesDetectionJob' {jobId = pJobId_}

-- | The identifier of the PII entities detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjJobId :: Lens.Lens' StopPiiEntitiesDetectionJob Lude.Text
spedjJobId = Lens.lens (jobId :: StopPiiEntitiesDetectionJob -> Lude.Text) (\s a -> s {jobId = a} :: StopPiiEntitiesDetectionJob)
{-# DEPRECATED spedjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest StopPiiEntitiesDetectionJob where
  type
    Rs StopPiiEntitiesDetectionJob =
      StopPiiEntitiesDetectionJobResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopPiiEntitiesDetectionJobResponse'
            Lude.<$> (x Lude..?> "JobId")
            Lude.<*> (x Lude..?> "JobStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopPiiEntitiesDetectionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.StopPiiEntitiesDetectionJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopPiiEntitiesDetectionJob where
  toJSON StopPiiEntitiesDetectionJob' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("JobId" Lude..= jobId)])

instance Lude.ToPath StopPiiEntitiesDetectionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StopPiiEntitiesDetectionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopPiiEntitiesDetectionJobResponse' smart constructor.
data StopPiiEntitiesDetectionJobResponse = StopPiiEntitiesDetectionJobResponse'
  { jobId ::
      Lude.Maybe
        Lude.Text,
    jobStatus ::
      Lude.Maybe
        JobStatus,
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

-- | Creates a value of 'StopPiiEntitiesDetectionJobResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier of the PII entities detection job to stop.
-- * 'jobStatus' - The status of the PII entities detection job.
-- * 'responseStatus' - The response status code.
mkStopPiiEntitiesDetectionJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopPiiEntitiesDetectionJobResponse
mkStopPiiEntitiesDetectionJobResponse pResponseStatus_ =
  StopPiiEntitiesDetectionJobResponse'
    { jobId = Lude.Nothing,
      jobStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the PII entities detection job to stop.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjprsJobId :: Lens.Lens' StopPiiEntitiesDetectionJobResponse (Lude.Maybe Lude.Text)
spedjprsJobId = Lens.lens (jobId :: StopPiiEntitiesDetectionJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StopPiiEntitiesDetectionJobResponse)
{-# DEPRECATED spedjprsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The status of the PII entities detection job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjprsJobStatus :: Lens.Lens' StopPiiEntitiesDetectionJobResponse (Lude.Maybe JobStatus)
spedjprsJobStatus = Lens.lens (jobStatus :: StopPiiEntitiesDetectionJobResponse -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: StopPiiEntitiesDetectionJobResponse)
{-# DEPRECATED spedjprsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjprsResponseStatus :: Lens.Lens' StopPiiEntitiesDetectionJobResponse Lude.Int
spedjprsResponseStatus = Lens.lens (responseStatus :: StopPiiEntitiesDetectionJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopPiiEntitiesDetectionJobResponse)
{-# DEPRECATED spedjprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
