{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.StopTextTranslationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an asynchronous batch translation job that is in progress.
--
-- If the job's state is @IN_PROGRESS@ , the job will be marked for termination and put into the @STOP_REQUESTED@ state. If the job completes before it can be stopped, it is put into the @COMPLETED@ state. Otherwise, the job is put into the @STOPPED@ state.
-- Asynchronous batch translation jobs are started with the 'StartTextTranslationJob' operation. You can use the 'DescribeTextTranslationJob' or 'ListTextTranslationJobs' operations to get a batch translation job's @JobId@ .
module Network.AWS.Translate.StopTextTranslationJob
  ( -- * Creating a request
    StopTextTranslationJob (..),
    mkStopTextTranslationJob,

    -- ** Request lenses
    sttjJobId,

    -- * Destructuring the response
    StopTextTranslationJobResponse (..),
    mkStopTextTranslationJobResponse,

    -- ** Response lenses
    sttjrsJobId,
    sttjrsJobStatus,
    sttjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Translate.Types

-- | /See:/ 'mkStopTextTranslationJob' smart constructor.
newtype StopTextTranslationJob = StopTextTranslationJob'
  { -- | The job ID of the job to be stopped.
    jobId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopTextTranslationJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The job ID of the job to be stopped.
mkStopTextTranslationJob ::
  -- | 'jobId'
  Lude.Text ->
  StopTextTranslationJob
mkStopTextTranslationJob pJobId_ =
  StopTextTranslationJob' {jobId = pJobId_}

-- | The job ID of the job to be stopped.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjJobId :: Lens.Lens' StopTextTranslationJob Lude.Text
sttjJobId = Lens.lens (jobId :: StopTextTranslationJob -> Lude.Text) (\s a -> s {jobId = a} :: StopTextTranslationJob)
{-# DEPRECATED sttjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest StopTextTranslationJob where
  type Rs StopTextTranslationJob = StopTextTranslationJobResponse
  request = Req.postJSON translateService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopTextTranslationJobResponse'
            Lude.<$> (x Lude..?> "JobId")
            Lude.<*> (x Lude..?> "JobStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopTextTranslationJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSShineFrontendService_20170701.StopTextTranslationJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopTextTranslationJob where
  toJSON StopTextTranslationJob' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("JobId" Lude..= jobId)])

instance Lude.ToPath StopTextTranslationJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StopTextTranslationJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopTextTranslationJobResponse' smart constructor.
data StopTextTranslationJobResponse = StopTextTranslationJobResponse'
  { -- | The job ID of the stopped batch translation job.
    jobId :: Lude.Maybe Lude.Text,
    -- | The status of the designated job. Upon successful completion, the job's status will be @STOPPED@ .
    jobStatus :: Lude.Maybe JobStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopTextTranslationJobResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The job ID of the stopped batch translation job.
-- * 'jobStatus' - The status of the designated job. Upon successful completion, the job's status will be @STOPPED@ .
-- * 'responseStatus' - The response status code.
mkStopTextTranslationJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopTextTranslationJobResponse
mkStopTextTranslationJobResponse pResponseStatus_ =
  StopTextTranslationJobResponse'
    { jobId = Lude.Nothing,
      jobStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The job ID of the stopped batch translation job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjrsJobId :: Lens.Lens' StopTextTranslationJobResponse (Lude.Maybe Lude.Text)
sttjrsJobId = Lens.lens (jobId :: StopTextTranslationJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StopTextTranslationJobResponse)
{-# DEPRECATED sttjrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The status of the designated job. Upon successful completion, the job's status will be @STOPPED@ .
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjrsJobStatus :: Lens.Lens' StopTextTranslationJobResponse (Lude.Maybe JobStatus)
sttjrsJobStatus = Lens.lens (jobStatus :: StopTextTranslationJobResponse -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: StopTextTranslationJobResponse)
{-# DEPRECATED sttjrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjrsResponseStatus :: Lens.Lens' StopTextTranslationJobResponse Lude.Int
sttjrsResponseStatus = Lens.lens (responseStatus :: StopTextTranslationJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopTextTranslationJobResponse)
{-# DEPRECATED sttjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
