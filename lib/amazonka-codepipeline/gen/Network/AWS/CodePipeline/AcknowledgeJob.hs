{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.AcknowledgeJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specified job and whether that job has been received by the job worker. Used for custom actions only.
module Network.AWS.CodePipeline.AcknowledgeJob
  ( -- * Creating a request
    AcknowledgeJob (..),
    mkAcknowledgeJob,

    -- ** Request lenses
    ajJobId,
    ajNonce,

    -- * Destructuring the response
    AcknowledgeJobResponse (..),
    mkAcknowledgeJobResponse,

    -- ** Response lenses
    ajrsStatus,
    ajrsResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of an AcknowledgeJob action.
--
-- /See:/ 'mkAcknowledgeJob' smart constructor.
data AcknowledgeJob = AcknowledgeJob'
  { jobId :: Lude.Text,
    nonce :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcknowledgeJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The unique system-generated ID of the job for which you want to confirm receipt.
-- * 'nonce' - A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Get this number from the response of the 'PollForJobs' request that returned this job.
mkAcknowledgeJob ::
  -- | 'jobId'
  Lude.Text ->
  -- | 'nonce'
  Lude.Text ->
  AcknowledgeJob
mkAcknowledgeJob pJobId_ pNonce_ =
  AcknowledgeJob' {jobId = pJobId_, nonce = pNonce_}

-- | The unique system-generated ID of the job for which you want to confirm receipt.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajJobId :: Lens.Lens' AcknowledgeJob Lude.Text
ajJobId = Lens.lens (jobId :: AcknowledgeJob -> Lude.Text) (\s a -> s {jobId = a} :: AcknowledgeJob)
{-# DEPRECATED ajJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Get this number from the response of the 'PollForJobs' request that returned this job.
--
-- /Note:/ Consider using 'nonce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajNonce :: Lens.Lens' AcknowledgeJob Lude.Text
ajNonce = Lens.lens (nonce :: AcknowledgeJob -> Lude.Text) (\s a -> s {nonce = a} :: AcknowledgeJob)
{-# DEPRECATED ajNonce "Use generic-lens or generic-optics with 'nonce' instead." #-}

instance Lude.AWSRequest AcknowledgeJob where
  type Rs AcknowledgeJob = AcknowledgeJobResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          AcknowledgeJobResponse'
            Lude.<$> (x Lude..?> "status") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AcknowledgeJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodePipeline_20150709.AcknowledgeJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AcknowledgeJob where
  toJSON AcknowledgeJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("jobId" Lude..= jobId),
            Lude.Just ("nonce" Lude..= nonce)
          ]
      )

instance Lude.ToPath AcknowledgeJob where
  toPath = Lude.const "/"

instance Lude.ToQuery AcknowledgeJob where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of an AcknowledgeJob action.
--
-- /See:/ 'mkAcknowledgeJobResponse' smart constructor.
data AcknowledgeJobResponse = AcknowledgeJobResponse'
  { status ::
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

-- | Creates a value of 'AcknowledgeJobResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'status' - Whether the job worker has received the specified job.
mkAcknowledgeJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AcknowledgeJobResponse
mkAcknowledgeJobResponse pResponseStatus_ =
  AcknowledgeJobResponse'
    { status = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Whether the job worker has received the specified job.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajrsStatus :: Lens.Lens' AcknowledgeJobResponse (Lude.Maybe JobStatus)
ajrsStatus = Lens.lens (status :: AcknowledgeJobResponse -> Lude.Maybe JobStatus) (\s a -> s {status = a} :: AcknowledgeJobResponse)
{-# DEPRECATED ajrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajrsResponseStatus :: Lens.Lens' AcknowledgeJobResponse Lude.Int
ajrsResponseStatus = Lens.lens (responseStatus :: AcknowledgeJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AcknowledgeJobResponse)
{-# DEPRECATED ajrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
