{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.GetJobDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a job. Used for custom actions only.
--
-- /Important:/ When this API is called, AWS CodePipeline returns temporary credentials for the S3 bucket used to store artifacts for the pipeline, if the action requires access to that S3 bucket for input or output artifacts. This API also returns any secret values defined for the action.
module Network.AWS.CodePipeline.GetJobDetails
  ( -- * Creating a request
    GetJobDetails (..),
    mkGetJobDetails,

    -- ** Request lenses
    gjdJobId,

    -- * Destructuring the response
    GetJobDetailsResponse (..),
    mkGetJobDetailsResponse,

    -- ** Response lenses
    gjdrsJobDetails,
    gjdrsResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @GetJobDetails@ action.
--
-- /See:/ 'mkGetJobDetails' smart constructor.
newtype GetJobDetails = GetJobDetails' {jobId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJobDetails' with the minimum fields required to make a request.
--
-- * 'jobId' - The unique system-generated ID for the job.
mkGetJobDetails ::
  -- | 'jobId'
  Lude.Text ->
  GetJobDetails
mkGetJobDetails pJobId_ = GetJobDetails' {jobId = pJobId_}

-- | The unique system-generated ID for the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdJobId :: Lens.Lens' GetJobDetails Lude.Text
gjdJobId = Lens.lens (jobId :: GetJobDetails -> Lude.Text) (\s a -> s {jobId = a} :: GetJobDetails)
{-# DEPRECATED gjdJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest GetJobDetails where
  type Rs GetJobDetails = GetJobDetailsResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetJobDetailsResponse'
            Lude.<$> (x Lude..?> "jobDetails") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetJobDetails where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodePipeline_20150709.GetJobDetails" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetJobDetails where
  toJSON GetJobDetails' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("jobId" Lude..= jobId)])

instance Lude.ToPath GetJobDetails where
  toPath = Lude.const "/"

instance Lude.ToQuery GetJobDetails where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @GetJobDetails@ action.
--
-- /See:/ 'mkGetJobDetailsResponse' smart constructor.
data GetJobDetailsResponse = GetJobDetailsResponse'
  { jobDetails ::
      Lude.Maybe JobDetails,
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

-- | Creates a value of 'GetJobDetailsResponse' with the minimum fields required to make a request.
--
-- * 'jobDetails' - The details of the job.
-- * 'responseStatus' - The response status code.
mkGetJobDetailsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetJobDetailsResponse
mkGetJobDetailsResponse pResponseStatus_ =
  GetJobDetailsResponse'
    { jobDetails = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The details of the job.
--
-- /Note:/ Consider using 'jobDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrsJobDetails :: Lens.Lens' GetJobDetailsResponse (Lude.Maybe JobDetails)
gjdrsJobDetails = Lens.lens (jobDetails :: GetJobDetailsResponse -> Lude.Maybe JobDetails) (\s a -> s {jobDetails = a} :: GetJobDetailsResponse)
{-# DEPRECATED gjdrsJobDetails "Use generic-lens or generic-optics with 'jobDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrsResponseStatus :: Lens.Lens' GetJobDetailsResponse Lude.Int
gjdrsResponseStatus = Lens.lens (responseStatus :: GetJobDetailsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetJobDetailsResponse)
{-# DEPRECATED gjdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
