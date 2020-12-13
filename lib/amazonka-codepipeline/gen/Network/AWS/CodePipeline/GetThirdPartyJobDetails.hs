{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.GetThirdPartyJobDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests the details of a job for a third party action. Used for partner actions only.
--
-- /Important:/ When this API is called, AWS CodePipeline returns temporary credentials for the S3 bucket used to store artifacts for the pipeline, if the action requires access to that S3 bucket for input or output artifacts. This API also returns any secret values defined for the action.
module Network.AWS.CodePipeline.GetThirdPartyJobDetails
  ( -- * Creating a request
    GetThirdPartyJobDetails (..),
    mkGetThirdPartyJobDetails,

    -- ** Request lenses
    gtpjdJobId,
    gtpjdClientToken,

    -- * Destructuring the response
    GetThirdPartyJobDetailsResponse (..),
    mkGetThirdPartyJobDetailsResponse,

    -- ** Response lenses
    gtpjdrsJobDetails,
    gtpjdrsResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @GetThirdPartyJobDetails@ action.
--
-- /See:/ 'mkGetThirdPartyJobDetails' smart constructor.
data GetThirdPartyJobDetails = GetThirdPartyJobDetails'
  { -- | The unique system-generated ID used for identifying the job.
    jobId :: Lude.Text,
    -- | The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
    clientToken :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetThirdPartyJobDetails' with the minimum fields required to make a request.
--
-- * 'jobId' - The unique system-generated ID used for identifying the job.
-- * 'clientToken' - The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
mkGetThirdPartyJobDetails ::
  -- | 'jobId'
  Lude.Text ->
  -- | 'clientToken'
  Lude.Text ->
  GetThirdPartyJobDetails
mkGetThirdPartyJobDetails pJobId_ pClientToken_ =
  GetThirdPartyJobDetails'
    { jobId = pJobId_,
      clientToken = pClientToken_
    }

-- | The unique system-generated ID used for identifying the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpjdJobId :: Lens.Lens' GetThirdPartyJobDetails Lude.Text
gtpjdJobId = Lens.lens (jobId :: GetThirdPartyJobDetails -> Lude.Text) (\s a -> s {jobId = a} :: GetThirdPartyJobDetails)
{-# DEPRECATED gtpjdJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpjdClientToken :: Lens.Lens' GetThirdPartyJobDetails Lude.Text
gtpjdClientToken = Lens.lens (clientToken :: GetThirdPartyJobDetails -> Lude.Text) (\s a -> s {clientToken = a} :: GetThirdPartyJobDetails)
{-# DEPRECATED gtpjdClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

instance Lude.AWSRequest GetThirdPartyJobDetails where
  type Rs GetThirdPartyJobDetails = GetThirdPartyJobDetailsResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetThirdPartyJobDetailsResponse'
            Lude.<$> (x Lude..?> "jobDetails") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetThirdPartyJobDetails where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodePipeline_20150709.GetThirdPartyJobDetails" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetThirdPartyJobDetails where
  toJSON GetThirdPartyJobDetails' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("jobId" Lude..= jobId),
            Lude.Just ("clientToken" Lude..= clientToken)
          ]
      )

instance Lude.ToPath GetThirdPartyJobDetails where
  toPath = Lude.const "/"

instance Lude.ToQuery GetThirdPartyJobDetails where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @GetThirdPartyJobDetails@ action.
--
-- /See:/ 'mkGetThirdPartyJobDetailsResponse' smart constructor.
data GetThirdPartyJobDetailsResponse = GetThirdPartyJobDetailsResponse'
  { -- | The details of the job, including any protected values defined for the job.
    jobDetails :: Lude.Maybe ThirdPartyJobDetails,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetThirdPartyJobDetailsResponse' with the minimum fields required to make a request.
--
-- * 'jobDetails' - The details of the job, including any protected values defined for the job.
-- * 'responseStatus' - The response status code.
mkGetThirdPartyJobDetailsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetThirdPartyJobDetailsResponse
mkGetThirdPartyJobDetailsResponse pResponseStatus_ =
  GetThirdPartyJobDetailsResponse'
    { jobDetails = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The details of the job, including any protected values defined for the job.
--
-- /Note:/ Consider using 'jobDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpjdrsJobDetails :: Lens.Lens' GetThirdPartyJobDetailsResponse (Lude.Maybe ThirdPartyJobDetails)
gtpjdrsJobDetails = Lens.lens (jobDetails :: GetThirdPartyJobDetailsResponse -> Lude.Maybe ThirdPartyJobDetails) (\s a -> s {jobDetails = a} :: GetThirdPartyJobDetailsResponse)
{-# DEPRECATED gtpjdrsJobDetails "Use generic-lens or generic-optics with 'jobDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpjdrsResponseStatus :: Lens.Lens' GetThirdPartyJobDetailsResponse Lude.Int
gtpjdrsResponseStatus = Lens.lens (responseStatus :: GetThirdPartyJobDetailsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetThirdPartyJobDetailsResponse)
{-# DEPRECATED gtpjdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
