{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribeDominantLanguageDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a dominant language detection job. Use this operation to get the status of a detection job.
module Network.AWS.Comprehend.DescribeDominantLanguageDetectionJob
  ( -- * Creating a request
    DescribeDominantLanguageDetectionJob (..),
    mkDescribeDominantLanguageDetectionJob,

    -- ** Request lenses
    ddldjJobId,

    -- * Destructuring the response
    DescribeDominantLanguageDetectionJobResponse (..),
    mkDescribeDominantLanguageDetectionJobResponse,

    -- ** Response lenses
    ddldjrsDominantLanguageDetectionJobProperties,
    ddldjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDominantLanguageDetectionJob' smart constructor.
newtype DescribeDominantLanguageDetectionJob = DescribeDominantLanguageDetectionJob'
  { -- | The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
    jobId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDominantLanguageDetectionJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
mkDescribeDominantLanguageDetectionJob ::
  -- | 'jobId'
  Lude.Text ->
  DescribeDominantLanguageDetectionJob
mkDescribeDominantLanguageDetectionJob pJobId_ =
  DescribeDominantLanguageDetectionJob' {jobId = pJobId_}

-- | The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddldjJobId :: Lens.Lens' DescribeDominantLanguageDetectionJob Lude.Text
ddldjJobId = Lens.lens (jobId :: DescribeDominantLanguageDetectionJob -> Lude.Text) (\s a -> s {jobId = a} :: DescribeDominantLanguageDetectionJob)
{-# DEPRECATED ddldjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest DescribeDominantLanguageDetectionJob where
  type
    Rs DescribeDominantLanguageDetectionJob =
      DescribeDominantLanguageDetectionJobResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDominantLanguageDetectionJobResponse'
            Lude.<$> (x Lude..?> "DominantLanguageDetectionJobProperties")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDominantLanguageDetectionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.DescribeDominantLanguageDetectionJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeDominantLanguageDetectionJob where
  toJSON DescribeDominantLanguageDetectionJob' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("JobId" Lude..= jobId)])

instance Lude.ToPath DescribeDominantLanguageDetectionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDominantLanguageDetectionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeDominantLanguageDetectionJobResponse' smart constructor.
data DescribeDominantLanguageDetectionJobResponse = DescribeDominantLanguageDetectionJobResponse'
  { -- | An object that contains the properties associated with a dominant language detection job.
    dominantLanguageDetectionJobProperties :: Lude.Maybe DominantLanguageDetectionJobProperties,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDominantLanguageDetectionJobResponse' with the minimum fields required to make a request.
--
-- * 'dominantLanguageDetectionJobProperties' - An object that contains the properties associated with a dominant language detection job.
-- * 'responseStatus' - The response status code.
mkDescribeDominantLanguageDetectionJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDominantLanguageDetectionJobResponse
mkDescribeDominantLanguageDetectionJobResponse pResponseStatus_ =
  DescribeDominantLanguageDetectionJobResponse'
    { dominantLanguageDetectionJobProperties =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that contains the properties associated with a dominant language detection job.
--
-- /Note:/ Consider using 'dominantLanguageDetectionJobProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddldjrsDominantLanguageDetectionJobProperties :: Lens.Lens' DescribeDominantLanguageDetectionJobResponse (Lude.Maybe DominantLanguageDetectionJobProperties)
ddldjrsDominantLanguageDetectionJobProperties = Lens.lens (dominantLanguageDetectionJobProperties :: DescribeDominantLanguageDetectionJobResponse -> Lude.Maybe DominantLanguageDetectionJobProperties) (\s a -> s {dominantLanguageDetectionJobProperties = a} :: DescribeDominantLanguageDetectionJobResponse)
{-# DEPRECATED ddldjrsDominantLanguageDetectionJobProperties "Use generic-lens or generic-optics with 'dominantLanguageDetectionJobProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddldjrsResponseStatus :: Lens.Lens' DescribeDominantLanguageDetectionJobResponse Lude.Int
ddldjrsResponseStatus = Lens.lens (responseStatus :: DescribeDominantLanguageDetectionJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDominantLanguageDetectionJobResponse)
{-# DEPRECATED ddldjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
