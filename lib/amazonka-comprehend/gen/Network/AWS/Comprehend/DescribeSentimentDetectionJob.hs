{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribeSentimentDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a sentiment detection job. Use this operation to get the status of a detection job.
module Network.AWS.Comprehend.DescribeSentimentDetectionJob
  ( -- * Creating a request
    DescribeSentimentDetectionJob (..),
    mkDescribeSentimentDetectionJob,

    -- ** Request lenses
    dsdjJobId,

    -- * Destructuring the response
    DescribeSentimentDetectionJobResponse (..),
    mkDescribeSentimentDetectionJobResponse,

    -- ** Response lenses
    dsdjrsSentimentDetectionJobProperties,
    dsdjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeSentimentDetectionJob' smart constructor.
newtype DescribeSentimentDetectionJob = DescribeSentimentDetectionJob'
  { -- | The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
    jobId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSentimentDetectionJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
mkDescribeSentimentDetectionJob ::
  -- | 'jobId'
  Lude.Text ->
  DescribeSentimentDetectionJob
mkDescribeSentimentDetectionJob pJobId_ =
  DescribeSentimentDetectionJob' {jobId = pJobId_}

-- | The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdjJobId :: Lens.Lens' DescribeSentimentDetectionJob Lude.Text
dsdjJobId = Lens.lens (jobId :: DescribeSentimentDetectionJob -> Lude.Text) (\s a -> s {jobId = a} :: DescribeSentimentDetectionJob)
{-# DEPRECATED dsdjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest DescribeSentimentDetectionJob where
  type
    Rs DescribeSentimentDetectionJob =
      DescribeSentimentDetectionJobResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeSentimentDetectionJobResponse'
            Lude.<$> (x Lude..?> "SentimentDetectionJobProperties")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSentimentDetectionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.DescribeSentimentDetectionJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeSentimentDetectionJob where
  toJSON DescribeSentimentDetectionJob' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("JobId" Lude..= jobId)])

instance Lude.ToPath DescribeSentimentDetectionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSentimentDetectionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeSentimentDetectionJobResponse' smart constructor.
data DescribeSentimentDetectionJobResponse = DescribeSentimentDetectionJobResponse'
  { -- | An object that contains the properties associated with a sentiment detection job.
    sentimentDetectionJobProperties :: Lude.Maybe SentimentDetectionJobProperties,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSentimentDetectionJobResponse' with the minimum fields required to make a request.
--
-- * 'sentimentDetectionJobProperties' - An object that contains the properties associated with a sentiment detection job.
-- * 'responseStatus' - The response status code.
mkDescribeSentimentDetectionJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSentimentDetectionJobResponse
mkDescribeSentimentDetectionJobResponse pResponseStatus_ =
  DescribeSentimentDetectionJobResponse'
    { sentimentDetectionJobProperties =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that contains the properties associated with a sentiment detection job.
--
-- /Note:/ Consider using 'sentimentDetectionJobProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdjrsSentimentDetectionJobProperties :: Lens.Lens' DescribeSentimentDetectionJobResponse (Lude.Maybe SentimentDetectionJobProperties)
dsdjrsSentimentDetectionJobProperties = Lens.lens (sentimentDetectionJobProperties :: DescribeSentimentDetectionJobResponse -> Lude.Maybe SentimentDetectionJobProperties) (\s a -> s {sentimentDetectionJobProperties = a} :: DescribeSentimentDetectionJobResponse)
{-# DEPRECATED dsdjrsSentimentDetectionJobProperties "Use generic-lens or generic-optics with 'sentimentDetectionJobProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdjrsResponseStatus :: Lens.Lens' DescribeSentimentDetectionJobResponse Lude.Int
dsdjrsResponseStatus = Lens.lens (responseStatus :: DescribeSentimentDetectionJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSentimentDetectionJobResponse)
{-# DEPRECATED dsdjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
