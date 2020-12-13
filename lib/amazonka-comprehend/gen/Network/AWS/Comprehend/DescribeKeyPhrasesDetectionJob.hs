{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribeKeyPhrasesDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a key phrases detection job. Use this operation to get the status of a detection job.
module Network.AWS.Comprehend.DescribeKeyPhrasesDetectionJob
  ( -- * Creating a request
    DescribeKeyPhrasesDetectionJob (..),
    mkDescribeKeyPhrasesDetectionJob,

    -- ** Request lenses
    dkpdjJobId,

    -- * Destructuring the response
    DescribeKeyPhrasesDetectionJobResponse (..),
    mkDescribeKeyPhrasesDetectionJobResponse,

    -- ** Response lenses
    dkpdjrsKeyPhrasesDetectionJobProperties,
    dkpdjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeKeyPhrasesDetectionJob' smart constructor.
newtype DescribeKeyPhrasesDetectionJob = DescribeKeyPhrasesDetectionJob'
  { -- | The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
    jobId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeKeyPhrasesDetectionJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
mkDescribeKeyPhrasesDetectionJob ::
  -- | 'jobId'
  Lude.Text ->
  DescribeKeyPhrasesDetectionJob
mkDescribeKeyPhrasesDetectionJob pJobId_ =
  DescribeKeyPhrasesDetectionJob' {jobId = pJobId_}

-- | The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpdjJobId :: Lens.Lens' DescribeKeyPhrasesDetectionJob Lude.Text
dkpdjJobId = Lens.lens (jobId :: DescribeKeyPhrasesDetectionJob -> Lude.Text) (\s a -> s {jobId = a} :: DescribeKeyPhrasesDetectionJob)
{-# DEPRECATED dkpdjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest DescribeKeyPhrasesDetectionJob where
  type
    Rs DescribeKeyPhrasesDetectionJob =
      DescribeKeyPhrasesDetectionJobResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeKeyPhrasesDetectionJobResponse'
            Lude.<$> (x Lude..?> "KeyPhrasesDetectionJobProperties")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeKeyPhrasesDetectionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.DescribeKeyPhrasesDetectionJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeKeyPhrasesDetectionJob where
  toJSON DescribeKeyPhrasesDetectionJob' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("JobId" Lude..= jobId)])

instance Lude.ToPath DescribeKeyPhrasesDetectionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeKeyPhrasesDetectionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeKeyPhrasesDetectionJobResponse' smart constructor.
data DescribeKeyPhrasesDetectionJobResponse = DescribeKeyPhrasesDetectionJobResponse'
  { -- | An object that contains the properties associated with a key phrases detection job.
    keyPhrasesDetectionJobProperties :: Lude.Maybe KeyPhrasesDetectionJobProperties,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeKeyPhrasesDetectionJobResponse' with the minimum fields required to make a request.
--
-- * 'keyPhrasesDetectionJobProperties' - An object that contains the properties associated with a key phrases detection job.
-- * 'responseStatus' - The response status code.
mkDescribeKeyPhrasesDetectionJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeKeyPhrasesDetectionJobResponse
mkDescribeKeyPhrasesDetectionJobResponse pResponseStatus_ =
  DescribeKeyPhrasesDetectionJobResponse'
    { keyPhrasesDetectionJobProperties =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that contains the properties associated with a key phrases detection job.
--
-- /Note:/ Consider using 'keyPhrasesDetectionJobProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpdjrsKeyPhrasesDetectionJobProperties :: Lens.Lens' DescribeKeyPhrasesDetectionJobResponse (Lude.Maybe KeyPhrasesDetectionJobProperties)
dkpdjrsKeyPhrasesDetectionJobProperties = Lens.lens (keyPhrasesDetectionJobProperties :: DescribeKeyPhrasesDetectionJobResponse -> Lude.Maybe KeyPhrasesDetectionJobProperties) (\s a -> s {keyPhrasesDetectionJobProperties = a} :: DescribeKeyPhrasesDetectionJobResponse)
{-# DEPRECATED dkpdjrsKeyPhrasesDetectionJobProperties "Use generic-lens or generic-optics with 'keyPhrasesDetectionJobProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpdjrsResponseStatus :: Lens.Lens' DescribeKeyPhrasesDetectionJobResponse Lude.Int
dkpdjrsResponseStatus = Lens.lens (responseStatus :: DescribeKeyPhrasesDetectionJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeKeyPhrasesDetectionJobResponse)
{-# DEPRECATED dkpdjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
