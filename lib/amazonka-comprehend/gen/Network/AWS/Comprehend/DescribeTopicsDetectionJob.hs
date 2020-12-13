{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribeTopicsDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a topic detection job. Use this operation to get the status of a detection job.
module Network.AWS.Comprehend.DescribeTopicsDetectionJob
  ( -- * Creating a request
    DescribeTopicsDetectionJob (..),
    mkDescribeTopicsDetectionJob,

    -- ** Request lenses
    dtdjJobId,

    -- * Destructuring the response
    DescribeTopicsDetectionJobResponse (..),
    mkDescribeTopicsDetectionJobResponse,

    -- ** Response lenses
    dtdjrsTopicsDetectionJobProperties,
    dtdjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTopicsDetectionJob' smart constructor.
newtype DescribeTopicsDetectionJob = DescribeTopicsDetectionJob'
  { -- | The identifier assigned by the user to the detection job.
    jobId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTopicsDetectionJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier assigned by the user to the detection job.
mkDescribeTopicsDetectionJob ::
  -- | 'jobId'
  Lude.Text ->
  DescribeTopicsDetectionJob
mkDescribeTopicsDetectionJob pJobId_ =
  DescribeTopicsDetectionJob' {jobId = pJobId_}

-- | The identifier assigned by the user to the detection job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtdjJobId :: Lens.Lens' DescribeTopicsDetectionJob Lude.Text
dtdjJobId = Lens.lens (jobId :: DescribeTopicsDetectionJob -> Lude.Text) (\s a -> s {jobId = a} :: DescribeTopicsDetectionJob)
{-# DEPRECATED dtdjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest DescribeTopicsDetectionJob where
  type
    Rs DescribeTopicsDetectionJob =
      DescribeTopicsDetectionJobResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTopicsDetectionJobResponse'
            Lude.<$> (x Lude..?> "TopicsDetectionJobProperties")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTopicsDetectionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.DescribeTopicsDetectionJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTopicsDetectionJob where
  toJSON DescribeTopicsDetectionJob' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("JobId" Lude..= jobId)])

instance Lude.ToPath DescribeTopicsDetectionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTopicsDetectionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeTopicsDetectionJobResponse' smart constructor.
data DescribeTopicsDetectionJobResponse = DescribeTopicsDetectionJobResponse'
  { -- | The list of properties for the requested job.
    topicsDetectionJobProperties :: Lude.Maybe TopicsDetectionJobProperties,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTopicsDetectionJobResponse' with the minimum fields required to make a request.
--
-- * 'topicsDetectionJobProperties' - The list of properties for the requested job.
-- * 'responseStatus' - The response status code.
mkDescribeTopicsDetectionJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTopicsDetectionJobResponse
mkDescribeTopicsDetectionJobResponse pResponseStatus_ =
  DescribeTopicsDetectionJobResponse'
    { topicsDetectionJobProperties =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of properties for the requested job.
--
-- /Note:/ Consider using 'topicsDetectionJobProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtdjrsTopicsDetectionJobProperties :: Lens.Lens' DescribeTopicsDetectionJobResponse (Lude.Maybe TopicsDetectionJobProperties)
dtdjrsTopicsDetectionJobProperties = Lens.lens (topicsDetectionJobProperties :: DescribeTopicsDetectionJobResponse -> Lude.Maybe TopicsDetectionJobProperties) (\s a -> s {topicsDetectionJobProperties = a} :: DescribeTopicsDetectionJobResponse)
{-# DEPRECATED dtdjrsTopicsDetectionJobProperties "Use generic-lens or generic-optics with 'topicsDetectionJobProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtdjrsResponseStatus :: Lens.Lens' DescribeTopicsDetectionJobResponse Lude.Int
dtdjrsResponseStatus = Lens.lens (responseStatus :: DescribeTopicsDetectionJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTopicsDetectionJobResponse)
{-# DEPRECATED dtdjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
