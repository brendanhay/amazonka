{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribeEntitiesDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with an entities detection job. Use this operation to get the status of a detection job.
module Network.AWS.Comprehend.DescribeEntitiesDetectionJob
  ( -- * Creating a request
    DescribeEntitiesDetectionJob (..),
    mkDescribeEntitiesDetectionJob,

    -- ** Request lenses
    dJobId,

    -- * Destructuring the response
    DescribeEntitiesDetectionJobResponse (..),
    mkDescribeEntitiesDetectionJobResponse,

    -- ** Response lenses
    desrsEntitiesDetectionJobProperties,
    desrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEntitiesDetectionJob' smart constructor.
newtype DescribeEntitiesDetectionJob = DescribeEntitiesDetectionJob'
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

-- | Creates a value of 'DescribeEntitiesDetectionJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
mkDescribeEntitiesDetectionJob ::
  -- | 'jobId'
  Lude.Text ->
  DescribeEntitiesDetectionJob
mkDescribeEntitiesDetectionJob pJobId_ =
  DescribeEntitiesDetectionJob' {jobId = pJobId_}

-- | The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dJobId :: Lens.Lens' DescribeEntitiesDetectionJob Lude.Text
dJobId = Lens.lens (jobId :: DescribeEntitiesDetectionJob -> Lude.Text) (\s a -> s {jobId = a} :: DescribeEntitiesDetectionJob)
{-# DEPRECATED dJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest DescribeEntitiesDetectionJob where
  type
    Rs DescribeEntitiesDetectionJob =
      DescribeEntitiesDetectionJobResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEntitiesDetectionJobResponse'
            Lude.<$> (x Lude..?> "EntitiesDetectionJobProperties")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEntitiesDetectionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.DescribeEntitiesDetectionJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEntitiesDetectionJob where
  toJSON DescribeEntitiesDetectionJob' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("JobId" Lude..= jobId)])

instance Lude.ToPath DescribeEntitiesDetectionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEntitiesDetectionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEntitiesDetectionJobResponse' smart constructor.
data DescribeEntitiesDetectionJobResponse = DescribeEntitiesDetectionJobResponse'
  { entitiesDetectionJobProperties ::
      Lude.Maybe
        EntitiesDetectionJobProperties,
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

-- | Creates a value of 'DescribeEntitiesDetectionJobResponse' with the minimum fields required to make a request.
--
-- * 'entitiesDetectionJobProperties' - An object that contains the properties associated with an entities detection job.
-- * 'responseStatus' - The response status code.
mkDescribeEntitiesDetectionJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEntitiesDetectionJobResponse
mkDescribeEntitiesDetectionJobResponse pResponseStatus_ =
  DescribeEntitiesDetectionJobResponse'
    { entitiesDetectionJobProperties =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that contains the properties associated with an entities detection job.
--
-- /Note:/ Consider using 'entitiesDetectionJobProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsEntitiesDetectionJobProperties :: Lens.Lens' DescribeEntitiesDetectionJobResponse (Lude.Maybe EntitiesDetectionJobProperties)
desrsEntitiesDetectionJobProperties = Lens.lens (entitiesDetectionJobProperties :: DescribeEntitiesDetectionJobResponse -> Lude.Maybe EntitiesDetectionJobProperties) (\s a -> s {entitiesDetectionJobProperties = a} :: DescribeEntitiesDetectionJobResponse)
{-# DEPRECATED desrsEntitiesDetectionJobProperties "Use generic-lens or generic-optics with 'entitiesDetectionJobProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsResponseStatus :: Lens.Lens' DescribeEntitiesDetectionJobResponse Lude.Int
desrsResponseStatus = Lens.lens (responseStatus :: DescribeEntitiesDetectionJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEntitiesDetectionJobResponse)
{-# DEPRECATED desrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
