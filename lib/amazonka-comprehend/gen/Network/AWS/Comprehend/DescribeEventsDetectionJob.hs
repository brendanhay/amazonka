{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribeEventsDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status and details of an events detection job.
module Network.AWS.Comprehend.DescribeEventsDetectionJob
  ( -- * Creating a request
    DescribeEventsDetectionJob (..),
    mkDescribeEventsDetectionJob,

    -- ** Request lenses
    dedjJobId,

    -- * Destructuring the response
    DescribeEventsDetectionJobResponse (..),
    mkDescribeEventsDetectionJobResponse,

    -- ** Response lenses
    dedjrsEventsDetectionJobProperties,
    dedjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEventsDetectionJob' smart constructor.
newtype DescribeEventsDetectionJob = DescribeEventsDetectionJob'
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

-- | Creates a value of 'DescribeEventsDetectionJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier of the events detection job.
mkDescribeEventsDetectionJob ::
  -- | 'jobId'
  Lude.Text ->
  DescribeEventsDetectionJob
mkDescribeEventsDetectionJob pJobId_ =
  DescribeEventsDetectionJob' {jobId = pJobId_}

-- | The identifier of the events detection job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedjJobId :: Lens.Lens' DescribeEventsDetectionJob Lude.Text
dedjJobId = Lens.lens (jobId :: DescribeEventsDetectionJob -> Lude.Text) (\s a -> s {jobId = a} :: DescribeEventsDetectionJob)
{-# DEPRECATED dedjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest DescribeEventsDetectionJob where
  type
    Rs DescribeEventsDetectionJob =
      DescribeEventsDetectionJobResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEventsDetectionJobResponse'
            Lude.<$> (x Lude..?> "EventsDetectionJobProperties")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEventsDetectionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.DescribeEventsDetectionJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEventsDetectionJob where
  toJSON DescribeEventsDetectionJob' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("JobId" Lude..= jobId)])

instance Lude.ToPath DescribeEventsDetectionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEventsDetectionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEventsDetectionJobResponse' smart constructor.
data DescribeEventsDetectionJobResponse = DescribeEventsDetectionJobResponse'
  { eventsDetectionJobProperties ::
      Lude.Maybe
        EventsDetectionJobProperties,
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

-- | Creates a value of 'DescribeEventsDetectionJobResponse' with the minimum fields required to make a request.
--
-- * 'eventsDetectionJobProperties' - An object that contains the properties associated with an event detection job.
-- * 'responseStatus' - The response status code.
mkDescribeEventsDetectionJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEventsDetectionJobResponse
mkDescribeEventsDetectionJobResponse pResponseStatus_ =
  DescribeEventsDetectionJobResponse'
    { eventsDetectionJobProperties =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that contains the properties associated with an event detection job.
--
-- /Note:/ Consider using 'eventsDetectionJobProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedjrsEventsDetectionJobProperties :: Lens.Lens' DescribeEventsDetectionJobResponse (Lude.Maybe EventsDetectionJobProperties)
dedjrsEventsDetectionJobProperties = Lens.lens (eventsDetectionJobProperties :: DescribeEventsDetectionJobResponse -> Lude.Maybe EventsDetectionJobProperties) (\s a -> s {eventsDetectionJobProperties = a} :: DescribeEventsDetectionJobResponse)
{-# DEPRECATED dedjrsEventsDetectionJobProperties "Use generic-lens or generic-optics with 'eventsDetectionJobProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedjrsResponseStatus :: Lens.Lens' DescribeEventsDetectionJobResponse Lude.Int
dedjrsResponseStatus = Lens.lens (responseStatus :: DescribeEventsDetectionJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEventsDetectionJobResponse)
{-# DEPRECATED dedjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
