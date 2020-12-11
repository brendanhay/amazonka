{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.DescribeJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a list of AWS Batch jobs.
module Network.AWS.Batch.DescribeJobs
  ( -- * Creating a request
    DescribeJobs (..),
    mkDescribeJobs,

    -- ** Request lenses
    djJobs,

    -- * Destructuring the response
    DescribeJobsResponse (..),
    mkDescribeJobsResponse,

    -- ** Response lenses
    djrsJobs,
    djrsResponseStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeJobs' smart constructor.
newtype DescribeJobs = DescribeJobs' {jobs :: [Lude.Text]}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeJobs' with the minimum fields required to make a request.
--
-- * 'jobs' - A list of up to 100 job IDs.
mkDescribeJobs ::
  DescribeJobs
mkDescribeJobs = DescribeJobs' {jobs = Lude.mempty}

-- | A list of up to 100 job IDs.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djJobs :: Lens.Lens' DescribeJobs [Lude.Text]
djJobs = Lens.lens (jobs :: DescribeJobs -> [Lude.Text]) (\s a -> s {jobs = a} :: DescribeJobs)
{-# DEPRECATED djJobs "Use generic-lens or generic-optics with 'jobs' instead." #-}

instance Lude.AWSRequest DescribeJobs where
  type Rs DescribeJobs = DescribeJobsResponse
  request = Req.postJSON batchService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeJobsResponse'
            Lude.<$> (x Lude..?> "jobs" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeJobs where
  toJSON DescribeJobs' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("jobs" Lude..= jobs)])

instance Lude.ToPath DescribeJobs where
  toPath = Lude.const "/v1/describejobs"

instance Lude.ToQuery DescribeJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeJobsResponse' smart constructor.
data DescribeJobsResponse = DescribeJobsResponse'
  { jobs ::
      Lude.Maybe [JobDetail],
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

-- | Creates a value of 'DescribeJobsResponse' with the minimum fields required to make a request.
--
-- * 'jobs' - The list of jobs.
-- * 'responseStatus' - The response status code.
mkDescribeJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeJobsResponse
mkDescribeJobsResponse pResponseStatus_ =
  DescribeJobsResponse'
    { jobs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of jobs.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djrsJobs :: Lens.Lens' DescribeJobsResponse (Lude.Maybe [JobDetail])
djrsJobs = Lens.lens (jobs :: DescribeJobsResponse -> Lude.Maybe [JobDetail]) (\s a -> s {jobs = a} :: DescribeJobsResponse)
{-# DEPRECATED djrsJobs "Use generic-lens or generic-optics with 'jobs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djrsResponseStatus :: Lens.Lens' DescribeJobsResponse Lude.Int
djrsResponseStatus = Lens.lens (responseStatus :: DescribeJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeJobsResponse)
{-# DEPRECATED djrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
