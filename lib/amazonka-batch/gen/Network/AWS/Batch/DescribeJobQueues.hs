{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.DescribeJobQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your job queues.
--
-- This operation returns paginated results.
module Network.AWS.Batch.DescribeJobQueues
  ( -- * Creating a request
    DescribeJobQueues (..),
    mkDescribeJobQueues,

    -- ** Request lenses
    djqNextToken,
    djqJobQueues,
    djqMaxResults,

    -- * Destructuring the response
    DescribeJobQueuesResponse (..),
    mkDescribeJobQueuesResponse,

    -- ** Response lenses
    djqsrsNextToken,
    djqsrsJobQueues,
    djqsrsResponseStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeJobQueues' smart constructor.
data DescribeJobQueues = DescribeJobQueues'
  { -- | The @nextToken@ value returned from a previous paginated @DescribeJobQueues@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of up to 100 queue names or full queue Amazon Resource Name (ARN) entries.
    jobQueues :: Lude.Maybe [Lude.Text],
    -- | The maximum number of results returned by @DescribeJobQueues@ in paginated output. When this parameter is used, @DescribeJobQueues@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeJobQueues@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @DescribeJobQueues@ returns up to 100 results and a @nextToken@ value if applicable.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeJobQueues' with the minimum fields required to make a request.
--
-- * 'nextToken' - The @nextToken@ value returned from a previous paginated @DescribeJobQueues@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
-- * 'jobQueues' - A list of up to 100 queue names or full queue Amazon Resource Name (ARN) entries.
-- * 'maxResults' - The maximum number of results returned by @DescribeJobQueues@ in paginated output. When this parameter is used, @DescribeJobQueues@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeJobQueues@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @DescribeJobQueues@ returns up to 100 results and a @nextToken@ value if applicable.
mkDescribeJobQueues ::
  DescribeJobQueues
mkDescribeJobQueues =
  DescribeJobQueues'
    { nextToken = Lude.Nothing,
      jobQueues = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The @nextToken@ value returned from a previous paginated @DescribeJobQueues@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djqNextToken :: Lens.Lens' DescribeJobQueues (Lude.Maybe Lude.Text)
djqNextToken = Lens.lens (nextToken :: DescribeJobQueues -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeJobQueues)
{-# DEPRECATED djqNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of up to 100 queue names or full queue Amazon Resource Name (ARN) entries.
--
-- /Note:/ Consider using 'jobQueues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djqJobQueues :: Lens.Lens' DescribeJobQueues (Lude.Maybe [Lude.Text])
djqJobQueues = Lens.lens (jobQueues :: DescribeJobQueues -> Lude.Maybe [Lude.Text]) (\s a -> s {jobQueues = a} :: DescribeJobQueues)
{-# DEPRECATED djqJobQueues "Use generic-lens or generic-optics with 'jobQueues' instead." #-}

-- | The maximum number of results returned by @DescribeJobQueues@ in paginated output. When this parameter is used, @DescribeJobQueues@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeJobQueues@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @DescribeJobQueues@ returns up to 100 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djqMaxResults :: Lens.Lens' DescribeJobQueues (Lude.Maybe Lude.Int)
djqMaxResults = Lens.lens (maxResults :: DescribeJobQueues -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeJobQueues)
{-# DEPRECATED djqMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeJobQueues where
  page rq rs
    | Page.stop (rs Lens.^. djqsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. djqsrsJobQueues) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& djqNextToken Lens..~ rs Lens.^. djqsrsNextToken

instance Lude.AWSRequest DescribeJobQueues where
  type Rs DescribeJobQueues = DescribeJobQueuesResponse
  request = Req.postJSON batchService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeJobQueuesResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "jobQueues" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeJobQueues where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeJobQueues where
  toJSON DescribeJobQueues' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("jobQueues" Lude..=) Lude.<$> jobQueues,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeJobQueues where
  toPath = Lude.const "/v1/describejobqueues"

instance Lude.ToQuery DescribeJobQueues where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeJobQueuesResponse' smart constructor.
data DescribeJobQueuesResponse = DescribeJobQueuesResponse'
  { -- | The @nextToken@ value to include in a future @DescribeJobQueues@ request. When the results of a @DescribeJobQueues@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The list of job queues.
    jobQueues :: Lude.Maybe [JobQueueDetail],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeJobQueuesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The @nextToken@ value to include in a future @DescribeJobQueues@ request. When the results of a @DescribeJobQueues@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'jobQueues' - The list of job queues.
-- * 'responseStatus' - The response status code.
mkDescribeJobQueuesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeJobQueuesResponse
mkDescribeJobQueuesResponse pResponseStatus_ =
  DescribeJobQueuesResponse'
    { nextToken = Lude.Nothing,
      jobQueues = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @nextToken@ value to include in a future @DescribeJobQueues@ request. When the results of a @DescribeJobQueues@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djqsrsNextToken :: Lens.Lens' DescribeJobQueuesResponse (Lude.Maybe Lude.Text)
djqsrsNextToken = Lens.lens (nextToken :: DescribeJobQueuesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeJobQueuesResponse)
{-# DEPRECATED djqsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of job queues.
--
-- /Note:/ Consider using 'jobQueues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djqsrsJobQueues :: Lens.Lens' DescribeJobQueuesResponse (Lude.Maybe [JobQueueDetail])
djqsrsJobQueues = Lens.lens (jobQueues :: DescribeJobQueuesResponse -> Lude.Maybe [JobQueueDetail]) (\s a -> s {jobQueues = a} :: DescribeJobQueuesResponse)
{-# DEPRECATED djqsrsJobQueues "Use generic-lens or generic-optics with 'jobQueues' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djqsrsResponseStatus :: Lens.Lens' DescribeJobQueuesResponse Lude.Int
djqsrsResponseStatus = Lens.lens (responseStatus :: DescribeJobQueuesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeJobQueuesResponse)
{-# DEPRECATED djqsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
