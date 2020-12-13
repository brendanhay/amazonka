{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.ListQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of your queues in the current region. The response includes a maximum of 1,000 results. If you specify a value for the optional @QueueNamePrefix@ parameter, only queues with a name that begins with the specified value are returned.
--
-- The @listQueues@ methods supports pagination. Set parameter @MaxResults@ in the request to specify the maximum number of results to be returned in the response. If you do not set @MaxResults@ , the response includes a maximum of 1,000 results. If you set @MaxResults@ and there are additional results to display, the response includes a value for @NextToken@ . Use @NextToken@ as a parameter in your next request to @listQueues@ to receive the next page of results.
--
-- This operation returns paginated results.
module Network.AWS.SQS.ListQueues
  ( -- * Creating a request
    ListQueues (..),
    mkListQueues,

    -- ** Request lenses
    lqQueueNamePrefix,
    lqNextToken,
    lqMaxResults,

    -- * Destructuring the response
    ListQueuesResponse (..),
    mkListQueuesResponse,

    -- ** Response lenses
    lqrsQueueURLs,
    lqrsNextToken,
    lqrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SQS.Types

-- |
--
-- /See:/ 'mkListQueues' smart constructor.
data ListQueues = ListQueues'
  { -- | A string to use for filtering the list results. Only those queues whose name begins with the specified string are returned.
    --
    -- Queue URLs and names are case-sensitive.
    queueNamePrefix :: Lude.Maybe Lude.Text,
    -- | Pagination token to request the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Maximum number of results to include in the response. Value range is 1 to 1000. You must set @MaxResults@ to receive a value for @NextToken@ in the response.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListQueues' with the minimum fields required to make a request.
--
-- * 'queueNamePrefix' - A string to use for filtering the list results. Only those queues whose name begins with the specified string are returned.
--
-- Queue URLs and names are case-sensitive.
-- * 'nextToken' - Pagination token to request the next set of results.
-- * 'maxResults' - Maximum number of results to include in the response. Value range is 1 to 1000. You must set @MaxResults@ to receive a value for @NextToken@ in the response.
mkListQueues ::
  ListQueues
mkListQueues =
  ListQueues'
    { queueNamePrefix = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A string to use for filtering the list results. Only those queues whose name begins with the specified string are returned.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqQueueNamePrefix :: Lens.Lens' ListQueues (Lude.Maybe Lude.Text)
lqQueueNamePrefix = Lens.lens (queueNamePrefix :: ListQueues -> Lude.Maybe Lude.Text) (\s a -> s {queueNamePrefix = a} :: ListQueues)
{-# DEPRECATED lqQueueNamePrefix "Use generic-lens or generic-optics with 'queueNamePrefix' instead." #-}

-- | Pagination token to request the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqNextToken :: Lens.Lens' ListQueues (Lude.Maybe Lude.Text)
lqNextToken = Lens.lens (nextToken :: ListQueues -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListQueues)
{-# DEPRECATED lqNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Maximum number of results to include in the response. Value range is 1 to 1000. You must set @MaxResults@ to receive a value for @NextToken@ in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqMaxResults :: Lens.Lens' ListQueues (Lude.Maybe Lude.Int)
lqMaxResults = Lens.lens (maxResults :: ListQueues -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListQueues)
{-# DEPRECATED lqMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListQueues where
  page rq rs
    | Page.stop (rs Lens.^. lqrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lqrsQueueURLs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lqNextToken Lens..~ rs Lens.^. lqrsNextToken

instance Lude.AWSRequest ListQueues where
  type Rs ListQueues = ListQueuesResponse
  request = Req.postQuery sqsService
  response =
    Res.receiveXMLWrapper
      "ListQueuesResult"
      ( \s h x ->
          ListQueuesResponse'
            Lude.<$> (Lude.may (Lude.parseXMLList "QueueUrl") x)
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListQueues where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListQueues where
  toPath = Lude.const "/"

instance Lude.ToQuery ListQueues where
  toQuery ListQueues' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListQueues" :: Lude.ByteString),
        "Version" Lude.=: ("2012-11-05" :: Lude.ByteString),
        "QueueNamePrefix" Lude.=: queueNamePrefix,
        "NextToken" Lude.=: nextToken,
        "MaxResults" Lude.=: maxResults
      ]

-- | A list of your queues.
--
-- /See:/ 'mkListQueuesResponse' smart constructor.
data ListQueuesResponse = ListQueuesResponse'
  { -- | A list of queue URLs, up to 1,000 entries, or the value of MaxResults that you sent in the request.
    queueURLs :: Lude.Maybe [Lude.Text],
    -- | Pagination token to include in the next request. Token value is @null@ if there are no additional results to request, or if you did not set @MaxResults@ in the request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListQueuesResponse' with the minimum fields required to make a request.
--
-- * 'queueURLs' - A list of queue URLs, up to 1,000 entries, or the value of MaxResults that you sent in the request.
-- * 'nextToken' - Pagination token to include in the next request. Token value is @null@ if there are no additional results to request, or if you did not set @MaxResults@ in the request.
-- * 'responseStatus' - The response status code.
mkListQueuesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListQueuesResponse
mkListQueuesResponse pResponseStatus_ =
  ListQueuesResponse'
    { queueURLs = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of queue URLs, up to 1,000 entries, or the value of MaxResults that you sent in the request.
--
-- /Note:/ Consider using 'queueURLs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrsQueueURLs :: Lens.Lens' ListQueuesResponse (Lude.Maybe [Lude.Text])
lqrsQueueURLs = Lens.lens (queueURLs :: ListQueuesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {queueURLs = a} :: ListQueuesResponse)
{-# DEPRECATED lqrsQueueURLs "Use generic-lens or generic-optics with 'queueURLs' instead." #-}

-- | Pagination token to include in the next request. Token value is @null@ if there are no additional results to request, or if you did not set @MaxResults@ in the request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrsNextToken :: Lens.Lens' ListQueuesResponse (Lude.Maybe Lude.Text)
lqrsNextToken = Lens.lens (nextToken :: ListQueuesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListQueuesResponse)
{-# DEPRECATED lqrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrsResponseStatus :: Lens.Lens' ListQueuesResponse Lude.Int
lqrsResponseStatus = Lens.lens (responseStatus :: ListQueuesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListQueuesResponse)
{-# DEPRECATED lqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
