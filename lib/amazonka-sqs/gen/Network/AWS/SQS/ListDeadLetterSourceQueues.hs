{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.ListDeadLetterSourceQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of your queues that have the @RedrivePolicy@ queue attribute configured with a dead-letter queue.
--
-- The @ListDeadLetterSourceQueues@ methods supports pagination. Set parameter @MaxResults@ in the request to specify the maximum number of results to be returned in the response. If you do not set @MaxResults@ , the response includes a maximum of 1,000 results. If you set @MaxResults@ and there are additional results to display, the response includes a value for @NextToken@ . Use @NextToken@ as a parameter in your next request to @ListDeadLetterSourceQueues@ to receive the next page of results.
-- For more information about using dead-letter queues, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html Using Amazon SQS Dead-Letter Queues> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.SQS.ListDeadLetterSourceQueues
  ( -- * Creating a request
    ListDeadLetterSourceQueues (..),
    mkListDeadLetterSourceQueues,

    -- ** Request lenses
    ldlsqNextToken,
    ldlsqQueueURL,
    ldlsqMaxResults,

    -- * Destructuring the response
    ListDeadLetterSourceQueuesResponse (..),
    mkListDeadLetterSourceQueuesResponse,

    -- ** Response lenses
    ldlsqrsQueueURLs,
    ldlsqrsNextToken,
    ldlsqrsResponseStatus,
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
-- /See:/ 'mkListDeadLetterSourceQueues' smart constructor.
data ListDeadLetterSourceQueues = ListDeadLetterSourceQueues'
  { -- | Pagination token to request the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The URL of a dead-letter queue.
    --
    -- Queue URLs and names are case-sensitive.
    queueURL :: Lude.Text,
    -- | Maximum number of results to include in the response. Value range is 1 to 1000. You must set @MaxResults@ to receive a value for @NextToken@ in the response.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDeadLetterSourceQueues' with the minimum fields required to make a request.
--
-- * 'nextToken' - Pagination token to request the next set of results.
-- * 'queueURL' - The URL of a dead-letter queue.
--
-- Queue URLs and names are case-sensitive.
-- * 'maxResults' - Maximum number of results to include in the response. Value range is 1 to 1000. You must set @MaxResults@ to receive a value for @NextToken@ in the response.
mkListDeadLetterSourceQueues ::
  -- | 'queueURL'
  Lude.Text ->
  ListDeadLetterSourceQueues
mkListDeadLetterSourceQueues pQueueURL_ =
  ListDeadLetterSourceQueues'
    { nextToken = Lude.Nothing,
      queueURL = pQueueURL_,
      maxResults = Lude.Nothing
    }

-- | Pagination token to request the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldlsqNextToken :: Lens.Lens' ListDeadLetterSourceQueues (Lude.Maybe Lude.Text)
ldlsqNextToken = Lens.lens (nextToken :: ListDeadLetterSourceQueues -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDeadLetterSourceQueues)
{-# DEPRECATED ldlsqNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The URL of a dead-letter queue.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldlsqQueueURL :: Lens.Lens' ListDeadLetterSourceQueues Lude.Text
ldlsqQueueURL = Lens.lens (queueURL :: ListDeadLetterSourceQueues -> Lude.Text) (\s a -> s {queueURL = a} :: ListDeadLetterSourceQueues)
{-# DEPRECATED ldlsqQueueURL "Use generic-lens or generic-optics with 'queueURL' instead." #-}

-- | Maximum number of results to include in the response. Value range is 1 to 1000. You must set @MaxResults@ to receive a value for @NextToken@ in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldlsqMaxResults :: Lens.Lens' ListDeadLetterSourceQueues (Lude.Maybe Lude.Int)
ldlsqMaxResults = Lens.lens (maxResults :: ListDeadLetterSourceQueues -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListDeadLetterSourceQueues)
{-# DEPRECATED ldlsqMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListDeadLetterSourceQueues where
  page rq rs
    | Page.stop (rs Lens.^. ldlsqrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldlsqrsQueueURLs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldlsqNextToken Lens..~ rs Lens.^. ldlsqrsNextToken

instance Lude.AWSRequest ListDeadLetterSourceQueues where
  type
    Rs ListDeadLetterSourceQueues =
      ListDeadLetterSourceQueuesResponse
  request = Req.postQuery sqsService
  response =
    Res.receiveXMLWrapper
      "ListDeadLetterSourceQueuesResult"
      ( \s h x ->
          ListDeadLetterSourceQueuesResponse'
            Lude.<$> (Lude.parseXMLList "QueueUrl" x)
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDeadLetterSourceQueues where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListDeadLetterSourceQueues where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDeadLetterSourceQueues where
  toQuery ListDeadLetterSourceQueues' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ListDeadLetterSourceQueues" :: Lude.ByteString),
        "Version" Lude.=: ("2012-11-05" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "QueueUrl" Lude.=: queueURL,
        "MaxResults" Lude.=: maxResults
      ]

-- | A list of your dead letter source queues.
--
-- /See:/ 'mkListDeadLetterSourceQueuesResponse' smart constructor.
data ListDeadLetterSourceQueuesResponse = ListDeadLetterSourceQueuesResponse'
  { -- | A list of source queue URLs that have the @RedrivePolicy@ queue attribute configured with a dead-letter queue.
    queueURLs :: [Lude.Text],
    -- | Pagination token to include in the next request. Token value is @null@ if there are no additional results to request, or if you did not set @MaxResults@ in the request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDeadLetterSourceQueuesResponse' with the minimum fields required to make a request.
--
-- * 'queueURLs' - A list of source queue URLs that have the @RedrivePolicy@ queue attribute configured with a dead-letter queue.
-- * 'nextToken' - Pagination token to include in the next request. Token value is @null@ if there are no additional results to request, or if you did not set @MaxResults@ in the request.
-- * 'responseStatus' - The response status code.
mkListDeadLetterSourceQueuesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDeadLetterSourceQueuesResponse
mkListDeadLetterSourceQueuesResponse pResponseStatus_ =
  ListDeadLetterSourceQueuesResponse'
    { queueURLs = Lude.mempty,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of source queue URLs that have the @RedrivePolicy@ queue attribute configured with a dead-letter queue.
--
-- /Note:/ Consider using 'queueURLs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldlsqrsQueueURLs :: Lens.Lens' ListDeadLetterSourceQueuesResponse [Lude.Text]
ldlsqrsQueueURLs = Lens.lens (queueURLs :: ListDeadLetterSourceQueuesResponse -> [Lude.Text]) (\s a -> s {queueURLs = a} :: ListDeadLetterSourceQueuesResponse)
{-# DEPRECATED ldlsqrsQueueURLs "Use generic-lens or generic-optics with 'queueURLs' instead." #-}

-- | Pagination token to include in the next request. Token value is @null@ if there are no additional results to request, or if you did not set @MaxResults@ in the request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldlsqrsNextToken :: Lens.Lens' ListDeadLetterSourceQueuesResponse (Lude.Maybe Lude.Text)
ldlsqrsNextToken = Lens.lens (nextToken :: ListDeadLetterSourceQueuesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDeadLetterSourceQueuesResponse)
{-# DEPRECATED ldlsqrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldlsqrsResponseStatus :: Lens.Lens' ListDeadLetterSourceQueuesResponse Lude.Int
ldlsqrsResponseStatus = Lens.lens (responseStatus :: ListDeadLetterSourceQueuesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDeadLetterSourceQueuesResponse)
{-# DEPRECATED ldlsqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
