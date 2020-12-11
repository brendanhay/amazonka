{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the queues for the specified Amazon Connect instance.
--
-- For more information about queues, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-queues-standard-and-agent.html Queues: Standard and Agent> in the /Amazon Connect Administrator Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListQueues
  ( -- * Creating a request
    ListQueues (..),
    mkListQueues,

    -- ** Request lenses
    lqNextToken,
    lqQueueTypes,
    lqMaxResults,
    lqInstanceId,

    -- * Destructuring the response
    ListQueuesResponse (..),
    mkListQueuesResponse,

    -- ** Response lenses
    lqrsNextToken,
    lqrsQueueSummaryList,
    lqrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListQueues' smart constructor.
data ListQueues = ListQueues'
  { nextToken :: Lude.Maybe Lude.Text,
    queueTypes :: Lude.Maybe [QueueType],
    maxResults :: Lude.Maybe Lude.Natural,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListQueues' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'maxResults' - The maximimum number of results to return per page.
-- * 'nextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
-- * 'queueTypes' - The type of queue.
mkListQueues ::
  -- | 'instanceId'
  Lude.Text ->
  ListQueues
mkListQueues pInstanceId_ =
  ListQueues'
    { nextToken = Lude.Nothing,
      queueTypes = Lude.Nothing,
      maxResults = Lude.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqNextToken :: Lens.Lens' ListQueues (Lude.Maybe Lude.Text)
lqNextToken = Lens.lens (nextToken :: ListQueues -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListQueues)
{-# DEPRECATED lqNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The type of queue.
--
-- /Note:/ Consider using 'queueTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqQueueTypes :: Lens.Lens' ListQueues (Lude.Maybe [QueueType])
lqQueueTypes = Lens.lens (queueTypes :: ListQueues -> Lude.Maybe [QueueType]) (\s a -> s {queueTypes = a} :: ListQueues)
{-# DEPRECATED lqQueueTypes "Use generic-lens or generic-optics with 'queueTypes' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqMaxResults :: Lens.Lens' ListQueues (Lude.Maybe Lude.Natural)
lqMaxResults = Lens.lens (maxResults :: ListQueues -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListQueues)
{-# DEPRECATED lqMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqInstanceId :: Lens.Lens' ListQueues Lude.Text
lqInstanceId = Lens.lens (instanceId :: ListQueues -> Lude.Text) (\s a -> s {instanceId = a} :: ListQueues)
{-# DEPRECATED lqInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Page.AWSPager ListQueues where
  page rq rs
    | Page.stop (rs Lens.^. lqrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lqrsQueueSummaryList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lqNextToken Lens..~ rs Lens.^. lqrsNextToken

instance Lude.AWSRequest ListQueues where
  type Rs ListQueues = ListQueuesResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListQueuesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "QueueSummaryList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListQueues where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListQueues where
  toPath ListQueues' {..} =
    Lude.mconcat ["/queues-summary/", Lude.toBS instanceId]

instance Lude.ToQuery ListQueues where
  toQuery ListQueues' {..} =
    Lude.mconcat
      [ "nextToken" Lude.=: nextToken,
        "queueTypes"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> queueTypes),
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListQueuesResponse' smart constructor.
data ListQueuesResponse = ListQueuesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    queueSummaryList :: Lude.Maybe [QueueSummary],
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

-- | Creates a value of 'ListQueuesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If there are additional results, this is the token for the next set of results.
-- * 'queueSummaryList' - Information about the queues.
-- * 'responseStatus' - The response status code.
mkListQueuesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListQueuesResponse
mkListQueuesResponse pResponseStatus_ =
  ListQueuesResponse'
    { nextToken = Lude.Nothing,
      queueSummaryList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrsNextToken :: Lens.Lens' ListQueuesResponse (Lude.Maybe Lude.Text)
lqrsNextToken = Lens.lens (nextToken :: ListQueuesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListQueuesResponse)
{-# DEPRECATED lqrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the queues.
--
-- /Note:/ Consider using 'queueSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrsQueueSummaryList :: Lens.Lens' ListQueuesResponse (Lude.Maybe [QueueSummary])
lqrsQueueSummaryList = Lens.lens (queueSummaryList :: ListQueuesResponse -> Lude.Maybe [QueueSummary]) (\s a -> s {queueSummaryList = a} :: ListQueuesResponse)
{-# DEPRECATED lqrsQueueSummaryList "Use generic-lens or generic-optics with 'queueSummaryList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrsResponseStatus :: Lens.Lens' ListQueuesResponse Lude.Int
lqrsResponseStatus = Lens.lens (responseStatus :: ListQueuesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListQueuesResponse)
{-# DEPRECATED lqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
