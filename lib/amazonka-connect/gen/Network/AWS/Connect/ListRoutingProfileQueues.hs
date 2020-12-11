{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListRoutingProfileQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the queues associated with a routing profile.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListRoutingProfileQueues
  ( -- * Creating a request
    ListRoutingProfileQueues (..),
    mkListRoutingProfileQueues,

    -- ** Request lenses
    lrpqNextToken,
    lrpqMaxResults,
    lrpqInstanceId,
    lrpqRoutingProfileId,

    -- * Destructuring the response
    ListRoutingProfileQueuesResponse (..),
    mkListRoutingProfileQueuesResponse,

    -- ** Response lenses
    lrpqrsRoutingProfileQueueConfigSummaryList,
    lrpqrsNextToken,
    lrpqrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListRoutingProfileQueues' smart constructor.
data ListRoutingProfileQueues = ListRoutingProfileQueues'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    instanceId :: Lude.Text,
    routingProfileId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRoutingProfileQueues' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'maxResults' - The maximimum number of results to return per page.
-- * 'nextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
-- * 'routingProfileId' - The identifier of the routing profile.
mkListRoutingProfileQueues ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'routingProfileId'
  Lude.Text ->
  ListRoutingProfileQueues
mkListRoutingProfileQueues pInstanceId_ pRoutingProfileId_ =
  ListRoutingProfileQueues'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      instanceId = pInstanceId_,
      routingProfileId = pRoutingProfileId_
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpqNextToken :: Lens.Lens' ListRoutingProfileQueues (Lude.Maybe Lude.Text)
lrpqNextToken = Lens.lens (nextToken :: ListRoutingProfileQueues -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRoutingProfileQueues)
{-# DEPRECATED lrpqNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpqMaxResults :: Lens.Lens' ListRoutingProfileQueues (Lude.Maybe Lude.Natural)
lrpqMaxResults = Lens.lens (maxResults :: ListRoutingProfileQueues -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListRoutingProfileQueues)
{-# DEPRECATED lrpqMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpqInstanceId :: Lens.Lens' ListRoutingProfileQueues Lude.Text
lrpqInstanceId = Lens.lens (instanceId :: ListRoutingProfileQueues -> Lude.Text) (\s a -> s {instanceId = a} :: ListRoutingProfileQueues)
{-# DEPRECATED lrpqInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the routing profile.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpqRoutingProfileId :: Lens.Lens' ListRoutingProfileQueues Lude.Text
lrpqRoutingProfileId = Lens.lens (routingProfileId :: ListRoutingProfileQueues -> Lude.Text) (\s a -> s {routingProfileId = a} :: ListRoutingProfileQueues)
{-# DEPRECATED lrpqRoutingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead." #-}

instance Page.AWSPager ListRoutingProfileQueues where
  page rq rs
    | Page.stop (rs Lens.^. lrpqrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrpqrsRoutingProfileQueueConfigSummaryList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrpqNextToken Lens..~ rs Lens.^. lrpqrsNextToken

instance Lude.AWSRequest ListRoutingProfileQueues where
  type Rs ListRoutingProfileQueues = ListRoutingProfileQueuesResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListRoutingProfileQueuesResponse'
            Lude.<$> ( x Lude..?> "RoutingProfileQueueConfigSummaryList"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListRoutingProfileQueues where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListRoutingProfileQueues where
  toPath ListRoutingProfileQueues' {..} =
    Lude.mconcat
      [ "/routing-profiles/",
        Lude.toBS instanceId,
        "/",
        Lude.toBS routingProfileId,
        "/queues"
      ]

instance Lude.ToQuery ListRoutingProfileQueues where
  toQuery ListRoutingProfileQueues' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListRoutingProfileQueuesResponse' smart constructor.
data ListRoutingProfileQueuesResponse = ListRoutingProfileQueuesResponse'
  { routingProfileQueueConfigSummaryList ::
      Lude.Maybe
        [RoutingProfileQueueConfigSummary],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListRoutingProfileQueuesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If there are additional results, this is the token for the next set of results.
-- * 'responseStatus' - The response status code.
-- * 'routingProfileQueueConfigSummaryList' - Information about the routing profiles.
mkListRoutingProfileQueuesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListRoutingProfileQueuesResponse
mkListRoutingProfileQueuesResponse pResponseStatus_ =
  ListRoutingProfileQueuesResponse'
    { routingProfileQueueConfigSummaryList =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the routing profiles.
--
-- /Note:/ Consider using 'routingProfileQueueConfigSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpqrsRoutingProfileQueueConfigSummaryList :: Lens.Lens' ListRoutingProfileQueuesResponse (Lude.Maybe [RoutingProfileQueueConfigSummary])
lrpqrsRoutingProfileQueueConfigSummaryList = Lens.lens (routingProfileQueueConfigSummaryList :: ListRoutingProfileQueuesResponse -> Lude.Maybe [RoutingProfileQueueConfigSummary]) (\s a -> s {routingProfileQueueConfigSummaryList = a} :: ListRoutingProfileQueuesResponse)
{-# DEPRECATED lrpqrsRoutingProfileQueueConfigSummaryList "Use generic-lens or generic-optics with 'routingProfileQueueConfigSummaryList' instead." #-}

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpqrsNextToken :: Lens.Lens' ListRoutingProfileQueuesResponse (Lude.Maybe Lude.Text)
lrpqrsNextToken = Lens.lens (nextToken :: ListRoutingProfileQueuesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRoutingProfileQueuesResponse)
{-# DEPRECATED lrpqrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpqrsResponseStatus :: Lens.Lens' ListRoutingProfileQueuesResponse Lude.Int
lrpqrsResponseStatus = Lens.lens (responseStatus :: ListRoutingProfileQueuesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListRoutingProfileQueuesResponse)
{-# DEPRECATED lrpqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
