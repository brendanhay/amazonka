{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListTopicRuleDestinations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the topic rule destinations in your AWS account.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListTopicRuleDestinations
  ( -- * Creating a request
    ListTopicRuleDestinations (..),
    mkListTopicRuleDestinations,

    -- ** Request lenses
    ltrdNextToken,
    ltrdMaxResults,

    -- * Destructuring the response
    ListTopicRuleDestinationsResponse (..),
    mkListTopicRuleDestinationsResponse,

    -- ** Response lenses
    ltrdrsDestinationSummaries,
    ltrdrsNextToken,
    ltrdrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTopicRuleDestinations' smart constructor.
data ListTopicRuleDestinations = ListTopicRuleDestinations'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTopicRuleDestinations' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to return at one time.
-- * 'nextToken' - To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
mkListTopicRuleDestinations ::
  ListTopicRuleDestinations
mkListTopicRuleDestinations =
  ListTopicRuleDestinations'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrdNextToken :: Lens.Lens' ListTopicRuleDestinations (Lude.Maybe Lude.Text)
ltrdNextToken = Lens.lens (nextToken :: ListTopicRuleDestinations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTopicRuleDestinations)
{-# DEPRECATED ltrdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrdMaxResults :: Lens.Lens' ListTopicRuleDestinations (Lude.Maybe Lude.Natural)
ltrdMaxResults = Lens.lens (maxResults :: ListTopicRuleDestinations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTopicRuleDestinations)
{-# DEPRECATED ltrdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListTopicRuleDestinations where
  page rq rs
    | Page.stop (rs Lens.^. ltrdrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltrdrsDestinationSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltrdNextToken Lens..~ rs Lens.^. ltrdrsNextToken

instance Lude.AWSRequest ListTopicRuleDestinations where
  type
    Rs ListTopicRuleDestinations =
      ListTopicRuleDestinationsResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTopicRuleDestinationsResponse'
            Lude.<$> (x Lude..?> "destinationSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTopicRuleDestinations where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListTopicRuleDestinations where
  toPath = Lude.const "/destinations"

instance Lude.ToQuery ListTopicRuleDestinations where
  toQuery ListTopicRuleDestinations' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListTopicRuleDestinationsResponse' smart constructor.
data ListTopicRuleDestinationsResponse = ListTopicRuleDestinationsResponse'
  { destinationSummaries ::
      Lude.Maybe
        [TopicRuleDestinationSummary],
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

-- | Creates a value of 'ListTopicRuleDestinationsResponse' with the minimum fields required to make a request.
--
-- * 'destinationSummaries' - Information about a topic rule destination.
-- * 'nextToken' - The token to use to get the next set of results, or __null__ if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListTopicRuleDestinationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTopicRuleDestinationsResponse
mkListTopicRuleDestinationsResponse pResponseStatus_ =
  ListTopicRuleDestinationsResponse'
    { destinationSummaries =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about a topic rule destination.
--
-- /Note:/ Consider using 'destinationSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrdrsDestinationSummaries :: Lens.Lens' ListTopicRuleDestinationsResponse (Lude.Maybe [TopicRuleDestinationSummary])
ltrdrsDestinationSummaries = Lens.lens (destinationSummaries :: ListTopicRuleDestinationsResponse -> Lude.Maybe [TopicRuleDestinationSummary]) (\s a -> s {destinationSummaries = a} :: ListTopicRuleDestinationsResponse)
{-# DEPRECATED ltrdrsDestinationSummaries "Use generic-lens or generic-optics with 'destinationSummaries' instead." #-}

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrdrsNextToken :: Lens.Lens' ListTopicRuleDestinationsResponse (Lude.Maybe Lude.Text)
ltrdrsNextToken = Lens.lens (nextToken :: ListTopicRuleDestinationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTopicRuleDestinationsResponse)
{-# DEPRECATED ltrdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrdrsResponseStatus :: Lens.Lens' ListTopicRuleDestinationsResponse Lude.Int
ltrdrsResponseStatus = Lens.lens (responseStatus :: ListTopicRuleDestinationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTopicRuleDestinationsResponse)
{-# DEPRECATED ltrdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
