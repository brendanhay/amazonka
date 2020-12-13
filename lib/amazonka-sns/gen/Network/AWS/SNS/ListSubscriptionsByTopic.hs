{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.ListSubscriptionsByTopic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the subscriptions to a specific topic. Each call returns a limited list of subscriptions, up to 100. If there are more subscriptions, a @NextToken@ is also returned. Use the @NextToken@ parameter in a new @ListSubscriptionsByTopic@ call to get further results.
--
-- This action is throttled at 30 transactions per second (TPS).
--
-- This operation returns paginated results.
module Network.AWS.SNS.ListSubscriptionsByTopic
  ( -- * Creating a request
    ListSubscriptionsByTopic (..),
    mkListSubscriptionsByTopic,

    -- ** Request lenses
    lsbtNextToken,
    lsbtTopicARN,

    -- * Destructuring the response
    ListSubscriptionsByTopicResponse (..),
    mkListSubscriptionsByTopicResponse,

    -- ** Response lenses
    lsbtrsNextToken,
    lsbtrsSubscriptions,
    lsbtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | Input for ListSubscriptionsByTopic action.
--
-- /See:/ 'mkListSubscriptionsByTopic' smart constructor.
data ListSubscriptionsByTopic = ListSubscriptionsByTopic'
  { -- | Token returned by the previous @ListSubscriptionsByTopic@ request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ARN of the topic for which you wish to find subscriptions.
    topicARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSubscriptionsByTopic' with the minimum fields required to make a request.
--
-- * 'nextToken' - Token returned by the previous @ListSubscriptionsByTopic@ request.
-- * 'topicARN' - The ARN of the topic for which you wish to find subscriptions.
mkListSubscriptionsByTopic ::
  -- | 'topicARN'
  Lude.Text ->
  ListSubscriptionsByTopic
mkListSubscriptionsByTopic pTopicARN_ =
  ListSubscriptionsByTopic'
    { nextToken = Lude.Nothing,
      topicARN = pTopicARN_
    }

-- | Token returned by the previous @ListSubscriptionsByTopic@ request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsbtNextToken :: Lens.Lens' ListSubscriptionsByTopic (Lude.Maybe Lude.Text)
lsbtNextToken = Lens.lens (nextToken :: ListSubscriptionsByTopic -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSubscriptionsByTopic)
{-# DEPRECATED lsbtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ARN of the topic for which you wish to find subscriptions.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsbtTopicARN :: Lens.Lens' ListSubscriptionsByTopic Lude.Text
lsbtTopicARN = Lens.lens (topicARN :: ListSubscriptionsByTopic -> Lude.Text) (\s a -> s {topicARN = a} :: ListSubscriptionsByTopic)
{-# DEPRECATED lsbtTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

instance Page.AWSPager ListSubscriptionsByTopic where
  page rq rs
    | Page.stop (rs Lens.^. lsbtrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsbtrsSubscriptions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsbtNextToken Lens..~ rs Lens.^. lsbtrsNextToken

instance Lude.AWSRequest ListSubscriptionsByTopic where
  type Rs ListSubscriptionsByTopic = ListSubscriptionsByTopicResponse
  request = Req.postQuery snsService
  response =
    Res.receiveXMLWrapper
      "ListSubscriptionsByTopicResult"
      ( \s h x ->
          ListSubscriptionsByTopicResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "Subscriptions" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSubscriptionsByTopic where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListSubscriptionsByTopic where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSubscriptionsByTopic where
  toQuery ListSubscriptionsByTopic' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListSubscriptionsByTopic" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "TopicArn" Lude.=: topicARN
      ]

-- | Response for ListSubscriptionsByTopic action.
--
-- /See:/ 'mkListSubscriptionsByTopicResponse' smart constructor.
data ListSubscriptionsByTopicResponse = ListSubscriptionsByTopicResponse'
  { -- | Token to pass along to the next @ListSubscriptionsByTopic@ request. This element is returned if there are more subscriptions to retrieve.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of subscriptions.
    subscriptions :: Lude.Maybe [Subscription],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSubscriptionsByTopicResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Token to pass along to the next @ListSubscriptionsByTopic@ request. This element is returned if there are more subscriptions to retrieve.
-- * 'subscriptions' - A list of subscriptions.
-- * 'responseStatus' - The response status code.
mkListSubscriptionsByTopicResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSubscriptionsByTopicResponse
mkListSubscriptionsByTopicResponse pResponseStatus_ =
  ListSubscriptionsByTopicResponse'
    { nextToken = Lude.Nothing,
      subscriptions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Token to pass along to the next @ListSubscriptionsByTopic@ request. This element is returned if there are more subscriptions to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsbtrsNextToken :: Lens.Lens' ListSubscriptionsByTopicResponse (Lude.Maybe Lude.Text)
lsbtrsNextToken = Lens.lens (nextToken :: ListSubscriptionsByTopicResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSubscriptionsByTopicResponse)
{-# DEPRECATED lsbtrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of subscriptions.
--
-- /Note:/ Consider using 'subscriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsbtrsSubscriptions :: Lens.Lens' ListSubscriptionsByTopicResponse (Lude.Maybe [Subscription])
lsbtrsSubscriptions = Lens.lens (subscriptions :: ListSubscriptionsByTopicResponse -> Lude.Maybe [Subscription]) (\s a -> s {subscriptions = a} :: ListSubscriptionsByTopicResponse)
{-# DEPRECATED lsbtrsSubscriptions "Use generic-lens or generic-optics with 'subscriptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsbtrsResponseStatus :: Lens.Lens' ListSubscriptionsByTopicResponse Lude.Int
lsbtrsResponseStatus = Lens.lens (responseStatus :: ListSubscriptionsByTopicResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSubscriptionsByTopicResponse)
{-# DEPRECATED lsbtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
