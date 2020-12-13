{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.ListSubscriptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the requester's subscriptions. Each call returns a limited list of subscriptions, up to 100. If there are more subscriptions, a @NextToken@ is also returned. Use the @NextToken@ parameter in a new @ListSubscriptions@ call to get further results.
--
-- This action is throttled at 30 transactions per second (TPS).
--
-- This operation returns paginated results.
module Network.AWS.SNS.ListSubscriptions
  ( -- * Creating a request
    ListSubscriptions (..),
    mkListSubscriptions,

    -- ** Request lenses
    lsNextToken,

    -- * Destructuring the response
    ListSubscriptionsResponse (..),
    mkListSubscriptionsResponse,

    -- ** Response lenses
    lsrsNextToken,
    lsrsSubscriptions,
    lsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | Input for ListSubscriptions action.
--
-- /See:/ 'mkListSubscriptions' smart constructor.
newtype ListSubscriptions = ListSubscriptions'
  { -- | Token returned by the previous @ListSubscriptions@ request.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSubscriptions' with the minimum fields required to make a request.
--
-- * 'nextToken' - Token returned by the previous @ListSubscriptions@ request.
mkListSubscriptions ::
  ListSubscriptions
mkListSubscriptions = ListSubscriptions' {nextToken = Lude.Nothing}

-- | Token returned by the previous @ListSubscriptions@ request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsNextToken :: Lens.Lens' ListSubscriptions (Lude.Maybe Lude.Text)
lsNextToken = Lens.lens (nextToken :: ListSubscriptions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSubscriptions)
{-# DEPRECATED lsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListSubscriptions where
  page rq rs
    | Page.stop (rs Lens.^. lsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsrsSubscriptions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsNextToken Lens..~ rs Lens.^. lsrsNextToken

instance Lude.AWSRequest ListSubscriptions where
  type Rs ListSubscriptions = ListSubscriptionsResponse
  request = Req.postQuery snsService
  response =
    Res.receiveXMLWrapper
      "ListSubscriptionsResult"
      ( \s h x ->
          ListSubscriptionsResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "Subscriptions" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSubscriptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListSubscriptions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSubscriptions where
  toQuery ListSubscriptions' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListSubscriptions" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken
      ]

-- | Response for ListSubscriptions action
--
-- /See:/ 'mkListSubscriptionsResponse' smart constructor.
data ListSubscriptionsResponse = ListSubscriptionsResponse'
  { -- | Token to pass along to the next @ListSubscriptions@ request. This element is returned if there are more subscriptions to retrieve.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of subscriptions.
    subscriptions :: Lude.Maybe [Subscription],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSubscriptionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Token to pass along to the next @ListSubscriptions@ request. This element is returned if there are more subscriptions to retrieve.
-- * 'subscriptions' - A list of subscriptions.
-- * 'responseStatus' - The response status code.
mkListSubscriptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSubscriptionsResponse
mkListSubscriptionsResponse pResponseStatus_ =
  ListSubscriptionsResponse'
    { nextToken = Lude.Nothing,
      subscriptions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Token to pass along to the next @ListSubscriptions@ request. This element is returned if there are more subscriptions to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsNextToken :: Lens.Lens' ListSubscriptionsResponse (Lude.Maybe Lude.Text)
lsrsNextToken = Lens.lens (nextToken :: ListSubscriptionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSubscriptionsResponse)
{-# DEPRECATED lsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of subscriptions.
--
-- /Note:/ Consider using 'subscriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsSubscriptions :: Lens.Lens' ListSubscriptionsResponse (Lude.Maybe [Subscription])
lsrsSubscriptions = Lens.lens (subscriptions :: ListSubscriptionsResponse -> Lude.Maybe [Subscription]) (\s a -> s {subscriptions = a} :: ListSubscriptionsResponse)
{-# DEPRECATED lsrsSubscriptions "Use generic-lens or generic-optics with 'subscriptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsResponseStatus :: Lens.Lens' ListSubscriptionsResponse Lude.Int
lsrsResponseStatus = Lens.lens (responseStatus :: ListSubscriptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSubscriptionsResponse)
{-# DEPRECATED lsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
