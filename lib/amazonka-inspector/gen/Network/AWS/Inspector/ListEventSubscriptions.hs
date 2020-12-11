{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.ListEventSubscriptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the event subscriptions for the assessment template that is specified by the ARN of the assessment template. For more information, see 'SubscribeToEvent' and 'UnsubscribeFromEvent' .
--
-- This operation returns paginated results.
module Network.AWS.Inspector.ListEventSubscriptions
  ( -- * Creating a request
    ListEventSubscriptions (..),
    mkListEventSubscriptions,

    -- ** Request lenses
    lesNextToken,
    lesResourceARN,
    lesMaxResults,

    -- * Destructuring the response
    ListEventSubscriptionsResponse (..),
    mkListEventSubscriptionsResponse,

    -- ** Response lenses
    lesrsNextToken,
    lesrsResponseStatus,
    lesrsSubscriptions,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListEventSubscriptions' smart constructor.
data ListEventSubscriptions = ListEventSubscriptions'
  { nextToken ::
      Lude.Maybe Lude.Text,
    resourceARN :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListEventSubscriptions' with the minimum fields required to make a request.
--
-- * 'maxResults' - You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
-- * 'nextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListEventSubscriptions__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
-- * 'resourceARN' - The ARN of the assessment template for which you want to list the existing event subscriptions.
mkListEventSubscriptions ::
  ListEventSubscriptions
mkListEventSubscriptions =
  ListEventSubscriptions'
    { nextToken = Lude.Nothing,
      resourceARN = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListEventSubscriptions__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesNextToken :: Lens.Lens' ListEventSubscriptions (Lude.Maybe Lude.Text)
lesNextToken = Lens.lens (nextToken :: ListEventSubscriptions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEventSubscriptions)
{-# DEPRECATED lesNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ARN of the assessment template for which you want to list the existing event subscriptions.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesResourceARN :: Lens.Lens' ListEventSubscriptions (Lude.Maybe Lude.Text)
lesResourceARN = Lens.lens (resourceARN :: ListEventSubscriptions -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: ListEventSubscriptions)
{-# DEPRECATED lesResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesMaxResults :: Lens.Lens' ListEventSubscriptions (Lude.Maybe Lude.Int)
lesMaxResults = Lens.lens (maxResults :: ListEventSubscriptions -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListEventSubscriptions)
{-# DEPRECATED lesMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListEventSubscriptions where
  page rq rs
    | Page.stop (rs Lens.^. lesrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lesrsSubscriptions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lesNextToken Lens..~ rs Lens.^. lesrsNextToken

instance Lude.AWSRequest ListEventSubscriptions where
  type Rs ListEventSubscriptions = ListEventSubscriptionsResponse
  request = Req.postJSON inspectorService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListEventSubscriptionsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "subscriptions" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListEventSubscriptions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.ListEventSubscriptions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListEventSubscriptions where
  toJSON ListEventSubscriptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("resourceArn" Lude..=) Lude.<$> resourceARN,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListEventSubscriptions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListEventSubscriptions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListEventSubscriptionsResponse' smart constructor.
data ListEventSubscriptionsResponse = ListEventSubscriptionsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    subscriptions ::
      [Subscription]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListEventSubscriptionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
-- * 'responseStatus' - The response status code.
-- * 'subscriptions' - Details of the returned event subscriptions.
mkListEventSubscriptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListEventSubscriptionsResponse
mkListEventSubscriptionsResponse pResponseStatus_ =
  ListEventSubscriptionsResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      subscriptions = Lude.mempty
    }

-- | When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesrsNextToken :: Lens.Lens' ListEventSubscriptionsResponse (Lude.Maybe Lude.Text)
lesrsNextToken = Lens.lens (nextToken :: ListEventSubscriptionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEventSubscriptionsResponse)
{-# DEPRECATED lesrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesrsResponseStatus :: Lens.Lens' ListEventSubscriptionsResponse Lude.Int
lesrsResponseStatus = Lens.lens (responseStatus :: ListEventSubscriptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListEventSubscriptionsResponse)
{-# DEPRECATED lesrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Details of the returned event subscriptions.
--
-- /Note:/ Consider using 'subscriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesrsSubscriptions :: Lens.Lens' ListEventSubscriptionsResponse [Subscription]
lesrsSubscriptions = Lens.lens (subscriptions :: ListEventSubscriptionsResponse -> [Subscription]) (\s a -> s {subscriptions = a} :: ListEventSubscriptionsResponse)
{-# DEPRECATED lesrsSubscriptions "Use generic-lens or generic-optics with 'subscriptions' instead." #-}
