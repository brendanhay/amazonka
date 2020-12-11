{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.ListWebhooks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a listing of all the webhooks in this AWS Region for this account. The output lists all webhooks and includes the webhook URL and ARN and the configuration for each webhook.
--
-- This operation returns paginated results.
module Network.AWS.CodePipeline.ListWebhooks
  ( -- * Creating a request
    ListWebhooks (..),
    mkListWebhooks,

    -- ** Request lenses
    lwNextToken,
    lwMaxResults,

    -- * Destructuring the response
    ListWebhooksResponse (..),
    mkListWebhooksResponse,

    -- ** Response lenses
    lwrsNextToken,
    lwrsWebhooks,
    lwrsResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListWebhooks' smart constructor.
data ListWebhooks = ListWebhooks'
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

-- | Creates a value of 'ListWebhooks' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned nextToken value.
-- * 'nextToken' - The token that was returned from the previous ListWebhooks call, which can be used to return the next set of webhooks in the list.
mkListWebhooks ::
  ListWebhooks
mkListWebhooks =
  ListWebhooks'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token that was returned from the previous ListWebhooks call, which can be used to return the next set of webhooks in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwNextToken :: Lens.Lens' ListWebhooks (Lude.Maybe Lude.Text)
lwNextToken = Lens.lens (nextToken :: ListWebhooks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListWebhooks)
{-# DEPRECATED lwNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned nextToken value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwMaxResults :: Lens.Lens' ListWebhooks (Lude.Maybe Lude.Natural)
lwMaxResults = Lens.lens (maxResults :: ListWebhooks -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListWebhooks)
{-# DEPRECATED lwMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListWebhooks where
  page rq rs
    | Page.stop (rs Lens.^. lwrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lwrsWebhooks) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lwNextToken Lens..~ rs Lens.^. lwrsNextToken

instance Lude.AWSRequest ListWebhooks where
  type Rs ListWebhooks = ListWebhooksResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListWebhooksResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "webhooks" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListWebhooks where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodePipeline_20150709.ListWebhooks" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListWebhooks where
  toJSON ListWebhooks' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListWebhooks where
  toPath = Lude.const "/"

instance Lude.ToQuery ListWebhooks where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListWebhooksResponse' smart constructor.
data ListWebhooksResponse = ListWebhooksResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    webhooks :: Lude.Maybe [ListWebhookItem],
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

-- | Creates a value of 'ListWebhooksResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the amount of returned information is significantly large, an identifier is also returned and can be used in a subsequent ListWebhooks call to return the next set of webhooks in the list.
-- * 'responseStatus' - The response status code.
-- * 'webhooks' - The JSON detail returned for each webhook in the list output for the ListWebhooks call.
mkListWebhooksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListWebhooksResponse
mkListWebhooksResponse pResponseStatus_ =
  ListWebhooksResponse'
    { nextToken = Lude.Nothing,
      webhooks = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the amount of returned information is significantly large, an identifier is also returned and can be used in a subsequent ListWebhooks call to return the next set of webhooks in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwrsNextToken :: Lens.Lens' ListWebhooksResponse (Lude.Maybe Lude.Text)
lwrsNextToken = Lens.lens (nextToken :: ListWebhooksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListWebhooksResponse)
{-# DEPRECATED lwrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The JSON detail returned for each webhook in the list output for the ListWebhooks call.
--
-- /Note:/ Consider using 'webhooks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwrsWebhooks :: Lens.Lens' ListWebhooksResponse (Lude.Maybe [ListWebhookItem])
lwrsWebhooks = Lens.lens (webhooks :: ListWebhooksResponse -> Lude.Maybe [ListWebhookItem]) (\s a -> s {webhooks = a} :: ListWebhooksResponse)
{-# DEPRECATED lwrsWebhooks "Use generic-lens or generic-optics with 'webhooks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwrsResponseStatus :: Lens.Lens' ListWebhooksResponse Lude.Int
lwrsResponseStatus = Lens.lens (responseStatus :: ListWebhooksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListWebhooksResponse)
{-# DEPRECATED lwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
