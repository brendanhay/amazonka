{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.ListBrokers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all brokers.
--
-- This operation returns paginated results.
module Network.AWS.MQ.ListBrokers
  ( -- * Creating a request
    ListBrokers (..),
    mkListBrokers,

    -- ** Request lenses
    lbNextToken,
    lbMaxResults,

    -- * Destructuring the response
    ListBrokersResponse (..),
    mkListBrokersResponse,

    -- ** Response lenses
    lbrsNextToken,
    lbrsBrokerSummaries,
    lbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListBrokers' smart constructor.
data ListBrokers = ListBrokers'
  { nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListBrokers' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of brokers that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
-- * 'nextToken' - The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
mkListBrokers ::
  ListBrokers
mkListBrokers =
  ListBrokers' {nextToken = Lude.Nothing, maxResults = Lude.Nothing}

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbNextToken :: Lens.Lens' ListBrokers (Lude.Maybe Lude.Text)
lbNextToken = Lens.lens (nextToken :: ListBrokers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBrokers)
{-# DEPRECATED lbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of brokers that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbMaxResults :: Lens.Lens' ListBrokers (Lude.Maybe Lude.Natural)
lbMaxResults = Lens.lens (maxResults :: ListBrokers -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListBrokers)
{-# DEPRECATED lbMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListBrokers where
  page rq rs
    | Page.stop (rs Lens.^. lbrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lbrsBrokerSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lbNextToken Lens..~ rs Lens.^. lbrsNextToken

instance Lude.AWSRequest ListBrokers where
  type Rs ListBrokers = ListBrokersResponse
  request = Req.get mqService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListBrokersResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "brokerSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListBrokers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListBrokers where
  toPath = Lude.const "/v1/brokers"

instance Lude.ToQuery ListBrokers where
  toQuery ListBrokers' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListBrokersResponse' smart constructor.
data ListBrokersResponse = ListBrokersResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    brokerSummaries :: Lude.Maybe [BrokerSummary],
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

-- | Creates a value of 'ListBrokersResponse' with the minimum fields required to make a request.
--
-- * 'brokerSummaries' - A list of information about all brokers.
-- * 'nextToken' - The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
-- * 'responseStatus' - The response status code.
mkListBrokersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListBrokersResponse
mkListBrokersResponse pResponseStatus_ =
  ListBrokersResponse'
    { nextToken = Lude.Nothing,
      brokerSummaries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsNextToken :: Lens.Lens' ListBrokersResponse (Lude.Maybe Lude.Text)
lbrsNextToken = Lens.lens (nextToken :: ListBrokersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBrokersResponse)
{-# DEPRECATED lbrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of information about all brokers.
--
-- /Note:/ Consider using 'brokerSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsBrokerSummaries :: Lens.Lens' ListBrokersResponse (Lude.Maybe [BrokerSummary])
lbrsBrokerSummaries = Lens.lens (brokerSummaries :: ListBrokersResponse -> Lude.Maybe [BrokerSummary]) (\s a -> s {brokerSummaries = a} :: ListBrokersResponse)
{-# DEPRECATED lbrsBrokerSummaries "Use generic-lens or generic-optics with 'brokerSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsResponseStatus :: Lens.Lens' ListBrokersResponse Lude.Int
lbrsResponseStatus = Lens.lens (responseStatus :: ListBrokersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListBrokersResponse)
{-# DEPRECATED lbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
