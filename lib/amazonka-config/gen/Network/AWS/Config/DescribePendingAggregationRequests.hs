{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribePendingAggregationRequests
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all pending aggregation requests.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribePendingAggregationRequests
  ( -- * Creating a request
    DescribePendingAggregationRequests (..),
    mkDescribePendingAggregationRequests,

    -- ** Request lenses
    dparNextToken,
    dparLimit,

    -- * Destructuring the response
    DescribePendingAggregationRequestsResponse (..),
    mkDescribePendingAggregationRequestsResponse,

    -- ** Response lenses
    dparrsNextToken,
    dparrsPendingAggregationRequests,
    dparrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribePendingAggregationRequests' smart constructor.
data DescribePendingAggregationRequests = DescribePendingAggregationRequests'
  { nextToken ::
      Lude.Maybe Lude.Text,
    limit ::
      Lude.Maybe
        Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePendingAggregationRequests' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of evaluation results returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
mkDescribePendingAggregationRequests ::
  DescribePendingAggregationRequests
mkDescribePendingAggregationRequests =
  DescribePendingAggregationRequests'
    { nextToken = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparNextToken :: Lens.Lens' DescribePendingAggregationRequests (Lude.Maybe Lude.Text)
dparNextToken = Lens.lens (nextToken :: DescribePendingAggregationRequests -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribePendingAggregationRequests)
{-# DEPRECATED dparNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of evaluation results returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparLimit :: Lens.Lens' DescribePendingAggregationRequests (Lude.Maybe Lude.Natural)
dparLimit = Lens.lens (limit :: DescribePendingAggregationRequests -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribePendingAggregationRequests)
{-# DEPRECATED dparLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager DescribePendingAggregationRequests where
  page rq rs
    | Page.stop (rs Lens.^. dparrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dparrsPendingAggregationRequests) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dparNextToken Lens..~ rs Lens.^. dparrsNextToken

instance Lude.AWSRequest DescribePendingAggregationRequests where
  type
    Rs DescribePendingAggregationRequests =
      DescribePendingAggregationRequestsResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribePendingAggregationRequestsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "PendingAggregationRequests" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribePendingAggregationRequests where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DescribePendingAggregationRequests" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribePendingAggregationRequests where
  toJSON DescribePendingAggregationRequests' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath DescribePendingAggregationRequests where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribePendingAggregationRequests where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribePendingAggregationRequestsResponse' smart constructor.
data DescribePendingAggregationRequestsResponse = DescribePendingAggregationRequestsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    pendingAggregationRequests ::
      Lude.Maybe
        [PendingAggregationRequest],
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

-- | Creates a value of 'DescribePendingAggregationRequestsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'pendingAggregationRequests' - Returns a PendingAggregationRequests object.
-- * 'responseStatus' - The response status code.
mkDescribePendingAggregationRequestsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribePendingAggregationRequestsResponse
mkDescribePendingAggregationRequestsResponse pResponseStatus_ =
  DescribePendingAggregationRequestsResponse'
    { nextToken =
        Lude.Nothing,
      pendingAggregationRequests = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparrsNextToken :: Lens.Lens' DescribePendingAggregationRequestsResponse (Lude.Maybe Lude.Text)
dparrsNextToken = Lens.lens (nextToken :: DescribePendingAggregationRequestsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribePendingAggregationRequestsResponse)
{-# DEPRECATED dparrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Returns a PendingAggregationRequests object.
--
-- /Note:/ Consider using 'pendingAggregationRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparrsPendingAggregationRequests :: Lens.Lens' DescribePendingAggregationRequestsResponse (Lude.Maybe [PendingAggregationRequest])
dparrsPendingAggregationRequests = Lens.lens (pendingAggregationRequests :: DescribePendingAggregationRequestsResponse -> Lude.Maybe [PendingAggregationRequest]) (\s a -> s {pendingAggregationRequests = a} :: DescribePendingAggregationRequestsResponse)
{-# DEPRECATED dparrsPendingAggregationRequests "Use generic-lens or generic-optics with 'pendingAggregationRequests' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparrsResponseStatus :: Lens.Lens' DescribePendingAggregationRequestsResponse Lude.Int
dparrsResponseStatus = Lens.lens (responseStatus :: DescribePendingAggregationRequestsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePendingAggregationRequestsResponse)
{-# DEPRECATED dparrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
