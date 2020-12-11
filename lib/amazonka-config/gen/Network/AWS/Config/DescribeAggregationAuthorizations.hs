{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeAggregationAuthorizations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of authorizations granted to various aggregator accounts and regions.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeAggregationAuthorizations
  ( -- * Creating a request
    DescribeAggregationAuthorizations (..),
    mkDescribeAggregationAuthorizations,

    -- ** Request lenses
    daaNextToken,
    daaLimit,

    -- * Destructuring the response
    DescribeAggregationAuthorizationsResponse (..),
    mkDescribeAggregationAuthorizationsResponse,

    -- ** Response lenses
    daarsAggregationAuthorizations,
    daarsNextToken,
    daarsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAggregationAuthorizations' smart constructor.
data DescribeAggregationAuthorizations = DescribeAggregationAuthorizations'
  { nextToken ::
      Lude.Maybe Lude.Text,
    limit ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAggregationAuthorizations' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of AggregationAuthorizations returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
mkDescribeAggregationAuthorizations ::
  DescribeAggregationAuthorizations
mkDescribeAggregationAuthorizations =
  DescribeAggregationAuthorizations'
    { nextToken = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaNextToken :: Lens.Lens' DescribeAggregationAuthorizations (Lude.Maybe Lude.Text)
daaNextToken = Lens.lens (nextToken :: DescribeAggregationAuthorizations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAggregationAuthorizations)
{-# DEPRECATED daaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of AggregationAuthorizations returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaLimit :: Lens.Lens' DescribeAggregationAuthorizations (Lude.Maybe Lude.Natural)
daaLimit = Lens.lens (limit :: DescribeAggregationAuthorizations -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeAggregationAuthorizations)
{-# DEPRECATED daaLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager DescribeAggregationAuthorizations where
  page rq rs
    | Page.stop (rs Lens.^. daarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. daarsAggregationAuthorizations) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& daaNextToken Lens..~ rs Lens.^. daarsNextToken

instance Lude.AWSRequest DescribeAggregationAuthorizations where
  type
    Rs DescribeAggregationAuthorizations =
      DescribeAggregationAuthorizationsResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAggregationAuthorizationsResponse'
            Lude.<$> (x Lude..?> "AggregationAuthorizations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAggregationAuthorizations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DescribeAggregationAuthorizations" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAggregationAuthorizations where
  toJSON DescribeAggregationAuthorizations' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath DescribeAggregationAuthorizations where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAggregationAuthorizations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAggregationAuthorizationsResponse' smart constructor.
data DescribeAggregationAuthorizationsResponse = DescribeAggregationAuthorizationsResponse'
  { aggregationAuthorizations ::
      Lude.Maybe
        [AggregationAuthorization],
    nextToken ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'DescribeAggregationAuthorizationsResponse' with the minimum fields required to make a request.
--
-- * 'aggregationAuthorizations' - Returns a list of authorizations granted to various aggregator accounts and regions.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'responseStatus' - The response status code.
mkDescribeAggregationAuthorizationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAggregationAuthorizationsResponse
mkDescribeAggregationAuthorizationsResponse pResponseStatus_ =
  DescribeAggregationAuthorizationsResponse'
    { aggregationAuthorizations =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns a list of authorizations granted to various aggregator accounts and regions.
--
-- /Note:/ Consider using 'aggregationAuthorizations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsAggregationAuthorizations :: Lens.Lens' DescribeAggregationAuthorizationsResponse (Lude.Maybe [AggregationAuthorization])
daarsAggregationAuthorizations = Lens.lens (aggregationAuthorizations :: DescribeAggregationAuthorizationsResponse -> Lude.Maybe [AggregationAuthorization]) (\s a -> s {aggregationAuthorizations = a} :: DescribeAggregationAuthorizationsResponse)
{-# DEPRECATED daarsAggregationAuthorizations "Use generic-lens or generic-optics with 'aggregationAuthorizations' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsNextToken :: Lens.Lens' DescribeAggregationAuthorizationsResponse (Lude.Maybe Lude.Text)
daarsNextToken = Lens.lens (nextToken :: DescribeAggregationAuthorizationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAggregationAuthorizationsResponse)
{-# DEPRECATED daarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsResponseStatus :: Lens.Lens' DescribeAggregationAuthorizationsResponse Lude.Int
daarsResponseStatus = Lens.lens (responseStatus :: DescribeAggregationAuthorizationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAggregationAuthorizationsResponse)
{-# DEPRECATED daarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
