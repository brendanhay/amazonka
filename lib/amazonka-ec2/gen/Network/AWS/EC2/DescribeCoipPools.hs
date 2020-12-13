{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeCoipPools
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified customer-owned address pools or all of your customer-owned address pools.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeCoipPools
  ( -- * Creating a request
    DescribeCoipPools (..),
    mkDescribeCoipPools,

    -- ** Request lenses
    dcpPoolIds,
    dcpFilters,
    dcpNextToken,
    dcpDryRun,
    dcpMaxResults,

    -- * Destructuring the response
    DescribeCoipPoolsResponse (..),
    mkDescribeCoipPoolsResponse,

    -- ** Response lenses
    dcprsCoipPools,
    dcprsNextToken,
    dcprsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeCoipPools' smart constructor.
data DescribeCoipPools = DescribeCoipPools'
  { -- | The IDs of the address pools.
    poolIds :: Lude.Maybe [Lude.Text],
    -- | The filters. The following are the possible values:
    --
    --
    --     * @coip-pool.pool-id@
    --
    --
    --
    --     * @coip-pool.local-gateway-route-table-id@
    filters :: Lude.Maybe [Filter],
    -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCoipPools' with the minimum fields required to make a request.
--
-- * 'poolIds' - The IDs of the address pools.
-- * 'filters' - The filters. The following are the possible values:
--
--
--     * @coip-pool.pool-id@
--
--
--
--     * @coip-pool.local-gateway-route-table-id@
--
--
-- * 'nextToken' - The token for the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
mkDescribeCoipPools ::
  DescribeCoipPools
mkDescribeCoipPools =
  DescribeCoipPools'
    { poolIds = Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The IDs of the address pools.
--
-- /Note:/ Consider using 'poolIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpPoolIds :: Lens.Lens' DescribeCoipPools (Lude.Maybe [Lude.Text])
dcpPoolIds = Lens.lens (poolIds :: DescribeCoipPools -> Lude.Maybe [Lude.Text]) (\s a -> s {poolIds = a} :: DescribeCoipPools)
{-# DEPRECATED dcpPoolIds "Use generic-lens or generic-optics with 'poolIds' instead." #-}

-- | The filters. The following are the possible values:
--
--
--     * @coip-pool.pool-id@
--
--
--
--     * @coip-pool.local-gateway-route-table-id@
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpFilters :: Lens.Lens' DescribeCoipPools (Lude.Maybe [Filter])
dcpFilters = Lens.lens (filters :: DescribeCoipPools -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeCoipPools)
{-# DEPRECATED dcpFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpNextToken :: Lens.Lens' DescribeCoipPools (Lude.Maybe Lude.Text)
dcpNextToken = Lens.lens (nextToken :: DescribeCoipPools -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeCoipPools)
{-# DEPRECATED dcpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpDryRun :: Lens.Lens' DescribeCoipPools (Lude.Maybe Lude.Bool)
dcpDryRun = Lens.lens (dryRun :: DescribeCoipPools -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeCoipPools)
{-# DEPRECATED dcpDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpMaxResults :: Lens.Lens' DescribeCoipPools (Lude.Maybe Lude.Natural)
dcpMaxResults = Lens.lens (maxResults :: DescribeCoipPools -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeCoipPools)
{-# DEPRECATED dcpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeCoipPools where
  page rq rs
    | Page.stop (rs Lens.^. dcprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dcprsCoipPools) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcpNextToken Lens..~ rs Lens.^. dcprsNextToken

instance Lude.AWSRequest DescribeCoipPools where
  type Rs DescribeCoipPools = DescribeCoipPoolsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeCoipPoolsResponse'
            Lude.<$> ( x Lude..@? "coipPoolSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCoipPools where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeCoipPools where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCoipPools where
  toQuery DescribeCoipPools' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeCoipPools" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "PoolId" Lude.<$> poolIds),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeCoipPoolsResponse' smart constructor.
data DescribeCoipPoolsResponse = DescribeCoipPoolsResponse'
  { -- | Information about the address pools.
    coipPools :: Lude.Maybe [CoipPool],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCoipPoolsResponse' with the minimum fields required to make a request.
--
-- * 'coipPools' - Information about the address pools.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeCoipPoolsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCoipPoolsResponse
mkDescribeCoipPoolsResponse pResponseStatus_ =
  DescribeCoipPoolsResponse'
    { coipPools = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the address pools.
--
-- /Note:/ Consider using 'coipPools' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprsCoipPools :: Lens.Lens' DescribeCoipPoolsResponse (Lude.Maybe [CoipPool])
dcprsCoipPools = Lens.lens (coipPools :: DescribeCoipPoolsResponse -> Lude.Maybe [CoipPool]) (\s a -> s {coipPools = a} :: DescribeCoipPoolsResponse)
{-# DEPRECATED dcprsCoipPools "Use generic-lens or generic-optics with 'coipPools' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprsNextToken :: Lens.Lens' DescribeCoipPoolsResponse (Lude.Maybe Lude.Text)
dcprsNextToken = Lens.lens (nextToken :: DescribeCoipPoolsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeCoipPoolsResponse)
{-# DEPRECATED dcprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprsResponseStatus :: Lens.Lens' DescribeCoipPoolsResponse Lude.Int
dcprsResponseStatus = Lens.lens (responseStatus :: DescribeCoipPoolsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCoipPoolsResponse)
{-# DEPRECATED dcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
