{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeIPv6Pools
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your IPv6 address pools.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeIPv6Pools
  ( -- * Creating a request
    DescribeIPv6Pools (..),
    mkDescribeIPv6Pools,

    -- ** Request lenses
    dipPoolIds,
    dipFilters,
    dipNextToken,
    dipDryRun,
    dipMaxResults,

    -- * Destructuring the response
    DescribeIPv6PoolsResponse (..),
    mkDescribeIPv6PoolsResponse,

    -- ** Response lenses
    diprsIPv6Pools,
    diprsNextToken,
    diprsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeIPv6Pools' smart constructor.
data DescribeIPv6Pools = DescribeIPv6Pools'
  { -- | The IDs of the IPv6 address pools.
    poolIds :: Lude.Maybe [Lude.Text],
    -- | One or more filters.
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
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

-- | Creates a value of 'DescribeIPv6Pools' with the minimum fields required to make a request.
--
-- * 'poolIds' - The IDs of the IPv6 address pools.
-- * 'filters' - One or more filters.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
-- * 'nextToken' - The token for the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
mkDescribeIPv6Pools ::
  DescribeIPv6Pools
mkDescribeIPv6Pools =
  DescribeIPv6Pools'
    { poolIds = Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The IDs of the IPv6 address pools.
--
-- /Note:/ Consider using 'poolIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipPoolIds :: Lens.Lens' DescribeIPv6Pools (Lude.Maybe [Lude.Text])
dipPoolIds = Lens.lens (poolIds :: DescribeIPv6Pools -> Lude.Maybe [Lude.Text]) (\s a -> s {poolIds = a} :: DescribeIPv6Pools)
{-# DEPRECATED dipPoolIds "Use generic-lens or generic-optics with 'poolIds' instead." #-}

-- | One or more filters.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipFilters :: Lens.Lens' DescribeIPv6Pools (Lude.Maybe [Filter])
dipFilters = Lens.lens (filters :: DescribeIPv6Pools -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeIPv6Pools)
{-# DEPRECATED dipFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipNextToken :: Lens.Lens' DescribeIPv6Pools (Lude.Maybe Lude.Text)
dipNextToken = Lens.lens (nextToken :: DescribeIPv6Pools -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeIPv6Pools)
{-# DEPRECATED dipNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipDryRun :: Lens.Lens' DescribeIPv6Pools (Lude.Maybe Lude.Bool)
dipDryRun = Lens.lens (dryRun :: DescribeIPv6Pools -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeIPv6Pools)
{-# DEPRECATED dipDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipMaxResults :: Lens.Lens' DescribeIPv6Pools (Lude.Maybe Lude.Natural)
dipMaxResults = Lens.lens (maxResults :: DescribeIPv6Pools -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeIPv6Pools)
{-# DEPRECATED dipMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeIPv6Pools where
  page rq rs
    | Page.stop (rs Lens.^. diprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. diprsIPv6Pools) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dipNextToken Lens..~ rs Lens.^. diprsNextToken

instance Lude.AWSRequest DescribeIPv6Pools where
  type Rs DescribeIPv6Pools = DescribeIPv6PoolsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeIPv6PoolsResponse'
            Lude.<$> ( x Lude..@? "ipv6PoolSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeIPv6Pools where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeIPv6Pools where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeIPv6Pools where
  toQuery DescribeIPv6Pools' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeIpv6Pools" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "PoolId" Lude.<$> poolIds),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeIPv6PoolsResponse' smart constructor.
data DescribeIPv6PoolsResponse = DescribeIPv6PoolsResponse'
  { -- | Information about the IPv6 address pools.
    ipv6Pools :: Lude.Maybe [IPv6Pool],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeIPv6PoolsResponse' with the minimum fields required to make a request.
--
-- * 'ipv6Pools' - Information about the IPv6 address pools.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeIPv6PoolsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeIPv6PoolsResponse
mkDescribeIPv6PoolsResponse pResponseStatus_ =
  DescribeIPv6PoolsResponse'
    { ipv6Pools = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the IPv6 address pools.
--
-- /Note:/ Consider using 'ipv6Pools' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprsIPv6Pools :: Lens.Lens' DescribeIPv6PoolsResponse (Lude.Maybe [IPv6Pool])
diprsIPv6Pools = Lens.lens (ipv6Pools :: DescribeIPv6PoolsResponse -> Lude.Maybe [IPv6Pool]) (\s a -> s {ipv6Pools = a} :: DescribeIPv6PoolsResponse)
{-# DEPRECATED diprsIPv6Pools "Use generic-lens or generic-optics with 'ipv6Pools' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprsNextToken :: Lens.Lens' DescribeIPv6PoolsResponse (Lude.Maybe Lude.Text)
diprsNextToken = Lens.lens (nextToken :: DescribeIPv6PoolsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeIPv6PoolsResponse)
{-# DEPRECATED diprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprsResponseStatus :: Lens.Lens' DescribeIPv6PoolsResponse Lude.Int
diprsResponseStatus = Lens.lens (responseStatus :: DescribeIPv6PoolsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeIPv6PoolsResponse)
{-# DEPRECATED diprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
