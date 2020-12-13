{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribePublicIPv4Pools
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified IPv4 address pools.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribePublicIPv4Pools
  ( -- * Creating a request
    DescribePublicIPv4Pools (..),
    mkDescribePublicIPv4Pools,

    -- ** Request lenses
    dpipPoolIds,
    dpipFilters,
    dpipNextToken,
    dpipMaxResults,

    -- * Destructuring the response
    DescribePublicIPv4PoolsResponse (..),
    mkDescribePublicIPv4PoolsResponse,

    -- ** Response lenses
    dpiprsPublicIPv4Pools,
    dpiprsNextToken,
    dpiprsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribePublicIPv4Pools' smart constructor.
data DescribePublicIPv4Pools = DescribePublicIPv4Pools'
  { -- | The IDs of the address pools.
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
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePublicIPv4Pools' with the minimum fields required to make a request.
--
-- * 'poolIds' - The IDs of the address pools.
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
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
mkDescribePublicIPv4Pools ::
  DescribePublicIPv4Pools
mkDescribePublicIPv4Pools =
  DescribePublicIPv4Pools'
    { poolIds = Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The IDs of the address pools.
--
-- /Note:/ Consider using 'poolIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpipPoolIds :: Lens.Lens' DescribePublicIPv4Pools (Lude.Maybe [Lude.Text])
dpipPoolIds = Lens.lens (poolIds :: DescribePublicIPv4Pools -> Lude.Maybe [Lude.Text]) (\s a -> s {poolIds = a} :: DescribePublicIPv4Pools)
{-# DEPRECATED dpipPoolIds "Use generic-lens or generic-optics with 'poolIds' instead." #-}

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
dpipFilters :: Lens.Lens' DescribePublicIPv4Pools (Lude.Maybe [Filter])
dpipFilters = Lens.lens (filters :: DescribePublicIPv4Pools -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribePublicIPv4Pools)
{-# DEPRECATED dpipFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpipNextToken :: Lens.Lens' DescribePublicIPv4Pools (Lude.Maybe Lude.Text)
dpipNextToken = Lens.lens (nextToken :: DescribePublicIPv4Pools -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribePublicIPv4Pools)
{-# DEPRECATED dpipNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpipMaxResults :: Lens.Lens' DescribePublicIPv4Pools (Lude.Maybe Lude.Natural)
dpipMaxResults = Lens.lens (maxResults :: DescribePublicIPv4Pools -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribePublicIPv4Pools)
{-# DEPRECATED dpipMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribePublicIPv4Pools where
  page rq rs
    | Page.stop (rs Lens.^. dpiprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dpiprsPublicIPv4Pools) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dpipNextToken Lens..~ rs Lens.^. dpiprsNextToken

instance Lude.AWSRequest DescribePublicIPv4Pools where
  type Rs DescribePublicIPv4Pools = DescribePublicIPv4PoolsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribePublicIPv4PoolsResponse'
            Lude.<$> ( x Lude..@? "publicIpv4PoolSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribePublicIPv4Pools where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribePublicIPv4Pools where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribePublicIPv4Pools where
  toQuery DescribePublicIPv4Pools' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribePublicIpv4Pools" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "PoolId" Lude.<$> poolIds),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribePublicIPv4PoolsResponse' smart constructor.
data DescribePublicIPv4PoolsResponse = DescribePublicIPv4PoolsResponse'
  { -- | Information about the address pools.
    publicIPv4Pools :: Lude.Maybe [PublicIPv4Pool],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePublicIPv4PoolsResponse' with the minimum fields required to make a request.
--
-- * 'publicIPv4Pools' - Information about the address pools.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribePublicIPv4PoolsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribePublicIPv4PoolsResponse
mkDescribePublicIPv4PoolsResponse pResponseStatus_ =
  DescribePublicIPv4PoolsResponse'
    { publicIPv4Pools = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the address pools.
--
-- /Note:/ Consider using 'publicIPv4Pools' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpiprsPublicIPv4Pools :: Lens.Lens' DescribePublicIPv4PoolsResponse (Lude.Maybe [PublicIPv4Pool])
dpiprsPublicIPv4Pools = Lens.lens (publicIPv4Pools :: DescribePublicIPv4PoolsResponse -> Lude.Maybe [PublicIPv4Pool]) (\s a -> s {publicIPv4Pools = a} :: DescribePublicIPv4PoolsResponse)
{-# DEPRECATED dpiprsPublicIPv4Pools "Use generic-lens or generic-optics with 'publicIPv4Pools' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpiprsNextToken :: Lens.Lens' DescribePublicIPv4PoolsResponse (Lude.Maybe Lude.Text)
dpiprsNextToken = Lens.lens (nextToken :: DescribePublicIPv4PoolsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribePublicIPv4PoolsResponse)
{-# DEPRECATED dpiprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpiprsResponseStatus :: Lens.Lens' DescribePublicIPv4PoolsResponse Lude.Int
dpiprsResponseStatus = Lens.lens (responseStatus :: DescribePublicIPv4PoolsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePublicIPv4PoolsResponse)
{-# DEPRECATED dpiprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
