{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeEgressOnlyInternetGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your egress-only internet gateways.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeEgressOnlyInternetGateways
  ( -- * Creating a request
    DescribeEgressOnlyInternetGateways (..),
    mkDescribeEgressOnlyInternetGateways,

    -- ** Request lenses
    deoigEgressOnlyInternetGatewayIds,
    deoigFilters,
    deoigNextToken,
    deoigDryRun,
    deoigMaxResults,

    -- * Destructuring the response
    DescribeEgressOnlyInternetGatewaysResponse (..),
    mkDescribeEgressOnlyInternetGatewaysResponse,

    -- ** Response lenses
    deoigrsEgressOnlyInternetGateways,
    deoigrsNextToken,
    deoigrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEgressOnlyInternetGateways' smart constructor.
data DescribeEgressOnlyInternetGateways = DescribeEgressOnlyInternetGateways'
  { egressOnlyInternetGatewayIds ::
      Lude.Maybe
        [Lude.Text],
    filters ::
      Lude.Maybe [Filter],
    nextToken ::
      Lude.Maybe Lude.Text,
    dryRun ::
      Lude.Maybe Lude.Bool,
    maxResults ::
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

-- | Creates a value of 'DescribeEgressOnlyInternetGateways' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'egressOnlyInternetGatewayIds' - One or more egress-only internet gateway IDs.
-- * 'filters' - One or more filters.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
-- * 'nextToken' - The token for the next page of results.
mkDescribeEgressOnlyInternetGateways ::
  DescribeEgressOnlyInternetGateways
mkDescribeEgressOnlyInternetGateways =
  DescribeEgressOnlyInternetGateways'
    { egressOnlyInternetGatewayIds =
        Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more egress-only internet gateway IDs.
--
-- /Note:/ Consider using 'egressOnlyInternetGatewayIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigEgressOnlyInternetGatewayIds :: Lens.Lens' DescribeEgressOnlyInternetGateways (Lude.Maybe [Lude.Text])
deoigEgressOnlyInternetGatewayIds = Lens.lens (egressOnlyInternetGatewayIds :: DescribeEgressOnlyInternetGateways -> Lude.Maybe [Lude.Text]) (\s a -> s {egressOnlyInternetGatewayIds = a} :: DescribeEgressOnlyInternetGateways)
{-# DEPRECATED deoigEgressOnlyInternetGatewayIds "Use generic-lens or generic-optics with 'egressOnlyInternetGatewayIds' instead." #-}

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
deoigFilters :: Lens.Lens' DescribeEgressOnlyInternetGateways (Lude.Maybe [Filter])
deoigFilters = Lens.lens (filters :: DescribeEgressOnlyInternetGateways -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeEgressOnlyInternetGateways)
{-# DEPRECATED deoigFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigNextToken :: Lens.Lens' DescribeEgressOnlyInternetGateways (Lude.Maybe Lude.Text)
deoigNextToken = Lens.lens (nextToken :: DescribeEgressOnlyInternetGateways -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEgressOnlyInternetGateways)
{-# DEPRECATED deoigNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigDryRun :: Lens.Lens' DescribeEgressOnlyInternetGateways (Lude.Maybe Lude.Bool)
deoigDryRun = Lens.lens (dryRun :: DescribeEgressOnlyInternetGateways -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeEgressOnlyInternetGateways)
{-# DEPRECATED deoigDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigMaxResults :: Lens.Lens' DescribeEgressOnlyInternetGateways (Lude.Maybe Lude.Natural)
deoigMaxResults = Lens.lens (maxResults :: DescribeEgressOnlyInternetGateways -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeEgressOnlyInternetGateways)
{-# DEPRECATED deoigMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeEgressOnlyInternetGateways where
  page rq rs
    | Page.stop (rs Lens.^. deoigrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. deoigrsEgressOnlyInternetGateways) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& deoigNextToken Lens..~ rs Lens.^. deoigrsNextToken

instance Lude.AWSRequest DescribeEgressOnlyInternetGateways where
  type
    Rs DescribeEgressOnlyInternetGateways =
      DescribeEgressOnlyInternetGatewaysResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeEgressOnlyInternetGatewaysResponse'
            Lude.<$> ( x Lude..@? "egressOnlyInternetGatewaySet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEgressOnlyInternetGateways where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeEgressOnlyInternetGateways where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEgressOnlyInternetGateways where
  toQuery DescribeEgressOnlyInternetGateways' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeEgressOnlyInternetGateways" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          ( Lude.toQueryList "EgressOnlyInternetGatewayId"
              Lude.<$> egressOnlyInternetGatewayIds
          ),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeEgressOnlyInternetGatewaysResponse' smart constructor.
data DescribeEgressOnlyInternetGatewaysResponse = DescribeEgressOnlyInternetGatewaysResponse'
  { egressOnlyInternetGateways ::
      Lude.Maybe
        [EgressOnlyInternetGateway],
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

-- | Creates a value of 'DescribeEgressOnlyInternetGatewaysResponse' with the minimum fields required to make a request.
--
-- * 'egressOnlyInternetGateways' - Information about the egress-only internet gateways.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeEgressOnlyInternetGatewaysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEgressOnlyInternetGatewaysResponse
mkDescribeEgressOnlyInternetGatewaysResponse pResponseStatus_ =
  DescribeEgressOnlyInternetGatewaysResponse'
    { egressOnlyInternetGateways =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the egress-only internet gateways.
--
-- /Note:/ Consider using 'egressOnlyInternetGateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigrsEgressOnlyInternetGateways :: Lens.Lens' DescribeEgressOnlyInternetGatewaysResponse (Lude.Maybe [EgressOnlyInternetGateway])
deoigrsEgressOnlyInternetGateways = Lens.lens (egressOnlyInternetGateways :: DescribeEgressOnlyInternetGatewaysResponse -> Lude.Maybe [EgressOnlyInternetGateway]) (\s a -> s {egressOnlyInternetGateways = a} :: DescribeEgressOnlyInternetGatewaysResponse)
{-# DEPRECATED deoigrsEgressOnlyInternetGateways "Use generic-lens or generic-optics with 'egressOnlyInternetGateways' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigrsNextToken :: Lens.Lens' DescribeEgressOnlyInternetGatewaysResponse (Lude.Maybe Lude.Text)
deoigrsNextToken = Lens.lens (nextToken :: DescribeEgressOnlyInternetGatewaysResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEgressOnlyInternetGatewaysResponse)
{-# DEPRECATED deoigrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigrsResponseStatus :: Lens.Lens' DescribeEgressOnlyInternetGatewaysResponse Lude.Int
deoigrsResponseStatus = Lens.lens (responseStatus :: DescribeEgressOnlyInternetGatewaysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEgressOnlyInternetGatewaysResponse)
{-# DEPRECATED deoigrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
