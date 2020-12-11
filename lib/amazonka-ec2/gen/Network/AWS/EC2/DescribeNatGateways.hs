{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeNatGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your NAT gateways.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeNatGateways
  ( -- * Creating a request
    DescribeNatGateways (..),
    mkDescribeNatGateways,

    -- ** Request lenses
    dngNatGatewayIds,
    dngNextToken,
    dngFilter,
    dngDryRun,
    dngMaxResults,

    -- * Destructuring the response
    DescribeNatGatewaysResponse (..),
    mkDescribeNatGatewaysResponse,

    -- ** Response lenses
    dngrsNatGateways,
    dngrsNextToken,
    dngrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeNatGateways' smart constructor.
data DescribeNatGateways = DescribeNatGateways'
  { natGatewayIds ::
      Lude.Maybe [Lude.Text],
    nextToken :: Lude.Maybe Lude.Text,
    filter :: Lude.Maybe [Filter],
    dryRun :: Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'DescribeNatGateways' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filter' - One or more filters.
--
--
--     * @nat-gateway-id@ - The ID of the NAT gateway.
--
--
--     * @state@ - The state of the NAT gateway (@pending@ | @failed@ | @available@ | @deleting@ | @deleted@ ).
--
--
--     * @subnet-id@ - The ID of the subnet in which the NAT gateway resides.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-id@ - The ID of the VPC in which the NAT gateway resides.
--
--
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
-- * 'natGatewayIds' - One or more NAT gateway IDs.
-- * 'nextToken' - The token for the next page of results.
mkDescribeNatGateways ::
  DescribeNatGateways
mkDescribeNatGateways =
  DescribeNatGateways'
    { natGatewayIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more NAT gateway IDs.
--
-- /Note:/ Consider using 'natGatewayIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngNatGatewayIds :: Lens.Lens' DescribeNatGateways (Lude.Maybe [Lude.Text])
dngNatGatewayIds = Lens.lens (natGatewayIds :: DescribeNatGateways -> Lude.Maybe [Lude.Text]) (\s a -> s {natGatewayIds = a} :: DescribeNatGateways)
{-# DEPRECATED dngNatGatewayIds "Use generic-lens or generic-optics with 'natGatewayIds' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngNextToken :: Lens.Lens' DescribeNatGateways (Lude.Maybe Lude.Text)
dngNextToken = Lens.lens (nextToken :: DescribeNatGateways -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeNatGateways)
{-# DEPRECATED dngNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One or more filters.
--
--
--     * @nat-gateway-id@ - The ID of the NAT gateway.
--
--
--     * @state@ - The state of the NAT gateway (@pending@ | @failed@ | @available@ | @deleting@ | @deleted@ ).
--
--
--     * @subnet-id@ - The ID of the subnet in which the NAT gateway resides.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-id@ - The ID of the VPC in which the NAT gateway resides.
--
--
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngFilter :: Lens.Lens' DescribeNatGateways (Lude.Maybe [Filter])
dngFilter = Lens.lens (filter :: DescribeNatGateways -> Lude.Maybe [Filter]) (\s a -> s {filter = a} :: DescribeNatGateways)
{-# DEPRECATED dngFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngDryRun :: Lens.Lens' DescribeNatGateways (Lude.Maybe Lude.Bool)
dngDryRun = Lens.lens (dryRun :: DescribeNatGateways -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeNatGateways)
{-# DEPRECATED dngDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngMaxResults :: Lens.Lens' DescribeNatGateways (Lude.Maybe Lude.Natural)
dngMaxResults = Lens.lens (maxResults :: DescribeNatGateways -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeNatGateways)
{-# DEPRECATED dngMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeNatGateways where
  page rq rs
    | Page.stop (rs Lens.^. dngrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dngrsNatGateways) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dngNextToken Lens..~ rs Lens.^. dngrsNextToken

instance Lude.AWSRequest DescribeNatGateways where
  type Rs DescribeNatGateways = DescribeNatGatewaysResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeNatGatewaysResponse'
            Lude.<$> ( x Lude..@? "natGatewaySet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeNatGateways where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeNatGateways where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeNatGateways where
  toQuery DescribeNatGateways' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeNatGateways" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "NatGatewayId" Lude.<$> natGatewayIds),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filter),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeNatGatewaysResponse' smart constructor.
data DescribeNatGatewaysResponse = DescribeNatGatewaysResponse'
  { natGateways ::
      Lude.Maybe [NatGateway],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeNatGatewaysResponse' with the minimum fields required to make a request.
--
-- * 'natGateways' - Information about the NAT gateways.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeNatGatewaysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeNatGatewaysResponse
mkDescribeNatGatewaysResponse pResponseStatus_ =
  DescribeNatGatewaysResponse'
    { natGateways = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the NAT gateways.
--
-- /Note:/ Consider using 'natGateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngrsNatGateways :: Lens.Lens' DescribeNatGatewaysResponse (Lude.Maybe [NatGateway])
dngrsNatGateways = Lens.lens (natGateways :: DescribeNatGatewaysResponse -> Lude.Maybe [NatGateway]) (\s a -> s {natGateways = a} :: DescribeNatGatewaysResponse)
{-# DEPRECATED dngrsNatGateways "Use generic-lens or generic-optics with 'natGateways' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngrsNextToken :: Lens.Lens' DescribeNatGatewaysResponse (Lude.Maybe Lude.Text)
dngrsNextToken = Lens.lens (nextToken :: DescribeNatGatewaysResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeNatGatewaysResponse)
{-# DEPRECATED dngrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngrsResponseStatus :: Lens.Lens' DescribeNatGatewaysResponse Lude.Int
dngrsResponseStatus = Lens.lens (responseStatus :: DescribeNatGatewaysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeNatGatewaysResponse)
{-# DEPRECATED dngrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
