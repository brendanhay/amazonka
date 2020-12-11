{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeInternetGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your internet gateways.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeInternetGateways
  ( -- * Creating a request
    DescribeInternetGateways (..),
    mkDescribeInternetGateways,

    -- ** Request lenses
    dFilters,
    dNextToken,
    dInternetGatewayIds,
    dDryRun,
    dMaxResults,

    -- * Destructuring the response
    DescribeInternetGatewaysResponse (..),
    mkDescribeInternetGatewaysResponse,

    -- ** Response lenses
    digrsNextToken,
    digrsInternetGateways,
    digrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeInternetGateways' smart constructor.
data DescribeInternetGateways = DescribeInternetGateways'
  { filters ::
      Lude.Maybe [Filter],
    nextToken :: Lude.Maybe Lude.Text,
    internetGatewayIds ::
      Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'DescribeInternetGateways' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters.
--
--
--     * @attachment.state@ - The current state of the attachment between the gateway and the VPC (@available@ ). Present only if a VPC is attached.
--
--
--     * @attachment.vpc-id@ - The ID of an attached VPC.
--
--
--     * @internet-gateway-id@ - The ID of the Internet gateway.
--
--
--     * @owner-id@ - The ID of the AWS account that owns the internet gateway.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
-- * 'internetGatewayIds' - One or more internet gateway IDs.
--
-- Default: Describes all your internet gateways.
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
-- * 'nextToken' - The token for the next page of results.
mkDescribeInternetGateways ::
  DescribeInternetGateways
mkDescribeInternetGateways =
  DescribeInternetGateways'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      internetGatewayIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @attachment.state@ - The current state of the attachment between the gateway and the VPC (@available@ ). Present only if a VPC is attached.
--
--
--     * @attachment.vpc-id@ - The ID of an attached VPC.
--
--
--     * @internet-gateway-id@ - The ID of the Internet gateway.
--
--
--     * @owner-id@ - The ID of the AWS account that owns the internet gateway.
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
dFilters :: Lens.Lens' DescribeInternetGateways (Lude.Maybe [Filter])
dFilters = Lens.lens (filters :: DescribeInternetGateways -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeInternetGateways)
{-# DEPRECATED dFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeInternetGateways (Lude.Maybe Lude.Text)
dNextToken = Lens.lens (nextToken :: DescribeInternetGateways -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInternetGateways)
{-# DEPRECATED dNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One or more internet gateway IDs.
--
-- Default: Describes all your internet gateways.
--
-- /Note:/ Consider using 'internetGatewayIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInternetGatewayIds :: Lens.Lens' DescribeInternetGateways (Lude.Maybe [Lude.Text])
dInternetGatewayIds = Lens.lens (internetGatewayIds :: DescribeInternetGateways -> Lude.Maybe [Lude.Text]) (\s a -> s {internetGatewayIds = a} :: DescribeInternetGateways)
{-# DEPRECATED dInternetGatewayIds "Use generic-lens or generic-optics with 'internetGatewayIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDryRun :: Lens.Lens' DescribeInternetGateways (Lude.Maybe Lude.Bool)
dDryRun = Lens.lens (dryRun :: DescribeInternetGateways -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeInternetGateways)
{-# DEPRECATED dDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxResults :: Lens.Lens' DescribeInternetGateways (Lude.Maybe Lude.Natural)
dMaxResults = Lens.lens (maxResults :: DescribeInternetGateways -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeInternetGateways)
{-# DEPRECATED dMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeInternetGateways where
  page rq rs
    | Page.stop (rs Lens.^. digrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. digrsInternetGateways) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dNextToken Lens..~ rs Lens.^. digrsNextToken

instance Lude.AWSRequest DescribeInternetGateways where
  type Rs DescribeInternetGateways = DescribeInternetGatewaysResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeInternetGatewaysResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "internetGatewaySet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInternetGateways where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeInternetGateways where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeInternetGateways where
  toQuery DescribeInternetGateways' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeInternetGateways" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery
          (Lude.toQueryList "InternetGatewayId" Lude.<$> internetGatewayIds),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeInternetGatewaysResponse' smart constructor.
data DescribeInternetGatewaysResponse = DescribeInternetGatewaysResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    internetGateways ::
      Lude.Maybe
        [InternetGateway],
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

-- | Creates a value of 'DescribeInternetGatewaysResponse' with the minimum fields required to make a request.
--
-- * 'internetGateways' - Information about one or more internet gateways.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeInternetGatewaysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInternetGatewaysResponse
mkDescribeInternetGatewaysResponse pResponseStatus_ =
  DescribeInternetGatewaysResponse'
    { nextToken = Lude.Nothing,
      internetGateways = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digrsNextToken :: Lens.Lens' DescribeInternetGatewaysResponse (Lude.Maybe Lude.Text)
digrsNextToken = Lens.lens (nextToken :: DescribeInternetGatewaysResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInternetGatewaysResponse)
{-# DEPRECATED digrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about one or more internet gateways.
--
-- /Note:/ Consider using 'internetGateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digrsInternetGateways :: Lens.Lens' DescribeInternetGatewaysResponse (Lude.Maybe [InternetGateway])
digrsInternetGateways = Lens.lens (internetGateways :: DescribeInternetGatewaysResponse -> Lude.Maybe [InternetGateway]) (\s a -> s {internetGateways = a} :: DescribeInternetGatewaysResponse)
{-# DEPRECATED digrsInternetGateways "Use generic-lens or generic-optics with 'internetGateways' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digrsResponseStatus :: Lens.Lens' DescribeInternetGatewaysResponse Lude.Int
digrsResponseStatus = Lens.lens (responseStatus :: DescribeInternetGatewaysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInternetGatewaysResponse)
{-# DEPRECATED digrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
