{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeCarrierGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your carrier gateways.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeCarrierGateways
  ( -- * Creating a request
    DescribeCarrierGateways (..),
    mkDescribeCarrierGateways,

    -- ** Request lenses
    dcgsFilters,
    dcgsNextToken,
    dcgsCarrierGatewayIds,
    dcgsDryRun,
    dcgsMaxResults,

    -- * Destructuring the response
    DescribeCarrierGatewaysResponse (..),
    mkDescribeCarrierGatewaysResponse,

    -- ** Response lenses
    dcgsrsNextToken,
    dcgsrsCarrierGateways,
    dcgsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeCarrierGateways' smart constructor.
data DescribeCarrierGateways = DescribeCarrierGateways'
  { -- | One or more filters.
    --
    --
    --     * @carrier-gateway-id@ - The ID of the carrier gateway.
    --
    --
    --     * @state@ - The state of the carrier gateway (@pending@ | @failed@ | @available@ | @deleting@ | @deleted@ ).
    --
    --
    --     * @owner-id@ - The AWS account ID of the owner of the carrier gateway.
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    --
    --
    --     * @vpc-id@ - The ID of the VPC associated with the carrier gateway.
    filters :: Lude.Maybe [Filter],
    -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | One or more carrier gateway IDs.
    carrierGatewayIds :: Lude.Maybe [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCarrierGateways' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters.
--
--
--     * @carrier-gateway-id@ - The ID of the carrier gateway.
--
--
--     * @state@ - The state of the carrier gateway (@pending@ | @failed@ | @available@ | @deleting@ | @deleted@ ).
--
--
--     * @owner-id@ - The AWS account ID of the owner of the carrier gateway.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-id@ - The ID of the VPC associated with the carrier gateway.
--
--
-- * 'nextToken' - The token for the next page of results.
-- * 'carrierGatewayIds' - One or more carrier gateway IDs.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
mkDescribeCarrierGateways ::
  DescribeCarrierGateways
mkDescribeCarrierGateways =
  DescribeCarrierGateways'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      carrierGatewayIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @carrier-gateway-id@ - The ID of the carrier gateway.
--
--
--     * @state@ - The state of the carrier gateway (@pending@ | @failed@ | @available@ | @deleting@ | @deleted@ ).
--
--
--     * @owner-id@ - The AWS account ID of the owner of the carrier gateway.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-id@ - The ID of the VPC associated with the carrier gateway.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgsFilters :: Lens.Lens' DescribeCarrierGateways (Lude.Maybe [Filter])
dcgsFilters = Lens.lens (filters :: DescribeCarrierGateways -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeCarrierGateways)
{-# DEPRECATED dcgsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgsNextToken :: Lens.Lens' DescribeCarrierGateways (Lude.Maybe Lude.Text)
dcgsNextToken = Lens.lens (nextToken :: DescribeCarrierGateways -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeCarrierGateways)
{-# DEPRECATED dcgsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One or more carrier gateway IDs.
--
-- /Note:/ Consider using 'carrierGatewayIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgsCarrierGatewayIds :: Lens.Lens' DescribeCarrierGateways (Lude.Maybe [Lude.Text])
dcgsCarrierGatewayIds = Lens.lens (carrierGatewayIds :: DescribeCarrierGateways -> Lude.Maybe [Lude.Text]) (\s a -> s {carrierGatewayIds = a} :: DescribeCarrierGateways)
{-# DEPRECATED dcgsCarrierGatewayIds "Use generic-lens or generic-optics with 'carrierGatewayIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgsDryRun :: Lens.Lens' DescribeCarrierGateways (Lude.Maybe Lude.Bool)
dcgsDryRun = Lens.lens (dryRun :: DescribeCarrierGateways -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeCarrierGateways)
{-# DEPRECATED dcgsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgsMaxResults :: Lens.Lens' DescribeCarrierGateways (Lude.Maybe Lude.Natural)
dcgsMaxResults = Lens.lens (maxResults :: DescribeCarrierGateways -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeCarrierGateways)
{-# DEPRECATED dcgsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeCarrierGateways where
  page rq rs
    | Page.stop (rs Lens.^. dcgsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dcgsrsCarrierGateways) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcgsNextToken Lens..~ rs Lens.^. dcgsrsNextToken

instance Lude.AWSRequest DescribeCarrierGateways where
  type Rs DescribeCarrierGateways = DescribeCarrierGatewaysResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeCarrierGatewaysResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "carrierGatewaySet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCarrierGateways where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeCarrierGateways where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCarrierGateways where
  toQuery DescribeCarrierGateways' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeCarrierGateways" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery
          (Lude.toQueryList "CarrierGatewayId" Lude.<$> carrierGatewayIds),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeCarrierGatewaysResponse' smart constructor.
data DescribeCarrierGatewaysResponse = DescribeCarrierGatewaysResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the carrier gateway.
    carrierGateways :: Lude.Maybe [CarrierGateway],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCarrierGatewaysResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'carrierGateways' - Information about the carrier gateway.
-- * 'responseStatus' - The response status code.
mkDescribeCarrierGatewaysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCarrierGatewaysResponse
mkDescribeCarrierGatewaysResponse pResponseStatus_ =
  DescribeCarrierGatewaysResponse'
    { nextToken = Lude.Nothing,
      carrierGateways = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgsrsNextToken :: Lens.Lens' DescribeCarrierGatewaysResponse (Lude.Maybe Lude.Text)
dcgsrsNextToken = Lens.lens (nextToken :: DescribeCarrierGatewaysResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeCarrierGatewaysResponse)
{-# DEPRECATED dcgsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the carrier gateway.
--
-- /Note:/ Consider using 'carrierGateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgsrsCarrierGateways :: Lens.Lens' DescribeCarrierGatewaysResponse (Lude.Maybe [CarrierGateway])
dcgsrsCarrierGateways = Lens.lens (carrierGateways :: DescribeCarrierGatewaysResponse -> Lude.Maybe [CarrierGateway]) (\s a -> s {carrierGateways = a} :: DescribeCarrierGatewaysResponse)
{-# DEPRECATED dcgsrsCarrierGateways "Use generic-lens or generic-optics with 'carrierGateways' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgsrsResponseStatus :: Lens.Lens' DescribeCarrierGatewaysResponse Lude.Int
dcgsrsResponseStatus = Lens.lens (responseStatus :: DescribeCarrierGatewaysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCarrierGatewaysResponse)
{-# DEPRECATED dcgsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
