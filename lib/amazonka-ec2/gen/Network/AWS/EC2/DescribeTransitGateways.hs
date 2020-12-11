{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeTransitGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more transit gateways. By default, all transit gateways are described. Alternatively, you can filter the results.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTransitGateways
  ( -- * Creating a request
    DescribeTransitGateways (..),
    mkDescribeTransitGateways,

    -- ** Request lenses
    dtgsFilters,
    dtgsTransitGatewayIds,
    dtgsNextToken,
    dtgsDryRun,
    dtgsMaxResults,

    -- * Destructuring the response
    DescribeTransitGatewaysResponse (..),
    mkDescribeTransitGatewaysResponse,

    -- ** Response lenses
    dtgrsTransitGateways,
    dtgrsNextToken,
    dtgrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTransitGateways' smart constructor.
data DescribeTransitGateways = DescribeTransitGateways'
  { filters ::
      Lude.Maybe [Filter],
    transitGatewayIds :: Lude.Maybe [Lude.Text],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeTransitGateways' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters. The possible values are:
--
--
--     * @options.propagation-default-route-table-id@ - The ID of the default propagation route table.
--
--
--     * @options.amazon-side-asn@ - The private ASN for the Amazon side of a BGP session.
--
--
--     * @options.association-default-route-table-id@ - The ID of the default association route table.
--
--
--     * @options.auto-accept-shared-attachments@ - Indicates whether there is automatic acceptance of attachment requests (@enable@ | @disable@ ).
--
--
--     * @options.default-route-table-association@ - Indicates whether resource attachments are automatically associated with the default association route table (@enable@ | @disable@ ).
--
--
--     * @options.default-route-table-propagation@ - Indicates whether resource attachments automatically propagate routes to the default propagation route table (@enable@ | @disable@ ).
--
--
--     * @options.dns-support@ - Indicates whether DNS support is enabled (@enable@ | @disable@ ).
--
--
--     * @options.vpn-ecmp-support@ - Indicates whether Equal Cost Multipath Protocol support is enabled (@enable@ | @disable@ ).
--
--
--     * @owner-id@ - The ID of the AWS account that owns the transit gateway.
--
--
--     * @state@ - The state of the transit gateway (@available@ | @deleted@ | @deleting@ | @modifying@ | @pending@ ).
--
--
--     * @transit-gateway-id@ - The ID of the transit gateway.
--
--
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
-- * 'nextToken' - The token for the next page of results.
-- * 'transitGatewayIds' - The IDs of the transit gateways.
mkDescribeTransitGateways ::
  DescribeTransitGateways
mkDescribeTransitGateways =
  DescribeTransitGateways'
    { filters = Lude.Nothing,
      transitGatewayIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters. The possible values are:
--
--
--     * @options.propagation-default-route-table-id@ - The ID of the default propagation route table.
--
--
--     * @options.amazon-side-asn@ - The private ASN for the Amazon side of a BGP session.
--
--
--     * @options.association-default-route-table-id@ - The ID of the default association route table.
--
--
--     * @options.auto-accept-shared-attachments@ - Indicates whether there is automatic acceptance of attachment requests (@enable@ | @disable@ ).
--
--
--     * @options.default-route-table-association@ - Indicates whether resource attachments are automatically associated with the default association route table (@enable@ | @disable@ ).
--
--
--     * @options.default-route-table-propagation@ - Indicates whether resource attachments automatically propagate routes to the default propagation route table (@enable@ | @disable@ ).
--
--
--     * @options.dns-support@ - Indicates whether DNS support is enabled (@enable@ | @disable@ ).
--
--
--     * @options.vpn-ecmp-support@ - Indicates whether Equal Cost Multipath Protocol support is enabled (@enable@ | @disable@ ).
--
--
--     * @owner-id@ - The ID of the AWS account that owns the transit gateway.
--
--
--     * @state@ - The state of the transit gateway (@available@ | @deleted@ | @deleting@ | @modifying@ | @pending@ ).
--
--
--     * @transit-gateway-id@ - The ID of the transit gateway.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgsFilters :: Lens.Lens' DescribeTransitGateways (Lude.Maybe [Filter])
dtgsFilters = Lens.lens (filters :: DescribeTransitGateways -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeTransitGateways)
{-# DEPRECATED dtgsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The IDs of the transit gateways.
--
-- /Note:/ Consider using 'transitGatewayIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgsTransitGatewayIds :: Lens.Lens' DescribeTransitGateways (Lude.Maybe [Lude.Text])
dtgsTransitGatewayIds = Lens.lens (transitGatewayIds :: DescribeTransitGateways -> Lude.Maybe [Lude.Text]) (\s a -> s {transitGatewayIds = a} :: DescribeTransitGateways)
{-# DEPRECATED dtgsTransitGatewayIds "Use generic-lens or generic-optics with 'transitGatewayIds' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgsNextToken :: Lens.Lens' DescribeTransitGateways (Lude.Maybe Lude.Text)
dtgsNextToken = Lens.lens (nextToken :: DescribeTransitGateways -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTransitGateways)
{-# DEPRECATED dtgsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgsDryRun :: Lens.Lens' DescribeTransitGateways (Lude.Maybe Lude.Bool)
dtgsDryRun = Lens.lens (dryRun :: DescribeTransitGateways -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeTransitGateways)
{-# DEPRECATED dtgsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgsMaxResults :: Lens.Lens' DescribeTransitGateways (Lude.Maybe Lude.Natural)
dtgsMaxResults = Lens.lens (maxResults :: DescribeTransitGateways -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeTransitGateways)
{-# DEPRECATED dtgsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeTransitGateways where
  page rq rs
    | Page.stop (rs Lens.^. dtgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dtgrsTransitGateways) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dtgsNextToken Lens..~ rs Lens.^. dtgrsNextToken

instance Lude.AWSRequest DescribeTransitGateways where
  type Rs DescribeTransitGateways = DescribeTransitGatewaysResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeTransitGatewaysResponse'
            Lude.<$> ( x Lude..@? "transitGatewaySet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTransitGateways where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeTransitGateways where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTransitGateways where
  toQuery DescribeTransitGateways' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeTransitGateways" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        Lude.toQuery
          (Lude.toQueryList "TransitGatewayIds" Lude.<$> transitGatewayIds),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeTransitGatewaysResponse' smart constructor.
data DescribeTransitGatewaysResponse = DescribeTransitGatewaysResponse'
  { transitGateways ::
      Lude.Maybe [TransitGateway],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeTransitGatewaysResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
-- * 'transitGateways' - Information about the transit gateways.
mkDescribeTransitGatewaysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTransitGatewaysResponse
mkDescribeTransitGatewaysResponse pResponseStatus_ =
  DescribeTransitGatewaysResponse'
    { transitGateways = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the transit gateways.
--
-- /Note:/ Consider using 'transitGateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrsTransitGateways :: Lens.Lens' DescribeTransitGatewaysResponse (Lude.Maybe [TransitGateway])
dtgrsTransitGateways = Lens.lens (transitGateways :: DescribeTransitGatewaysResponse -> Lude.Maybe [TransitGateway]) (\s a -> s {transitGateways = a} :: DescribeTransitGatewaysResponse)
{-# DEPRECATED dtgrsTransitGateways "Use generic-lens or generic-optics with 'transitGateways' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrsNextToken :: Lens.Lens' DescribeTransitGatewaysResponse (Lude.Maybe Lude.Text)
dtgrsNextToken = Lens.lens (nextToken :: DescribeTransitGatewaysResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTransitGatewaysResponse)
{-# DEPRECATED dtgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrsResponseStatus :: Lens.Lens' DescribeTransitGatewaysResponse Lude.Int
dtgrsResponseStatus = Lens.lens (responseStatus :: DescribeTransitGatewaysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTransitGatewaysResponse)
{-# DEPRECATED dtgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
