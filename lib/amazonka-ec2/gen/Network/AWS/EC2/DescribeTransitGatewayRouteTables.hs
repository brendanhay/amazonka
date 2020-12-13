{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeTransitGatewayRouteTables
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more transit gateway route tables. By default, all transit gateway route tables are described. Alternatively, you can filter the results.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTransitGatewayRouteTables
  ( -- * Creating a request
    DescribeTransitGatewayRouteTables (..),
    mkDescribeTransitGatewayRouteTables,

    -- ** Request lenses
    dtgrtsFilters,
    dtgrtsNextToken,
    dtgrtsDryRun,
    dtgrtsTransitGatewayRouteTableIds,
    dtgrtsMaxResults,

    -- * Destructuring the response
    DescribeTransitGatewayRouteTablesResponse (..),
    mkDescribeTransitGatewayRouteTablesResponse,

    -- ** Response lenses
    dtgrtrsTransitGatewayRouteTables,
    dtgrtrsNextToken,
    dtgrtrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTransitGatewayRouteTables' smart constructor.
data DescribeTransitGatewayRouteTables = DescribeTransitGatewayRouteTables'
  { -- | One or more filters. The possible values are:
    --
    --
    --     * @default-association-route-table@ - Indicates whether this is the default association route table for the transit gateway (@true@ | @false@ ).
    --
    --
    --     * @default-propagation-route-table@ - Indicates whether this is the default propagation route table for the transit gateway (@true@ | @false@ ).
    --
    --
    --     * @state@ - The state of the route table (@available@ | @deleting@ | @deleted@ | @pending@ ).
    --
    --
    --     * @transit-gateway-id@ - The ID of the transit gateway.
    --
    --
    --     * @transit-gateway-route-table-id@ - The ID of the transit gateway route table.
    filters :: Lude.Maybe [Filter],
    -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The IDs of the transit gateway route tables.
    transitGatewayRouteTableIds :: Lude.Maybe [Lude.Text],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTransitGatewayRouteTables' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters. The possible values are:
--
--
--     * @default-association-route-table@ - Indicates whether this is the default association route table for the transit gateway (@true@ | @false@ ).
--
--
--     * @default-propagation-route-table@ - Indicates whether this is the default propagation route table for the transit gateway (@true@ | @false@ ).
--
--
--     * @state@ - The state of the route table (@available@ | @deleting@ | @deleted@ | @pending@ ).
--
--
--     * @transit-gateway-id@ - The ID of the transit gateway.
--
--
--     * @transit-gateway-route-table-id@ - The ID of the transit gateway route table.
--
--
-- * 'nextToken' - The token for the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'transitGatewayRouteTableIds' - The IDs of the transit gateway route tables.
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
mkDescribeTransitGatewayRouteTables ::
  DescribeTransitGatewayRouteTables
mkDescribeTransitGatewayRouteTables =
  DescribeTransitGatewayRouteTables'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      transitGatewayRouteTableIds = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters. The possible values are:
--
--
--     * @default-association-route-table@ - Indicates whether this is the default association route table for the transit gateway (@true@ | @false@ ).
--
--
--     * @default-propagation-route-table@ - Indicates whether this is the default propagation route table for the transit gateway (@true@ | @false@ ).
--
--
--     * @state@ - The state of the route table (@available@ | @deleting@ | @deleted@ | @pending@ ).
--
--
--     * @transit-gateway-id@ - The ID of the transit gateway.
--
--
--     * @transit-gateway-route-table-id@ - The ID of the transit gateway route table.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtsFilters :: Lens.Lens' DescribeTransitGatewayRouteTables (Lude.Maybe [Filter])
dtgrtsFilters = Lens.lens (filters :: DescribeTransitGatewayRouteTables -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeTransitGatewayRouteTables)
{-# DEPRECATED dtgrtsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtsNextToken :: Lens.Lens' DescribeTransitGatewayRouteTables (Lude.Maybe Lude.Text)
dtgrtsNextToken = Lens.lens (nextToken :: DescribeTransitGatewayRouteTables -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTransitGatewayRouteTables)
{-# DEPRECATED dtgrtsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtsDryRun :: Lens.Lens' DescribeTransitGatewayRouteTables (Lude.Maybe Lude.Bool)
dtgrtsDryRun = Lens.lens (dryRun :: DescribeTransitGatewayRouteTables -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeTransitGatewayRouteTables)
{-# DEPRECATED dtgrtsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The IDs of the transit gateway route tables.
--
-- /Note:/ Consider using 'transitGatewayRouteTableIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtsTransitGatewayRouteTableIds :: Lens.Lens' DescribeTransitGatewayRouteTables (Lude.Maybe [Lude.Text])
dtgrtsTransitGatewayRouteTableIds = Lens.lens (transitGatewayRouteTableIds :: DescribeTransitGatewayRouteTables -> Lude.Maybe [Lude.Text]) (\s a -> s {transitGatewayRouteTableIds = a} :: DescribeTransitGatewayRouteTables)
{-# DEPRECATED dtgrtsTransitGatewayRouteTableIds "Use generic-lens or generic-optics with 'transitGatewayRouteTableIds' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtsMaxResults :: Lens.Lens' DescribeTransitGatewayRouteTables (Lude.Maybe Lude.Natural)
dtgrtsMaxResults = Lens.lens (maxResults :: DescribeTransitGatewayRouteTables -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeTransitGatewayRouteTables)
{-# DEPRECATED dtgrtsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeTransitGatewayRouteTables where
  page rq rs
    | Page.stop (rs Lens.^. dtgrtrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dtgrtrsTransitGatewayRouteTables) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dtgrtsNextToken Lens..~ rs Lens.^. dtgrtrsNextToken

instance Lude.AWSRequest DescribeTransitGatewayRouteTables where
  type
    Rs DescribeTransitGatewayRouteTables =
      DescribeTransitGatewayRouteTablesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeTransitGatewayRouteTablesResponse'
            Lude.<$> ( x Lude..@? "transitGatewayRouteTables" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTransitGatewayRouteTables where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeTransitGatewayRouteTables where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTransitGatewayRouteTables where
  toQuery DescribeTransitGatewayRouteTables' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeTransitGatewayRouteTables" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        Lude.toQuery
          ( Lude.toQueryList "TransitGatewayRouteTableIds"
              Lude.<$> transitGatewayRouteTableIds
          ),
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeTransitGatewayRouteTablesResponse' smart constructor.
data DescribeTransitGatewayRouteTablesResponse = DescribeTransitGatewayRouteTablesResponse'
  { -- | Information about the transit gateway route tables.
    transitGatewayRouteTables :: Lude.Maybe [TransitGatewayRouteTable],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTransitGatewayRouteTablesResponse' with the minimum fields required to make a request.
--
-- * 'transitGatewayRouteTables' - Information about the transit gateway route tables.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeTransitGatewayRouteTablesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTransitGatewayRouteTablesResponse
mkDescribeTransitGatewayRouteTablesResponse pResponseStatus_ =
  DescribeTransitGatewayRouteTablesResponse'
    { transitGatewayRouteTables =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the transit gateway route tables.
--
-- /Note:/ Consider using 'transitGatewayRouteTables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtrsTransitGatewayRouteTables :: Lens.Lens' DescribeTransitGatewayRouteTablesResponse (Lude.Maybe [TransitGatewayRouteTable])
dtgrtrsTransitGatewayRouteTables = Lens.lens (transitGatewayRouteTables :: DescribeTransitGatewayRouteTablesResponse -> Lude.Maybe [TransitGatewayRouteTable]) (\s a -> s {transitGatewayRouteTables = a} :: DescribeTransitGatewayRouteTablesResponse)
{-# DEPRECATED dtgrtrsTransitGatewayRouteTables "Use generic-lens or generic-optics with 'transitGatewayRouteTables' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtrsNextToken :: Lens.Lens' DescribeTransitGatewayRouteTablesResponse (Lude.Maybe Lude.Text)
dtgrtrsNextToken = Lens.lens (nextToken :: DescribeTransitGatewayRouteTablesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTransitGatewayRouteTablesResponse)
{-# DEPRECATED dtgrtrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtrsResponseStatus :: Lens.Lens' DescribeTransitGatewayRouteTablesResponse Lude.Int
dtgrtrsResponseStatus = Lens.lens (responseStatus :: DescribeTransitGatewayRouteTablesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTransitGatewayRouteTablesResponse)
{-# DEPRECATED dtgrtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
