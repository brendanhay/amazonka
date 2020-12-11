{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.SearchTransitGatewayRoutes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for routes in the specified transit gateway route table.
module Network.AWS.EC2.SearchTransitGatewayRoutes
  ( -- * Creating a request
    SearchTransitGatewayRoutes (..),
    mkSearchTransitGatewayRoutes,

    -- ** Request lenses
    stgrDryRun,
    stgrMaxResults,
    stgrTransitGatewayRouteTableId,
    stgrFilters,

    -- * Destructuring the response
    SearchTransitGatewayRoutesResponse (..),
    mkSearchTransitGatewayRoutesResponse,

    -- ** Response lenses
    stgrrsAdditionalRoutesAvailable,
    stgrrsRoutes,
    stgrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSearchTransitGatewayRoutes' smart constructor.
data SearchTransitGatewayRoutes = SearchTransitGatewayRoutes'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    maxResults :: Lude.Maybe Lude.Natural,
    transitGatewayRouteTableId ::
      Lude.Text,
    filters :: [Filter]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchTransitGatewayRoutes' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters. The possible values are:
--
--
--     * @attachment.transit-gateway-attachment-id@ - The id of the transit gateway attachment.
--
--
--     * @attachment.resource-id@ - The resource id of the transit gateway attachment.
--
--
--     * @attachment.resource-type@ - The attachment resource type. Valid values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @peering@ .
--
--
--     * @prefix-list-id@ - The ID of the prefix list.
--
--
--     * @route-search.exact-match@ - The exact match of the specified filter.
--
--
--     * @route-search.longest-prefix-match@ - The longest prefix that matches the route.
--
--
--     * @route-search.subnet-of-match@ - The routes with a subnet that match the specified CIDR filter.
--
--
--     * @route-search.supernet-of-match@ - The routes with a CIDR that encompass the CIDR filter. For example, if you have 10.0.1.0/29 and 10.0.1.0/31 routes in your route table and you specify supernet-of-match as 10.0.1.0/30, then the result returns 10.0.1.0/29.
--
--
--     * @state@ - The state of the route (@active@ | @blackhole@ ).
--
--
--     * @type@ - The type of route (@propagated@ | @static@ ).
--
--
-- * 'maxResults' - The maximum number of routes to return.
-- * 'transitGatewayRouteTableId' - The ID of the transit gateway route table.
mkSearchTransitGatewayRoutes ::
  -- | 'transitGatewayRouteTableId'
  Lude.Text ->
  SearchTransitGatewayRoutes
mkSearchTransitGatewayRoutes pTransitGatewayRouteTableId_ =
  SearchTransitGatewayRoutes'
    { dryRun = Lude.Nothing,
      maxResults = Lude.Nothing,
      transitGatewayRouteTableId = pTransitGatewayRouteTableId_,
      filters = Lude.mempty
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgrDryRun :: Lens.Lens' SearchTransitGatewayRoutes (Lude.Maybe Lude.Bool)
stgrDryRun = Lens.lens (dryRun :: SearchTransitGatewayRoutes -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: SearchTransitGatewayRoutes)
{-# DEPRECATED stgrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of routes to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgrMaxResults :: Lens.Lens' SearchTransitGatewayRoutes (Lude.Maybe Lude.Natural)
stgrMaxResults = Lens.lens (maxResults :: SearchTransitGatewayRoutes -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: SearchTransitGatewayRoutes)
{-# DEPRECATED stgrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgrTransitGatewayRouteTableId :: Lens.Lens' SearchTransitGatewayRoutes Lude.Text
stgrTransitGatewayRouteTableId = Lens.lens (transitGatewayRouteTableId :: SearchTransitGatewayRoutes -> Lude.Text) (\s a -> s {transitGatewayRouteTableId = a} :: SearchTransitGatewayRoutes)
{-# DEPRECATED stgrTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | One or more filters. The possible values are:
--
--
--     * @attachment.transit-gateway-attachment-id@ - The id of the transit gateway attachment.
--
--
--     * @attachment.resource-id@ - The resource id of the transit gateway attachment.
--
--
--     * @attachment.resource-type@ - The attachment resource type. Valid values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @peering@ .
--
--
--     * @prefix-list-id@ - The ID of the prefix list.
--
--
--     * @route-search.exact-match@ - The exact match of the specified filter.
--
--
--     * @route-search.longest-prefix-match@ - The longest prefix that matches the route.
--
--
--     * @route-search.subnet-of-match@ - The routes with a subnet that match the specified CIDR filter.
--
--
--     * @route-search.supernet-of-match@ - The routes with a CIDR that encompass the CIDR filter. For example, if you have 10.0.1.0/29 and 10.0.1.0/31 routes in your route table and you specify supernet-of-match as 10.0.1.0/30, then the result returns 10.0.1.0/29.
--
--
--     * @state@ - The state of the route (@active@ | @blackhole@ ).
--
--
--     * @type@ - The type of route (@propagated@ | @static@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgrFilters :: Lens.Lens' SearchTransitGatewayRoutes [Filter]
stgrFilters = Lens.lens (filters :: SearchTransitGatewayRoutes -> [Filter]) (\s a -> s {filters = a} :: SearchTransitGatewayRoutes)
{-# DEPRECATED stgrFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

instance Lude.AWSRequest SearchTransitGatewayRoutes where
  type
    Rs SearchTransitGatewayRoutes =
      SearchTransitGatewayRoutesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          SearchTransitGatewayRoutesResponse'
            Lude.<$> (x Lude..@? "additionalRoutesAvailable")
            Lude.<*> ( x Lude..@? "routeSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SearchTransitGatewayRoutes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SearchTransitGatewayRoutes where
  toPath = Lude.const "/"

instance Lude.ToQuery SearchTransitGatewayRoutes where
  toQuery SearchTransitGatewayRoutes' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("SearchTransitGatewayRoutes" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults,
        "TransitGatewayRouteTableId" Lude.=: transitGatewayRouteTableId,
        Lude.toQueryList "Filter" filters
      ]

-- | /See:/ 'mkSearchTransitGatewayRoutesResponse' smart constructor.
data SearchTransitGatewayRoutesResponse = SearchTransitGatewayRoutesResponse'
  { additionalRoutesAvailable ::
      Lude.Maybe Lude.Bool,
    routes ::
      Lude.Maybe
        [TransitGatewayRoute],
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

-- | Creates a value of 'SearchTransitGatewayRoutesResponse' with the minimum fields required to make a request.
--
-- * 'additionalRoutesAvailable' - Indicates whether there are additional routes available.
-- * 'responseStatus' - The response status code.
-- * 'routes' - Information about the routes.
mkSearchTransitGatewayRoutesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SearchTransitGatewayRoutesResponse
mkSearchTransitGatewayRoutesResponse pResponseStatus_ =
  SearchTransitGatewayRoutesResponse'
    { additionalRoutesAvailable =
        Lude.Nothing,
      routes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Indicates whether there are additional routes available.
--
-- /Note:/ Consider using 'additionalRoutesAvailable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgrrsAdditionalRoutesAvailable :: Lens.Lens' SearchTransitGatewayRoutesResponse (Lude.Maybe Lude.Bool)
stgrrsAdditionalRoutesAvailable = Lens.lens (additionalRoutesAvailable :: SearchTransitGatewayRoutesResponse -> Lude.Maybe Lude.Bool) (\s a -> s {additionalRoutesAvailable = a} :: SearchTransitGatewayRoutesResponse)
{-# DEPRECATED stgrrsAdditionalRoutesAvailable "Use generic-lens or generic-optics with 'additionalRoutesAvailable' instead." #-}

-- | Information about the routes.
--
-- /Note:/ Consider using 'routes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgrrsRoutes :: Lens.Lens' SearchTransitGatewayRoutesResponse (Lude.Maybe [TransitGatewayRoute])
stgrrsRoutes = Lens.lens (routes :: SearchTransitGatewayRoutesResponse -> Lude.Maybe [TransitGatewayRoute]) (\s a -> s {routes = a} :: SearchTransitGatewayRoutesResponse)
{-# DEPRECATED stgrrsRoutes "Use generic-lens or generic-optics with 'routes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgrrsResponseStatus :: Lens.Lens' SearchTransitGatewayRoutesResponse Lude.Int
stgrrsResponseStatus = Lens.lens (responseStatus :: SearchTransitGatewayRoutesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SearchTransitGatewayRoutesResponse)
{-# DEPRECATED stgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
