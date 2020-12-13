{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeClientVPNRoutes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the routes for the specified Client VPN endpoint.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeClientVPNRoutes
  ( -- * Creating a request
    DescribeClientVPNRoutes (..),
    mkDescribeClientVPNRoutes,

    -- ** Request lenses
    dcvrFilters,
    dcvrNextToken,
    dcvrClientVPNEndpointId,
    dcvrDryRun,
    dcvrMaxResults,

    -- * Destructuring the response
    DescribeClientVPNRoutesResponse (..),
    mkDescribeClientVPNRoutesResponse,

    -- ** Response lenses
    dcvrrsRoutes,
    dcvrrsNextToken,
    dcvrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeClientVPNRoutes' smart constructor.
data DescribeClientVPNRoutes = DescribeClientVPNRoutes'
  { -- | One or more filters. Filter names and values are case-sensitive.
    --
    --
    --     * @destination-cidr@ - The CIDR of the route destination.
    --
    --
    --     * @origin@ - How the route was associated with the Client VPN endpoint (@associate@ | @add-route@ ).
    --
    --
    --     * @target-subnet@ - The ID of the subnet through which traffic is routed.
    filters :: Lude.Maybe [Filter],
    -- | The token to retrieve the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ID of the Client VPN endpoint.
    clientVPNEndpointId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClientVPNRoutes' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters. Filter names and values are case-sensitive.
--
--
--     * @destination-cidr@ - The CIDR of the route destination.
--
--
--     * @origin@ - How the route was associated with the Client VPN endpoint (@associate@ | @add-route@ ).
--
--
--     * @target-subnet@ - The ID of the subnet through which traffic is routed.
--
--
-- * 'nextToken' - The token to retrieve the next page of results.
-- * 'clientVPNEndpointId' - The ID of the Client VPN endpoint.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
mkDescribeClientVPNRoutes ::
  -- | 'clientVPNEndpointId'
  Lude.Text ->
  DescribeClientVPNRoutes
mkDescribeClientVPNRoutes pClientVPNEndpointId_ =
  DescribeClientVPNRoutes'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      clientVPNEndpointId = pClientVPNEndpointId_,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters. Filter names and values are case-sensitive.
--
--
--     * @destination-cidr@ - The CIDR of the route destination.
--
--
--     * @origin@ - How the route was associated with the Client VPN endpoint (@associate@ | @add-route@ ).
--
--
--     * @target-subnet@ - The ID of the subnet through which traffic is routed.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrFilters :: Lens.Lens' DescribeClientVPNRoutes (Lude.Maybe [Filter])
dcvrFilters = Lens.lens (filters :: DescribeClientVPNRoutes -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeClientVPNRoutes)
{-# DEPRECATED dcvrFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrNextToken :: Lens.Lens' DescribeClientVPNRoutes (Lude.Maybe Lude.Text)
dcvrNextToken = Lens.lens (nextToken :: DescribeClientVPNRoutes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeClientVPNRoutes)
{-# DEPRECATED dcvrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrClientVPNEndpointId :: Lens.Lens' DescribeClientVPNRoutes Lude.Text
dcvrClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: DescribeClientVPNRoutes -> Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: DescribeClientVPNRoutes)
{-# DEPRECATED dcvrClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrDryRun :: Lens.Lens' DescribeClientVPNRoutes (Lude.Maybe Lude.Bool)
dcvrDryRun = Lens.lens (dryRun :: DescribeClientVPNRoutes -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeClientVPNRoutes)
{-# DEPRECATED dcvrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrMaxResults :: Lens.Lens' DescribeClientVPNRoutes (Lude.Maybe Lude.Natural)
dcvrMaxResults = Lens.lens (maxResults :: DescribeClientVPNRoutes -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeClientVPNRoutes)
{-# DEPRECATED dcvrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeClientVPNRoutes where
  page rq rs
    | Page.stop (rs Lens.^. dcvrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dcvrrsRoutes) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcvrNextToken Lens..~ rs Lens.^. dcvrrsNextToken

instance Lude.AWSRequest DescribeClientVPNRoutes where
  type Rs DescribeClientVPNRoutes = DescribeClientVPNRoutesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeClientVPNRoutesResponse'
            Lude.<$> ( x Lude..@? "routes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeClientVPNRoutes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeClientVPNRoutes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeClientVPNRoutes where
  toQuery DescribeClientVPNRoutes' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeClientVpnRoutes" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "ClientVpnEndpointId" Lude.=: clientVPNEndpointId,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeClientVPNRoutesResponse' smart constructor.
data DescribeClientVPNRoutesResponse = DescribeClientVPNRoutesResponse'
  { -- | Information about the Client VPN endpoint routes.
    routes :: Lude.Maybe [ClientVPNRoute],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClientVPNRoutesResponse' with the minimum fields required to make a request.
--
-- * 'routes' - Information about the Client VPN endpoint routes.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeClientVPNRoutesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeClientVPNRoutesResponse
mkDescribeClientVPNRoutesResponse pResponseStatus_ =
  DescribeClientVPNRoutesResponse'
    { routes = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the Client VPN endpoint routes.
--
-- /Note:/ Consider using 'routes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrrsRoutes :: Lens.Lens' DescribeClientVPNRoutesResponse (Lude.Maybe [ClientVPNRoute])
dcvrrsRoutes = Lens.lens (routes :: DescribeClientVPNRoutesResponse -> Lude.Maybe [ClientVPNRoute]) (\s a -> s {routes = a} :: DescribeClientVPNRoutesResponse)
{-# DEPRECATED dcvrrsRoutes "Use generic-lens or generic-optics with 'routes' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrrsNextToken :: Lens.Lens' DescribeClientVPNRoutesResponse (Lude.Maybe Lude.Text)
dcvrrsNextToken = Lens.lens (nextToken :: DescribeClientVPNRoutesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeClientVPNRoutesResponse)
{-# DEPRECATED dcvrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrrsResponseStatus :: Lens.Lens' DescribeClientVPNRoutesResponse Lude.Int
dcvrrsResponseStatus = Lens.lens (responseStatus :: DescribeClientVPNRoutesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeClientVPNRoutesResponse)
{-# DEPRECATED dcvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
