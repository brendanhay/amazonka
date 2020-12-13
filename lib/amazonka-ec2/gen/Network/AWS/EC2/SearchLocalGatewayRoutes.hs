{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.SearchLocalGatewayRoutes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for routes in the specified local gateway route table.
--
-- This operation returns paginated results.
module Network.AWS.EC2.SearchLocalGatewayRoutes
  ( -- * Creating a request
    SearchLocalGatewayRoutes (..),
    mkSearchLocalGatewayRoutes,

    -- ** Request lenses
    slgrFilters,
    slgrNextToken,
    slgrLocalGatewayRouteTableId,
    slgrDryRun,
    slgrMaxResults,

    -- * Destructuring the response
    SearchLocalGatewayRoutesResponse (..),
    mkSearchLocalGatewayRoutesResponse,

    -- ** Response lenses
    slgrrsRoutes,
    slgrrsNextToken,
    slgrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSearchLocalGatewayRoutes' smart constructor.
data SearchLocalGatewayRoutes = SearchLocalGatewayRoutes'
  { -- | One or more filters.
    filters :: [Filter],
    -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchLocalGatewayRoutes' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters.
-- * 'nextToken' - The token for the next page of results.
-- * 'localGatewayRouteTableId' - The ID of the local gateway route table.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
mkSearchLocalGatewayRoutes ::
  -- | 'localGatewayRouteTableId'
  Lude.Text ->
  SearchLocalGatewayRoutes
mkSearchLocalGatewayRoutes pLocalGatewayRouteTableId_ =
  SearchLocalGatewayRoutes'
    { filters = Lude.mempty,
      nextToken = Lude.Nothing,
      localGatewayRouteTableId = pLocalGatewayRouteTableId_,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrFilters :: Lens.Lens' SearchLocalGatewayRoutes [Filter]
slgrFilters = Lens.lens (filters :: SearchLocalGatewayRoutes -> [Filter]) (\s a -> s {filters = a} :: SearchLocalGatewayRoutes)
{-# DEPRECATED slgrFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrNextToken :: Lens.Lens' SearchLocalGatewayRoutes (Lude.Maybe Lude.Text)
slgrNextToken = Lens.lens (nextToken :: SearchLocalGatewayRoutes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchLocalGatewayRoutes)
{-# DEPRECATED slgrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrLocalGatewayRouteTableId :: Lens.Lens' SearchLocalGatewayRoutes Lude.Text
slgrLocalGatewayRouteTableId = Lens.lens (localGatewayRouteTableId :: SearchLocalGatewayRoutes -> Lude.Text) (\s a -> s {localGatewayRouteTableId = a} :: SearchLocalGatewayRoutes)
{-# DEPRECATED slgrLocalGatewayRouteTableId "Use generic-lens or generic-optics with 'localGatewayRouteTableId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrDryRun :: Lens.Lens' SearchLocalGatewayRoutes (Lude.Maybe Lude.Bool)
slgrDryRun = Lens.lens (dryRun :: SearchLocalGatewayRoutes -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: SearchLocalGatewayRoutes)
{-# DEPRECATED slgrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrMaxResults :: Lens.Lens' SearchLocalGatewayRoutes (Lude.Maybe Lude.Int)
slgrMaxResults = Lens.lens (maxResults :: SearchLocalGatewayRoutes -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: SearchLocalGatewayRoutes)
{-# DEPRECATED slgrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager SearchLocalGatewayRoutes where
  page rq rs
    | Page.stop (rs Lens.^. slgrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. slgrrsRoutes) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& slgrNextToken Lens..~ rs Lens.^. slgrrsNextToken

instance Lude.AWSRequest SearchLocalGatewayRoutes where
  type Rs SearchLocalGatewayRoutes = SearchLocalGatewayRoutesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          SearchLocalGatewayRoutesResponse'
            Lude.<$> ( x Lude..@? "routeSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SearchLocalGatewayRoutes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SearchLocalGatewayRoutes where
  toPath = Lude.const "/"

instance Lude.ToQuery SearchLocalGatewayRoutes where
  toQuery SearchLocalGatewayRoutes' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SearchLocalGatewayRoutes" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQueryList "Filter" filters,
        "NextToken" Lude.=: nextToken,
        "LocalGatewayRouteTableId" Lude.=: localGatewayRouteTableId,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkSearchLocalGatewayRoutesResponse' smart constructor.
data SearchLocalGatewayRoutesResponse = SearchLocalGatewayRoutesResponse'
  { -- | Information about the routes.
    routes :: Lude.Maybe [LocalGatewayRoute],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchLocalGatewayRoutesResponse' with the minimum fields required to make a request.
--
-- * 'routes' - Information about the routes.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkSearchLocalGatewayRoutesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SearchLocalGatewayRoutesResponse
mkSearchLocalGatewayRoutesResponse pResponseStatus_ =
  SearchLocalGatewayRoutesResponse'
    { routes = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the routes.
--
-- /Note:/ Consider using 'routes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrrsRoutes :: Lens.Lens' SearchLocalGatewayRoutesResponse (Lude.Maybe [LocalGatewayRoute])
slgrrsRoutes = Lens.lens (routes :: SearchLocalGatewayRoutesResponse -> Lude.Maybe [LocalGatewayRoute]) (\s a -> s {routes = a} :: SearchLocalGatewayRoutesResponse)
{-# DEPRECATED slgrrsRoutes "Use generic-lens or generic-optics with 'routes' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrrsNextToken :: Lens.Lens' SearchLocalGatewayRoutesResponse (Lude.Maybe Lude.Text)
slgrrsNextToken = Lens.lens (nextToken :: SearchLocalGatewayRoutesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchLocalGatewayRoutesResponse)
{-# DEPRECATED slgrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slgrrsResponseStatus :: Lens.Lens' SearchLocalGatewayRoutesResponse Lude.Int
slgrrsResponseStatus = Lens.lens (responseStatus :: SearchLocalGatewayRoutesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SearchLocalGatewayRoutesResponse)
{-# DEPRECATED slgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
