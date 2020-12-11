{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetTransitGatewayRouteTablePropagations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the route table propagations for the specified transit gateway route table.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetTransitGatewayRouteTablePropagations
  ( -- * Creating a request
    GetTransitGatewayRouteTablePropagations (..),
    mkGetTransitGatewayRouteTablePropagations,

    -- ** Request lenses
    gtgrtpFilters,
    gtgrtpNextToken,
    gtgrtpDryRun,
    gtgrtpMaxResults,
    gtgrtpTransitGatewayRouteTableId,

    -- * Destructuring the response
    GetTransitGatewayRouteTablePropagationsResponse (..),
    mkGetTransitGatewayRouteTablePropagationsResponse,

    -- ** Response lenses
    gtgrtprsTransitGatewayRouteTablePropagations,
    gtgrtprsNextToken,
    gtgrtprsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTransitGatewayRouteTablePropagations' smart constructor.
data GetTransitGatewayRouteTablePropagations = GetTransitGatewayRouteTablePropagations'
  { filters ::
      Lude.Maybe
        [Filter],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    dryRun ::
      Lude.Maybe
        Lude.Bool,
    maxResults ::
      Lude.Maybe
        Lude.Natural,
    transitGatewayRouteTableId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTransitGatewayRouteTablePropagations' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters. The possible values are:
--
--
--     * @resource-id@ - The ID of the resource.
--
--
--     * @resource-type@ - The resource type. Valid values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @peering@ .
--
--
--     * @transit-gateway-attachment-id@ - The ID of the attachment.
--
--
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
-- * 'nextToken' - The token for the next page of results.
-- * 'transitGatewayRouteTableId' - The ID of the transit gateway route table.
mkGetTransitGatewayRouteTablePropagations ::
  -- | 'transitGatewayRouteTableId'
  Lude.Text ->
  GetTransitGatewayRouteTablePropagations
mkGetTransitGatewayRouteTablePropagations
  pTransitGatewayRouteTableId_ =
    GetTransitGatewayRouteTablePropagations'
      { filters = Lude.Nothing,
        nextToken = Lude.Nothing,
        dryRun = Lude.Nothing,
        maxResults = Lude.Nothing,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_
      }

-- | One or more filters. The possible values are:
--
--
--     * @resource-id@ - The ID of the resource.
--
--
--     * @resource-type@ - The resource type. Valid values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @peering@ .
--
--
--     * @transit-gateway-attachment-id@ - The ID of the attachment.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtpFilters :: Lens.Lens' GetTransitGatewayRouteTablePropagations (Lude.Maybe [Filter])
gtgrtpFilters = Lens.lens (filters :: GetTransitGatewayRouteTablePropagations -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: GetTransitGatewayRouteTablePropagations)
{-# DEPRECATED gtgrtpFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtpNextToken :: Lens.Lens' GetTransitGatewayRouteTablePropagations (Lude.Maybe Lude.Text)
gtgrtpNextToken = Lens.lens (nextToken :: GetTransitGatewayRouteTablePropagations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTransitGatewayRouteTablePropagations)
{-# DEPRECATED gtgrtpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtpDryRun :: Lens.Lens' GetTransitGatewayRouteTablePropagations (Lude.Maybe Lude.Bool)
gtgrtpDryRun = Lens.lens (dryRun :: GetTransitGatewayRouteTablePropagations -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: GetTransitGatewayRouteTablePropagations)
{-# DEPRECATED gtgrtpDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtpMaxResults :: Lens.Lens' GetTransitGatewayRouteTablePropagations (Lude.Maybe Lude.Natural)
gtgrtpMaxResults = Lens.lens (maxResults :: GetTransitGatewayRouteTablePropagations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetTransitGatewayRouteTablePropagations)
{-# DEPRECATED gtgrtpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtpTransitGatewayRouteTableId :: Lens.Lens' GetTransitGatewayRouteTablePropagations Lude.Text
gtgrtpTransitGatewayRouteTableId = Lens.lens (transitGatewayRouteTableId :: GetTransitGatewayRouteTablePropagations -> Lude.Text) (\s a -> s {transitGatewayRouteTableId = a} :: GetTransitGatewayRouteTablePropagations)
{-# DEPRECATED gtgrtpTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

instance Page.AWSPager GetTransitGatewayRouteTablePropagations where
  page rq rs
    | Page.stop (rs Lens.^. gtgrtprsNextToken) = Lude.Nothing
    | Page.stop
        (rs Lens.^. gtgrtprsTransitGatewayRouteTablePropagations) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gtgrtpNextToken Lens..~ rs Lens.^. gtgrtprsNextToken

instance Lude.AWSRequest GetTransitGatewayRouteTablePropagations where
  type
    Rs GetTransitGatewayRouteTablePropagations =
      GetTransitGatewayRouteTablePropagationsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetTransitGatewayRouteTablePropagationsResponse'
            Lude.<$> ( x Lude..@? "transitGatewayRouteTablePropagations"
                         Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTransitGatewayRouteTablePropagations where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetTransitGatewayRouteTablePropagations where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTransitGatewayRouteTablePropagations where
  toQuery GetTransitGatewayRouteTablePropagations' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GetTransitGatewayRouteTablePropagations" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults,
        "TransitGatewayRouteTableId" Lude.=: transitGatewayRouteTableId
      ]

-- | /See:/ 'mkGetTransitGatewayRouteTablePropagationsResponse' smart constructor.
data GetTransitGatewayRouteTablePropagationsResponse = GetTransitGatewayRouteTablePropagationsResponse'
  { transitGatewayRouteTablePropagations ::
      Lude.Maybe
        [TransitGatewayRouteTablePropagation],
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'GetTransitGatewayRouteTablePropagationsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
-- * 'transitGatewayRouteTablePropagations' - Information about the route table propagations.
mkGetTransitGatewayRouteTablePropagationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTransitGatewayRouteTablePropagationsResponse
mkGetTransitGatewayRouteTablePropagationsResponse pResponseStatus_ =
  GetTransitGatewayRouteTablePropagationsResponse'
    { transitGatewayRouteTablePropagations =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the route table propagations.
--
-- /Note:/ Consider using 'transitGatewayRouteTablePropagations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtprsTransitGatewayRouteTablePropagations :: Lens.Lens' GetTransitGatewayRouteTablePropagationsResponse (Lude.Maybe [TransitGatewayRouteTablePropagation])
gtgrtprsTransitGatewayRouteTablePropagations = Lens.lens (transitGatewayRouteTablePropagations :: GetTransitGatewayRouteTablePropagationsResponse -> Lude.Maybe [TransitGatewayRouteTablePropagation]) (\s a -> s {transitGatewayRouteTablePropagations = a} :: GetTransitGatewayRouteTablePropagationsResponse)
{-# DEPRECATED gtgrtprsTransitGatewayRouteTablePropagations "Use generic-lens or generic-optics with 'transitGatewayRouteTablePropagations' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtprsNextToken :: Lens.Lens' GetTransitGatewayRouteTablePropagationsResponse (Lude.Maybe Lude.Text)
gtgrtprsNextToken = Lens.lens (nextToken :: GetTransitGatewayRouteTablePropagationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTransitGatewayRouteTablePropagationsResponse)
{-# DEPRECATED gtgrtprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtprsResponseStatus :: Lens.Lens' GetTransitGatewayRouteTablePropagationsResponse Lude.Int
gtgrtprsResponseStatus = Lens.lens (responseStatus :: GetTransitGatewayRouteTablePropagationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTransitGatewayRouteTablePropagationsResponse)
{-# DEPRECATED gtgrtprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
