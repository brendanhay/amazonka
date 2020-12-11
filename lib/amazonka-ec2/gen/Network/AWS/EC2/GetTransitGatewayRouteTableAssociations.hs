{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetTransitGatewayRouteTableAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the associations for the specified transit gateway route table.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetTransitGatewayRouteTableAssociations
  ( -- * Creating a request
    GetTransitGatewayRouteTableAssociations (..),
    mkGetTransitGatewayRouteTableAssociations,

    -- ** Request lenses
    gtgrtaFilters,
    gtgrtaNextToken,
    gtgrtaDryRun,
    gtgrtaMaxResults,
    gtgrtaTransitGatewayRouteTableId,

    -- * Destructuring the response
    GetTransitGatewayRouteTableAssociationsResponse (..),
    mkGetTransitGatewayRouteTableAssociationsResponse,

    -- ** Response lenses
    gtgrtarsNextToken,
    gtgrtarsAssociations,
    gtgrtarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTransitGatewayRouteTableAssociations' smart constructor.
data GetTransitGatewayRouteTableAssociations = GetTransitGatewayRouteTableAssociations'
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

-- | Creates a value of 'GetTransitGatewayRouteTableAssociations' with the minimum fields required to make a request.
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
mkGetTransitGatewayRouteTableAssociations ::
  -- | 'transitGatewayRouteTableId'
  Lude.Text ->
  GetTransitGatewayRouteTableAssociations
mkGetTransitGatewayRouteTableAssociations
  pTransitGatewayRouteTableId_ =
    GetTransitGatewayRouteTableAssociations'
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
gtgrtaFilters :: Lens.Lens' GetTransitGatewayRouteTableAssociations (Lude.Maybe [Filter])
gtgrtaFilters = Lens.lens (filters :: GetTransitGatewayRouteTableAssociations -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: GetTransitGatewayRouteTableAssociations)
{-# DEPRECATED gtgrtaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtaNextToken :: Lens.Lens' GetTransitGatewayRouteTableAssociations (Lude.Maybe Lude.Text)
gtgrtaNextToken = Lens.lens (nextToken :: GetTransitGatewayRouteTableAssociations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTransitGatewayRouteTableAssociations)
{-# DEPRECATED gtgrtaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtaDryRun :: Lens.Lens' GetTransitGatewayRouteTableAssociations (Lude.Maybe Lude.Bool)
gtgrtaDryRun = Lens.lens (dryRun :: GetTransitGatewayRouteTableAssociations -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: GetTransitGatewayRouteTableAssociations)
{-# DEPRECATED gtgrtaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtaMaxResults :: Lens.Lens' GetTransitGatewayRouteTableAssociations (Lude.Maybe Lude.Natural)
gtgrtaMaxResults = Lens.lens (maxResults :: GetTransitGatewayRouteTableAssociations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetTransitGatewayRouteTableAssociations)
{-# DEPRECATED gtgrtaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtaTransitGatewayRouteTableId :: Lens.Lens' GetTransitGatewayRouteTableAssociations Lude.Text
gtgrtaTransitGatewayRouteTableId = Lens.lens (transitGatewayRouteTableId :: GetTransitGatewayRouteTableAssociations -> Lude.Text) (\s a -> s {transitGatewayRouteTableId = a} :: GetTransitGatewayRouteTableAssociations)
{-# DEPRECATED gtgrtaTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

instance Page.AWSPager GetTransitGatewayRouteTableAssociations where
  page rq rs
    | Page.stop (rs Lens.^. gtgrtarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gtgrtarsAssociations) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gtgrtaNextToken Lens..~ rs Lens.^. gtgrtarsNextToken

instance Lude.AWSRequest GetTransitGatewayRouteTableAssociations where
  type
    Rs GetTransitGatewayRouteTableAssociations =
      GetTransitGatewayRouteTableAssociationsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetTransitGatewayRouteTableAssociationsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "associations" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTransitGatewayRouteTableAssociations where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetTransitGatewayRouteTableAssociations where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTransitGatewayRouteTableAssociations where
  toQuery GetTransitGatewayRouteTableAssociations' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GetTransitGatewayRouteTableAssociations" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults,
        "TransitGatewayRouteTableId" Lude.=: transitGatewayRouteTableId
      ]

-- | /See:/ 'mkGetTransitGatewayRouteTableAssociationsResponse' smart constructor.
data GetTransitGatewayRouteTableAssociationsResponse = GetTransitGatewayRouteTableAssociationsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    associations ::
      Lude.Maybe
        [TransitGatewayRouteTableAssociation],
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

-- | Creates a value of 'GetTransitGatewayRouteTableAssociationsResponse' with the minimum fields required to make a request.
--
-- * 'associations' - Information about the associations.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkGetTransitGatewayRouteTableAssociationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTransitGatewayRouteTableAssociationsResponse
mkGetTransitGatewayRouteTableAssociationsResponse pResponseStatus_ =
  GetTransitGatewayRouteTableAssociationsResponse'
    { nextToken =
        Lude.Nothing,
      associations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtarsNextToken :: Lens.Lens' GetTransitGatewayRouteTableAssociationsResponse (Lude.Maybe Lude.Text)
gtgrtarsNextToken = Lens.lens (nextToken :: GetTransitGatewayRouteTableAssociationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTransitGatewayRouteTableAssociationsResponse)
{-# DEPRECATED gtgrtarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the associations.
--
-- /Note:/ Consider using 'associations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtarsAssociations :: Lens.Lens' GetTransitGatewayRouteTableAssociationsResponse (Lude.Maybe [TransitGatewayRouteTableAssociation])
gtgrtarsAssociations = Lens.lens (associations :: GetTransitGatewayRouteTableAssociationsResponse -> Lude.Maybe [TransitGatewayRouteTableAssociation]) (\s a -> s {associations = a} :: GetTransitGatewayRouteTableAssociationsResponse)
{-# DEPRECATED gtgrtarsAssociations "Use generic-lens or generic-optics with 'associations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtarsResponseStatus :: Lens.Lens' GetTransitGatewayRouteTableAssociationsResponse Lude.Int
gtgrtarsResponseStatus = Lens.lens (responseStatus :: GetTransitGatewayRouteTableAssociationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTransitGatewayRouteTableAssociationsResponse)
{-# DEPRECATED gtgrtarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
