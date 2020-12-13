{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetTransitGatewayPrefixListReferences
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the prefix list references in a specified transit gateway route table.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetTransitGatewayPrefixListReferences
  ( -- * Creating a request
    GetTransitGatewayPrefixListReferences (..),
    mkGetTransitGatewayPrefixListReferences,

    -- ** Request lenses
    gtgplrTransitGatewayRouteTableId,
    gtgplrFilters,
    gtgplrNextToken,
    gtgplrDryRun,
    gtgplrMaxResults,

    -- * Destructuring the response
    GetTransitGatewayPrefixListReferencesResponse (..),
    mkGetTransitGatewayPrefixListReferencesResponse,

    -- ** Response lenses
    gtgplrrsTransitGatewayPrefixListReferences,
    gtgplrrsNextToken,
    gtgplrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTransitGatewayPrefixListReferences' smart constructor.
data GetTransitGatewayPrefixListReferences = GetTransitGatewayPrefixListReferences'
  { -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Lude.Text,
    -- | One or more filters. The possible values are:
    --
    --
    --     * @attachment.resource-id@ - The ID of the resource for the attachment.
    --
    --
    --     * @attachment.resource-type@ - The type of resource for the attachment. Valid values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @peering@ .
    --
    --
    --     * @attachment.transit-gateway-attachment-id@ - The ID of the attachment.
    --
    --
    --     * @is-blackhole@ - Whether traffic matching the route is blocked (@true@ | @false@ ).
    --
    --
    --     * @prefix-list-id@ - The ID of the prefix list.
    --
    --
    --     * @prefix-list-owner-id@ - The ID of the owner of the prefix list.
    --
    --
    --     * @state@ - The state of the prefix list reference (@pending@ | @available@ | @modifying@ | @deleting@ ).
    filters :: Lude.Maybe [Filter],
    -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTransitGatewayPrefixListReferences' with the minimum fields required to make a request.
--
-- * 'transitGatewayRouteTableId' - The ID of the transit gateway route table.
-- * 'filters' - One or more filters. The possible values are:
--
--
--     * @attachment.resource-id@ - The ID of the resource for the attachment.
--
--
--     * @attachment.resource-type@ - The type of resource for the attachment. Valid values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @peering@ .
--
--
--     * @attachment.transit-gateway-attachment-id@ - The ID of the attachment.
--
--
--     * @is-blackhole@ - Whether traffic matching the route is blocked (@true@ | @false@ ).
--
--
--     * @prefix-list-id@ - The ID of the prefix list.
--
--
--     * @prefix-list-owner-id@ - The ID of the owner of the prefix list.
--
--
--     * @state@ - The state of the prefix list reference (@pending@ | @available@ | @modifying@ | @deleting@ ).
--
--
-- * 'nextToken' - The token for the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
mkGetTransitGatewayPrefixListReferences ::
  -- | 'transitGatewayRouteTableId'
  Lude.Text ->
  GetTransitGatewayPrefixListReferences
mkGetTransitGatewayPrefixListReferences
  pTransitGatewayRouteTableId_ =
    GetTransitGatewayPrefixListReferences'
      { transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_,
        filters = Lude.Nothing,
        nextToken = Lude.Nothing,
        dryRun = Lude.Nothing,
        maxResults = Lude.Nothing
      }

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgplrTransitGatewayRouteTableId :: Lens.Lens' GetTransitGatewayPrefixListReferences Lude.Text
gtgplrTransitGatewayRouteTableId = Lens.lens (transitGatewayRouteTableId :: GetTransitGatewayPrefixListReferences -> Lude.Text) (\s a -> s {transitGatewayRouteTableId = a} :: GetTransitGatewayPrefixListReferences)
{-# DEPRECATED gtgplrTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | One or more filters. The possible values are:
--
--
--     * @attachment.resource-id@ - The ID of the resource for the attachment.
--
--
--     * @attachment.resource-type@ - The type of resource for the attachment. Valid values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @peering@ .
--
--
--     * @attachment.transit-gateway-attachment-id@ - The ID of the attachment.
--
--
--     * @is-blackhole@ - Whether traffic matching the route is blocked (@true@ | @false@ ).
--
--
--     * @prefix-list-id@ - The ID of the prefix list.
--
--
--     * @prefix-list-owner-id@ - The ID of the owner of the prefix list.
--
--
--     * @state@ - The state of the prefix list reference (@pending@ | @available@ | @modifying@ | @deleting@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgplrFilters :: Lens.Lens' GetTransitGatewayPrefixListReferences (Lude.Maybe [Filter])
gtgplrFilters = Lens.lens (filters :: GetTransitGatewayPrefixListReferences -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: GetTransitGatewayPrefixListReferences)
{-# DEPRECATED gtgplrFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgplrNextToken :: Lens.Lens' GetTransitGatewayPrefixListReferences (Lude.Maybe Lude.Text)
gtgplrNextToken = Lens.lens (nextToken :: GetTransitGatewayPrefixListReferences -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTransitGatewayPrefixListReferences)
{-# DEPRECATED gtgplrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgplrDryRun :: Lens.Lens' GetTransitGatewayPrefixListReferences (Lude.Maybe Lude.Bool)
gtgplrDryRun = Lens.lens (dryRun :: GetTransitGatewayPrefixListReferences -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: GetTransitGatewayPrefixListReferences)
{-# DEPRECATED gtgplrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgplrMaxResults :: Lens.Lens' GetTransitGatewayPrefixListReferences (Lude.Maybe Lude.Natural)
gtgplrMaxResults = Lens.lens (maxResults :: GetTransitGatewayPrefixListReferences -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetTransitGatewayPrefixListReferences)
{-# DEPRECATED gtgplrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetTransitGatewayPrefixListReferences where
  page rq rs
    | Page.stop (rs Lens.^. gtgplrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gtgplrrsTransitGatewayPrefixListReferences) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gtgplrNextToken Lens..~ rs Lens.^. gtgplrrsNextToken

instance Lude.AWSRequest GetTransitGatewayPrefixListReferences where
  type
    Rs GetTransitGatewayPrefixListReferences =
      GetTransitGatewayPrefixListReferencesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetTransitGatewayPrefixListReferencesResponse'
            Lude.<$> ( x Lude..@? "transitGatewayPrefixListReferenceSet"
                         Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTransitGatewayPrefixListReferences where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetTransitGatewayPrefixListReferences where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTransitGatewayPrefixListReferences where
  toQuery GetTransitGatewayPrefixListReferences' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GetTransitGatewayPrefixListReferences" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "TransitGatewayRouteTableId" Lude.=: transitGatewayRouteTableId,
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkGetTransitGatewayPrefixListReferencesResponse' smart constructor.
data GetTransitGatewayPrefixListReferencesResponse = GetTransitGatewayPrefixListReferencesResponse'
  { -- | Information about the prefix list references.
    transitGatewayPrefixListReferences :: Lude.Maybe [TransitGatewayPrefixListReference],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTransitGatewayPrefixListReferencesResponse' with the minimum fields required to make a request.
--
-- * 'transitGatewayPrefixListReferences' - Information about the prefix list references.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkGetTransitGatewayPrefixListReferencesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTransitGatewayPrefixListReferencesResponse
mkGetTransitGatewayPrefixListReferencesResponse pResponseStatus_ =
  GetTransitGatewayPrefixListReferencesResponse'
    { transitGatewayPrefixListReferences =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the prefix list references.
--
-- /Note:/ Consider using 'transitGatewayPrefixListReferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgplrrsTransitGatewayPrefixListReferences :: Lens.Lens' GetTransitGatewayPrefixListReferencesResponse (Lude.Maybe [TransitGatewayPrefixListReference])
gtgplrrsTransitGatewayPrefixListReferences = Lens.lens (transitGatewayPrefixListReferences :: GetTransitGatewayPrefixListReferencesResponse -> Lude.Maybe [TransitGatewayPrefixListReference]) (\s a -> s {transitGatewayPrefixListReferences = a} :: GetTransitGatewayPrefixListReferencesResponse)
{-# DEPRECATED gtgplrrsTransitGatewayPrefixListReferences "Use generic-lens or generic-optics with 'transitGatewayPrefixListReferences' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgplrrsNextToken :: Lens.Lens' GetTransitGatewayPrefixListReferencesResponse (Lude.Maybe Lude.Text)
gtgplrrsNextToken = Lens.lens (nextToken :: GetTransitGatewayPrefixListReferencesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTransitGatewayPrefixListReferencesResponse)
{-# DEPRECATED gtgplrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgplrrsResponseStatus :: Lens.Lens' GetTransitGatewayPrefixListReferencesResponse Lude.Int
gtgplrrsResponseStatus = Lens.lens (responseStatus :: GetTransitGatewayPrefixListReferencesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTransitGatewayPrefixListReferencesResponse)
{-# DEPRECATED gtgplrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
