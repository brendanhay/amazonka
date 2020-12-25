{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the associations between virtual interface groups and local gateway route tables.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
  ( -- * Creating a request
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (..),
    mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations,

    -- ** Request lenses
    dlgrtvigaDryRun,
    dlgrtvigaFilters,
    dlgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationIds,
    dlgrtvigaMaxResults,
    dlgrtvigaNextToken,

    -- * Destructuring the response
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse (..),
    mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse,

    -- ** Response lenses
    dlgrtvigarrsLocalGatewayRouteTableVirtualInterfaceGroupAssociations,
    dlgrtvigarrsNextToken,
    dlgrtvigarrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' smart constructor.
data DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations = DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters.
    --
    --
    --     * @local-gateway-id@ - The ID of a local gateway.
    --
    --
    --     * @local-gateway-route-table-id@ - The ID of the local gateway route table.
    --
    --
    --     * @local-gateway-route-table-virtual-interface-group-association-id@ - The ID of the association.
    --
    --
    --     * @local-gateway-route-table-virtual-interface-group-id@ - The ID of the virtual interface group.
    --
    --
    --     * @state@ - The state of the association.
    filters :: Core.Maybe [Types.Filter],
    -- | The IDs of the associations.
    localGatewayRouteTableVirtualInterfaceGroupAssociationIds :: Core.Maybe [Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociationId],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' value with any optional fields omitted.
mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations ::
  DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations =
  DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations'
    { dryRun =
        Core.Nothing,
      filters = Core.Nothing,
      localGatewayRouteTableVirtualInterfaceGroupAssociationIds =
        Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigaDryRun :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Core.Maybe Core.Bool)
dlgrtvigaDryRun = Lens.field @"dryRun"
{-# DEPRECATED dlgrtvigaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more filters.
--
--
--     * @local-gateway-id@ - The ID of a local gateway.
--
--
--     * @local-gateway-route-table-id@ - The ID of the local gateway route table.
--
--
--     * @local-gateway-route-table-virtual-interface-group-association-id@ - The ID of the association.
--
--
--     * @local-gateway-route-table-virtual-interface-group-id@ - The ID of the virtual interface group.
--
--
--     * @state@ - The state of the association.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigaFilters :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Core.Maybe [Types.Filter])
dlgrtvigaFilters = Lens.field @"filters"
{-# DEPRECATED dlgrtvigaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The IDs of the associations.
--
-- /Note:/ Consider using 'localGatewayRouteTableVirtualInterfaceGroupAssociationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationIds :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Core.Maybe [Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociationId])
dlgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationIds = Lens.field @"localGatewayRouteTableVirtualInterfaceGroupAssociationIds"
{-# DEPRECATED dlgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationIds "Use generic-lens or generic-optics with 'localGatewayRouteTableVirtualInterfaceGroupAssociationIds' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigaMaxResults :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Core.Maybe Core.Natural)
dlgrtvigaMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dlgrtvigaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigaNextToken :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Core.Maybe Types.NextToken)
dlgrtvigaNextToken = Lens.field @"nextToken"
{-# DEPRECATED dlgrtvigaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance
  Core.AWSRequest
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
  where
  type
    Rs
      DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations =
      DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure
                ( "Action",
                  "DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations"
                )
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> ( Core.toQueryList
                            "LocalGatewayRouteTableVirtualInterfaceGroupAssociationId"
                            Core.<$> localGatewayRouteTableVirtualInterfaceGroupAssociationIds
                        )
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse'
            Core.<$> ( x
                         Core..@? "localGatewayRouteTableVirtualInterfaceGroupAssociationSet"
                         Core..<@> Core.parseXMLList "item"
                     )
              Core.<*> (x Core..@? "nextToken")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Pager.AWSPager
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
  where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field
              @"localGatewayRouteTableVirtualInterfaceGroupAssociations"
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse' smart constructor.
data DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse = DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse'
  { -- | Information about the associations.
    localGatewayRouteTableVirtualInterfaceGroupAssociations :: Core.Maybe [Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociation],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse' value with any optional fields omitted.
mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
  responseStatus =
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse'
      { localGatewayRouteTableVirtualInterfaceGroupAssociations =
          Core.Nothing,
        nextToken =
          Core.Nothing,
        responseStatus
      }

-- | Information about the associations.
--
-- /Note:/ Consider using 'localGatewayRouteTableVirtualInterfaceGroupAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigarrsLocalGatewayRouteTableVirtualInterfaceGroupAssociations :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse (Core.Maybe [Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociation])
dlgrtvigarrsLocalGatewayRouteTableVirtualInterfaceGroupAssociations = Lens.field @"localGatewayRouteTableVirtualInterfaceGroupAssociations"
{-# DEPRECATED dlgrtvigarrsLocalGatewayRouteTableVirtualInterfaceGroupAssociations "Use generic-lens or generic-optics with 'localGatewayRouteTableVirtualInterfaceGroupAssociations' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigarrsNextToken :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse (Core.Maybe Types.String)
dlgrtvigarrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dlgrtvigarrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigarrsResponseStatus :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse Core.Int
dlgrtvigarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlgrtvigarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
