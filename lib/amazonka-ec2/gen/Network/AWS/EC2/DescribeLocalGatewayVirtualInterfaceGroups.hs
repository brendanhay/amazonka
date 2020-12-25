{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeLocalGatewayVirtualInterfaceGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified local gateway virtual interface groups.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLocalGatewayVirtualInterfaceGroups
  ( -- * Creating a request
    DescribeLocalGatewayVirtualInterfaceGroups (..),
    mkDescribeLocalGatewayVirtualInterfaceGroups,

    -- ** Request lenses
    dlgvigDryRun,
    dlgvigFilters,
    dlgvigLocalGatewayVirtualInterfaceGroupIds,
    dlgvigMaxResults,
    dlgvigNextToken,

    -- * Destructuring the response
    DescribeLocalGatewayVirtualInterfaceGroupsResponse (..),
    mkDescribeLocalGatewayVirtualInterfaceGroupsResponse,

    -- ** Response lenses
    dlgvigrrsLocalGatewayVirtualInterfaceGroups,
    dlgvigrrsNextToken,
    dlgvigrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLocalGatewayVirtualInterfaceGroups' smart constructor.
data DescribeLocalGatewayVirtualInterfaceGroups = DescribeLocalGatewayVirtualInterfaceGroups'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters.
    --
    --
    --     * @local-gateway-id@ - The ID of a local gateway.
    --
    --
    --     * @local-gateway-virtual-interface-id@ - The ID of the virtual interface.
    --
    --
    --     * @local-gateway-virtual-interface-group-id@ - The ID of the virtual interface group.
    filters :: Core.Maybe [Types.Filter],
    -- | The IDs of the virtual interface groups.
    localGatewayVirtualInterfaceGroupIds :: Core.Maybe [Types.LocalGatewayVirtualInterfaceGroupId],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLocalGatewayVirtualInterfaceGroups' value with any optional fields omitted.
mkDescribeLocalGatewayVirtualInterfaceGroups ::
  DescribeLocalGatewayVirtualInterfaceGroups
mkDescribeLocalGatewayVirtualInterfaceGroups =
  DescribeLocalGatewayVirtualInterfaceGroups'
    { dryRun =
        Core.Nothing,
      filters = Core.Nothing,
      localGatewayVirtualInterfaceGroupIds = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgvigDryRun :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Core.Maybe Core.Bool)
dlgvigDryRun = Lens.field @"dryRun"
{-# DEPRECATED dlgvigDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more filters.
--
--
--     * @local-gateway-id@ - The ID of a local gateway.
--
--
--     * @local-gateway-virtual-interface-id@ - The ID of the virtual interface.
--
--
--     * @local-gateway-virtual-interface-group-id@ - The ID of the virtual interface group.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgvigFilters :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Core.Maybe [Types.Filter])
dlgvigFilters = Lens.field @"filters"
{-# DEPRECATED dlgvigFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The IDs of the virtual interface groups.
--
-- /Note:/ Consider using 'localGatewayVirtualInterfaceGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgvigLocalGatewayVirtualInterfaceGroupIds :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Core.Maybe [Types.LocalGatewayVirtualInterfaceGroupId])
dlgvigLocalGatewayVirtualInterfaceGroupIds = Lens.field @"localGatewayVirtualInterfaceGroupIds"
{-# DEPRECATED dlgvigLocalGatewayVirtualInterfaceGroupIds "Use generic-lens or generic-optics with 'localGatewayVirtualInterfaceGroupIds' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgvigMaxResults :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Core.Maybe Core.Natural)
dlgvigMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dlgvigMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgvigNextToken :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Core.Maybe Types.String)
dlgvigNextToken = Lens.field @"nextToken"
{-# DEPRECATED dlgvigNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeLocalGatewayVirtualInterfaceGroups where
  type
    Rs DescribeLocalGatewayVirtualInterfaceGroups =
      DescribeLocalGatewayVirtualInterfaceGroupsResponse
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
            ( Core.pure ("Action", "DescribeLocalGatewayVirtualInterfaceGroups")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> ( Core.toQueryList "LocalGatewayVirtualInterfaceGroupId"
                            Core.<$> localGatewayVirtualInterfaceGroupIds
                        )
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeLocalGatewayVirtualInterfaceGroupsResponse'
            Core.<$> ( x Core..@? "localGatewayVirtualInterfaceGroupSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeLocalGatewayVirtualInterfaceGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"localGatewayVirtualInterfaceGroups" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeLocalGatewayVirtualInterfaceGroupsResponse' smart constructor.
data DescribeLocalGatewayVirtualInterfaceGroupsResponse = DescribeLocalGatewayVirtualInterfaceGroupsResponse'
  { -- | The virtual interface groups.
    localGatewayVirtualInterfaceGroups :: Core.Maybe [Types.LocalGatewayVirtualInterfaceGroup],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLocalGatewayVirtualInterfaceGroupsResponse' value with any optional fields omitted.
mkDescribeLocalGatewayVirtualInterfaceGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeLocalGatewayVirtualInterfaceGroupsResponse
mkDescribeLocalGatewayVirtualInterfaceGroupsResponse responseStatus =
  DescribeLocalGatewayVirtualInterfaceGroupsResponse'
    { localGatewayVirtualInterfaceGroups =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The virtual interface groups.
--
-- /Note:/ Consider using 'localGatewayVirtualInterfaceGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgvigrrsLocalGatewayVirtualInterfaceGroups :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroupsResponse (Core.Maybe [Types.LocalGatewayVirtualInterfaceGroup])
dlgvigrrsLocalGatewayVirtualInterfaceGroups = Lens.field @"localGatewayVirtualInterfaceGroups"
{-# DEPRECATED dlgvigrrsLocalGatewayVirtualInterfaceGroups "Use generic-lens or generic-optics with 'localGatewayVirtualInterfaceGroups' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgvigrrsNextToken :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroupsResponse (Core.Maybe Types.String)
dlgvigrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dlgvigrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgvigrrsResponseStatus :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroupsResponse Core.Int
dlgvigrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlgvigrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
