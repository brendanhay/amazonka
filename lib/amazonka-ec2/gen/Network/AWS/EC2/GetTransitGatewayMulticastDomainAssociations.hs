{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetTransitGatewayMulticastDomainAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the associations for the transit gateway multicast domain.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetTransitGatewayMulticastDomainAssociations
  ( -- * Creating a request
    GetTransitGatewayMulticastDomainAssociations (..),
    mkGetTransitGatewayMulticastDomainAssociations,

    -- ** Request lenses
    gtgmdaDryRun,
    gtgmdaFilters,
    gtgmdaMaxResults,
    gtgmdaNextToken,
    gtgmdaTransitGatewayMulticastDomainId,

    -- * Destructuring the response
    GetTransitGatewayMulticastDomainAssociationsResponse (..),
    mkGetTransitGatewayMulticastDomainAssociationsResponse,

    -- ** Response lenses
    gtgmdarrsMulticastDomainAssociations,
    gtgmdarrsNextToken,
    gtgmdarrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTransitGatewayMulticastDomainAssociations' smart constructor.
data GetTransitGatewayMulticastDomainAssociations = GetTransitGatewayMulticastDomainAssociations'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters. The possible values are:
    --
    --
    --     * @resource-id@ - The ID of the resource.
    --
    --
    --     * @resource-type@ - The type of resource. The valid value is: @vpc@ .
    --
    --
    --     * @state@ - The state of the subnet association. Valid values are @associated@ | @associating@ | @disassociated@ | @disassociating@ .
    --
    --
    --     * @subnet-id@ - The ID of the subnet.
    --
    --
    --     * @transit-gateway-attachment-id@ - The id of the transit gateway attachment.
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.String,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Core.Maybe Types.TransitGatewayMulticastDomainId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTransitGatewayMulticastDomainAssociations' value with any optional fields omitted.
mkGetTransitGatewayMulticastDomainAssociations ::
  GetTransitGatewayMulticastDomainAssociations
mkGetTransitGatewayMulticastDomainAssociations =
  GetTransitGatewayMulticastDomainAssociations'
    { dryRun =
        Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      transitGatewayMulticastDomainId = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgmdaDryRun :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Core.Maybe Core.Bool)
gtgmdaDryRun = Lens.field @"dryRun"
{-# DEPRECATED gtgmdaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more filters. The possible values are:
--
--
--     * @resource-id@ - The ID of the resource.
--
--
--     * @resource-type@ - The type of resource. The valid value is: @vpc@ .
--
--
--     * @state@ - The state of the subnet association. Valid values are @associated@ | @associating@ | @disassociated@ | @disassociating@ .
--
--
--     * @subnet-id@ - The ID of the subnet.
--
--
--     * @transit-gateway-attachment-id@ - The id of the transit gateway attachment.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgmdaFilters :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Core.Maybe [Types.Filter])
gtgmdaFilters = Lens.field @"filters"
{-# DEPRECATED gtgmdaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgmdaMaxResults :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Core.Maybe Core.Natural)
gtgmdaMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gtgmdaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgmdaNextToken :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Core.Maybe Types.String)
gtgmdaNextToken = Lens.field @"nextToken"
{-# DEPRECATED gtgmdaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgmdaTransitGatewayMulticastDomainId :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Core.Maybe Types.TransitGatewayMulticastDomainId)
gtgmdaTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# DEPRECATED gtgmdaTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

instance
  Core.AWSRequest
    GetTransitGatewayMulticastDomainAssociations
  where
  type
    Rs GetTransitGatewayMulticastDomainAssociations =
      GetTransitGatewayMulticastDomainAssociationsResponse
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
                ("Action", "GetTransitGatewayMulticastDomainAssociations")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> ( Core.toQueryValue "TransitGatewayMulticastDomainId"
                            Core.<$> transitGatewayMulticastDomainId
                        )
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetTransitGatewayMulticastDomainAssociationsResponse'
            Core.<$> ( x Core..@? "multicastDomainAssociations"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Pager.AWSPager
    GetTransitGatewayMulticastDomainAssociations
  where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"multicastDomainAssociations" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetTransitGatewayMulticastDomainAssociationsResponse' smart constructor.
data GetTransitGatewayMulticastDomainAssociationsResponse = GetTransitGatewayMulticastDomainAssociationsResponse'
  { -- | Information about the multicast domain associations.
    multicastDomainAssociations :: Core.Maybe [Types.TransitGatewayMulticastDomainAssociation],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTransitGatewayMulticastDomainAssociationsResponse' value with any optional fields omitted.
mkGetTransitGatewayMulticastDomainAssociationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTransitGatewayMulticastDomainAssociationsResponse
mkGetTransitGatewayMulticastDomainAssociationsResponse
  responseStatus =
    GetTransitGatewayMulticastDomainAssociationsResponse'
      { multicastDomainAssociations =
          Core.Nothing,
        nextToken = Core.Nothing,
        responseStatus
      }

-- | Information about the multicast domain associations.
--
-- /Note:/ Consider using 'multicastDomainAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgmdarrsMulticastDomainAssociations :: Lens.Lens' GetTransitGatewayMulticastDomainAssociationsResponse (Core.Maybe [Types.TransitGatewayMulticastDomainAssociation])
gtgmdarrsMulticastDomainAssociations = Lens.field @"multicastDomainAssociations"
{-# DEPRECATED gtgmdarrsMulticastDomainAssociations "Use generic-lens or generic-optics with 'multicastDomainAssociations' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgmdarrsNextToken :: Lens.Lens' GetTransitGatewayMulticastDomainAssociationsResponse (Core.Maybe Types.String)
gtgmdarrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gtgmdarrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgmdarrsResponseStatus :: Lens.Lens' GetTransitGatewayMulticastDomainAssociationsResponse Core.Int
gtgmdarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtgmdarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
