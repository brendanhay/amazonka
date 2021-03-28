{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetTransitGatewayMulticastDomainAssociations (..)
    , mkGetTransitGatewayMulticastDomainAssociations
    -- ** Request lenses
    , gtgmdaDryRun
    , gtgmdaFilters
    , gtgmdaMaxResults
    , gtgmdaNextToken
    , gtgmdaTransitGatewayMulticastDomainId

    -- * Destructuring the response
    , GetTransitGatewayMulticastDomainAssociationsResponse (..)
    , mkGetTransitGatewayMulticastDomainAssociationsResponse
    -- ** Response lenses
    , gtgmdarrsMulticastDomainAssociations
    , gtgmdarrsNextToken
    , gtgmdarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTransitGatewayMulticastDomainAssociations' smart constructor.
data GetTransitGatewayMulticastDomainAssociations = GetTransitGatewayMulticastDomainAssociations'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters. The possible values are:
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
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next page of results.
  , transitGatewayMulticastDomainId :: Core.Maybe Types.TransitGatewayMulticastDomainId
    -- ^ The ID of the transit gateway multicast domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTransitGatewayMulticastDomainAssociations' value with any optional fields omitted.
mkGetTransitGatewayMulticastDomainAssociations
    :: GetTransitGatewayMulticastDomainAssociations
mkGetTransitGatewayMulticastDomainAssociations
  = GetTransitGatewayMulticastDomainAssociations'{dryRun =
                                                    Core.Nothing,
                                                  filters = Core.Nothing, maxResults = Core.Nothing,
                                                  nextToken = Core.Nothing,
                                                  transitGatewayMulticastDomainId = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgmdaDryRun :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Core.Maybe Core.Bool)
gtgmdaDryRun = Lens.field @"dryRun"
{-# INLINEABLE gtgmdaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

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
{-# INLINEABLE gtgmdaFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgmdaMaxResults :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Core.Maybe Core.Natural)
gtgmdaMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gtgmdaMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgmdaNextToken :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Core.Maybe Core.Text)
gtgmdaNextToken = Lens.field @"nextToken"
{-# INLINEABLE gtgmdaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgmdaTransitGatewayMulticastDomainId :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Core.Maybe Types.TransitGatewayMulticastDomainId)
gtgmdaTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# INLINEABLE gtgmdaTransitGatewayMulticastDomainId #-}
{-# DEPRECATED transitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead"  #-}

instance Core.ToQuery GetTransitGatewayMulticastDomainAssociations
         where
        toQuery GetTransitGatewayMulticastDomainAssociations{..}
          = Core.toQueryPair "Action"
              ("GetTransitGatewayMulticastDomainAssociations" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "TransitGatewayMulticastDomainId")
                transitGatewayMulticastDomainId

instance Core.ToHeaders
           GetTransitGatewayMulticastDomainAssociations
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest
           GetTransitGatewayMulticastDomainAssociations
         where
        type Rs GetTransitGatewayMulticastDomainAssociations =
             GetTransitGatewayMulticastDomainAssociationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetTransitGatewayMulticastDomainAssociationsResponse' Core.<$>
                   (x Core..@? "multicastDomainAssociations" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager
           GetTransitGatewayMulticastDomainAssociations
         where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"multicastDomainAssociations" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetTransitGatewayMulticastDomainAssociationsResponse' smart constructor.
data GetTransitGatewayMulticastDomainAssociationsResponse = GetTransitGatewayMulticastDomainAssociationsResponse'
  { multicastDomainAssociations :: Core.Maybe [Types.TransitGatewayMulticastDomainAssociation]
    -- ^ Information about the multicast domain associations.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTransitGatewayMulticastDomainAssociationsResponse' value with any optional fields omitted.
mkGetTransitGatewayMulticastDomainAssociationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTransitGatewayMulticastDomainAssociationsResponse
mkGetTransitGatewayMulticastDomainAssociationsResponse
  responseStatus
  = GetTransitGatewayMulticastDomainAssociationsResponse'{multicastDomainAssociations
                                                            = Core.Nothing,
                                                          nextToken = Core.Nothing, responseStatus}

-- | Information about the multicast domain associations.
--
-- /Note:/ Consider using 'multicastDomainAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgmdarrsMulticastDomainAssociations :: Lens.Lens' GetTransitGatewayMulticastDomainAssociationsResponse (Core.Maybe [Types.TransitGatewayMulticastDomainAssociation])
gtgmdarrsMulticastDomainAssociations = Lens.field @"multicastDomainAssociations"
{-# INLINEABLE gtgmdarrsMulticastDomainAssociations #-}
{-# DEPRECATED multicastDomainAssociations "Use generic-lens or generic-optics with 'multicastDomainAssociations' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgmdarrsNextToken :: Lens.Lens' GetTransitGatewayMulticastDomainAssociationsResponse (Core.Maybe Core.Text)
gtgmdarrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gtgmdarrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgmdarrsResponseStatus :: Lens.Lens' GetTransitGatewayMulticastDomainAssociationsResponse Core.Int
gtgmdarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtgmdarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
