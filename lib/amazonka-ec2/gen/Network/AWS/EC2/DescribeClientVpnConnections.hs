{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeClientVpnConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes active client connections and connections that have been terminated within the last 60 minutes for the specified Client VPN endpoint.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeClientVpnConnections
    (
    -- * Creating a request
      DescribeClientVpnConnections (..)
    , mkDescribeClientVpnConnections
    -- ** Request lenses
    , dcvcClientVpnEndpointId
    , dcvcDryRun
    , dcvcFilters
    , dcvcMaxResults
    , dcvcNextToken

    -- * Destructuring the response
    , DescribeClientVpnConnectionsResponse (..)
    , mkDescribeClientVpnConnectionsResponse
    -- ** Response lenses
    , dcvcrrsConnections
    , dcvcrrsNextToken
    , dcvcrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeClientVpnConnections' smart constructor.
data DescribeClientVpnConnections = DescribeClientVpnConnections'
  { clientVpnEndpointId :: Types.ClientVpnEndpointId
    -- ^ The ID of the Client VPN endpoint.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters. Filter names and values are case-sensitive.
--
--
--     * @connection-id@ - The ID of the connection.
--
--
--     * @username@ - For Active Directory client authentication, the user name of the client who established the client connection.
--
--
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to retrieve the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClientVpnConnections' value with any optional fields omitted.
mkDescribeClientVpnConnections
    :: Types.ClientVpnEndpointId -- ^ 'clientVpnEndpointId'
    -> DescribeClientVpnConnections
mkDescribeClientVpnConnections clientVpnEndpointId
  = DescribeClientVpnConnections'{clientVpnEndpointId,
                                  dryRun = Core.Nothing, filters = Core.Nothing,
                                  maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvcClientVpnEndpointId :: Lens.Lens' DescribeClientVpnConnections Types.ClientVpnEndpointId
dcvcClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# INLINEABLE dcvcClientVpnEndpointId #-}
{-# DEPRECATED clientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvcDryRun :: Lens.Lens' DescribeClientVpnConnections (Core.Maybe Core.Bool)
dcvcDryRun = Lens.field @"dryRun"
{-# INLINEABLE dcvcDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters. Filter names and values are case-sensitive.
--
--
--     * @connection-id@ - The ID of the connection.
--
--
--     * @username@ - For Active Directory client authentication, the user name of the client who established the client connection.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvcFilters :: Lens.Lens' DescribeClientVpnConnections (Core.Maybe [Types.Filter])
dcvcFilters = Lens.field @"filters"
{-# INLINEABLE dcvcFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvcMaxResults :: Lens.Lens' DescribeClientVpnConnections (Core.Maybe Core.Natural)
dcvcMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dcvcMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvcNextToken :: Lens.Lens' DescribeClientVpnConnections (Core.Maybe Types.NextToken)
dcvcNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcvcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeClientVpnConnections where
        toQuery DescribeClientVpnConnections{..}
          = Core.toQueryPair "Action"
              ("DescribeClientVpnConnections" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ClientVpnEndpointId" clientVpnEndpointId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeClientVpnConnections where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeClientVpnConnections where
        type Rs DescribeClientVpnConnections =
             DescribeClientVpnConnectionsResponse
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
                 DescribeClientVpnConnectionsResponse' Core.<$>
                   (x Core..@? "connections" Core..<@> Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeClientVpnConnections where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"connections" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeClientVpnConnectionsResponse' smart constructor.
data DescribeClientVpnConnectionsResponse = DescribeClientVpnConnectionsResponse'
  { connections :: Core.Maybe [Types.ClientVpnConnection]
    -- ^ Information about the active and terminated client connections.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClientVpnConnectionsResponse' value with any optional fields omitted.
mkDescribeClientVpnConnectionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeClientVpnConnectionsResponse
mkDescribeClientVpnConnectionsResponse responseStatus
  = DescribeClientVpnConnectionsResponse'{connections = Core.Nothing,
                                          nextToken = Core.Nothing, responseStatus}

-- | Information about the active and terminated client connections.
--
-- /Note:/ Consider using 'connections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvcrrsConnections :: Lens.Lens' DescribeClientVpnConnectionsResponse (Core.Maybe [Types.ClientVpnConnection])
dcvcrrsConnections = Lens.field @"connections"
{-# INLINEABLE dcvcrrsConnections #-}
{-# DEPRECATED connections "Use generic-lens or generic-optics with 'connections' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvcrrsNextToken :: Lens.Lens' DescribeClientVpnConnectionsResponse (Core.Maybe Types.NextToken)
dcvcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcvcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvcrrsResponseStatus :: Lens.Lens' DescribeClientVpnConnectionsResponse Core.Int
dcvcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcvcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
