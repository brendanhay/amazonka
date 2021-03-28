{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DescribeInboundCrossClusterSearchConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the inbound cross-cluster search connections for a destination domain.
module Network.AWS.ElasticSearch.DescribeInboundCrossClusterSearchConnections
    (
    -- * Creating a request
      DescribeInboundCrossClusterSearchConnections (..)
    , mkDescribeInboundCrossClusterSearchConnections
    -- ** Request lenses
    , diccscFilters
    , diccscMaxResults
    , diccscNextToken

    -- * Destructuring the response
    , DescribeInboundCrossClusterSearchConnectionsResponse (..)
    , mkDescribeInboundCrossClusterSearchConnectionsResponse
    -- ** Response lenses
    , diccscrrsCrossClusterSearchConnections
    , diccscrrsNextToken
    , diccscrrsResponseStatus
    ) where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DescribeInboundCrossClusterSearchConnections' @ operation.
--
-- /See:/ 'mkDescribeInboundCrossClusterSearchConnections' smart constructor.
data DescribeInboundCrossClusterSearchConnections = DescribeInboundCrossClusterSearchConnections'
  { filters :: Core.Maybe [Types.Filter]
    -- ^ A list of filters used to match properties for inbound cross-cluster search connection. Available @'Filter' @ names for this operation are: 
--
--     * cross-cluster-search-connection-id
--
--     * source-domain-info.domain-name
--
--     * source-domain-info.owner-id
--
--     * source-domain-info.region
--
--     * destination-domain-info.domain-name
--
--
  , maxResults :: Core.Maybe Core.Int
    -- ^ Set this value to limit the number of results returned. If not specified, defaults to 100.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ NextToken is sent in case the earlier API call results contain the NextToken. It is used for pagination.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInboundCrossClusterSearchConnections' value with any optional fields omitted.
mkDescribeInboundCrossClusterSearchConnections
    :: DescribeInboundCrossClusterSearchConnections
mkDescribeInboundCrossClusterSearchConnections
  = DescribeInboundCrossClusterSearchConnections'{filters =
                                                    Core.Nothing,
                                                  maxResults = Core.Nothing,
                                                  nextToken = Core.Nothing}

-- | A list of filters used to match properties for inbound cross-cluster search connection. Available @'Filter' @ names for this operation are: 
--
--     * cross-cluster-search-connection-id
--
--     * source-domain-info.domain-name
--
--     * source-domain-info.owner-id
--
--     * source-domain-info.region
--
--     * destination-domain-info.domain-name
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diccscFilters :: Lens.Lens' DescribeInboundCrossClusterSearchConnections (Core.Maybe [Types.Filter])
diccscFilters = Lens.field @"filters"
{-# INLINEABLE diccscFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | Set this value to limit the number of results returned. If not specified, defaults to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diccscMaxResults :: Lens.Lens' DescribeInboundCrossClusterSearchConnections (Core.Maybe Core.Int)
diccscMaxResults = Lens.field @"maxResults"
{-# INLINEABLE diccscMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | NextToken is sent in case the earlier API call results contain the NextToken. It is used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diccscNextToken :: Lens.Lens' DescribeInboundCrossClusterSearchConnections (Core.Maybe Types.NextToken)
diccscNextToken = Lens.field @"nextToken"
{-# INLINEABLE diccscNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeInboundCrossClusterSearchConnections
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders
           DescribeInboundCrossClusterSearchConnections
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON DescribeInboundCrossClusterSearchConnections
         where
        toJSON DescribeInboundCrossClusterSearchConnections{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest
           DescribeInboundCrossClusterSearchConnections
         where
        type Rs DescribeInboundCrossClusterSearchConnections =
             DescribeInboundCrossClusterSearchConnectionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/2015-01-01/es/ccs/inboundConnection/search",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeInboundCrossClusterSearchConnectionsResponse' Core.<$>
                   (x Core..:? "CrossClusterSearchConnections") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @'DescribeInboundCrossClusterSearchConnections' @ request. Contains the list of connections matching the filter criteria.
--
-- /See:/ 'mkDescribeInboundCrossClusterSearchConnectionsResponse' smart constructor.
data DescribeInboundCrossClusterSearchConnectionsResponse = DescribeInboundCrossClusterSearchConnectionsResponse'
  { crossClusterSearchConnections :: Core.Maybe [Types.InboundCrossClusterSearchConnection]
    -- ^ Consists of list of @'InboundCrossClusterSearchConnection' @ matching the specified filter criteria.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If more results are available and NextToken is present, make the next request to the same API with the received NextToken to paginate the remaining results. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInboundCrossClusterSearchConnectionsResponse' value with any optional fields omitted.
mkDescribeInboundCrossClusterSearchConnectionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeInboundCrossClusterSearchConnectionsResponse
mkDescribeInboundCrossClusterSearchConnectionsResponse
  responseStatus
  = DescribeInboundCrossClusterSearchConnectionsResponse'{crossClusterSearchConnections
                                                            = Core.Nothing,
                                                          nextToken = Core.Nothing, responseStatus}

-- | Consists of list of @'InboundCrossClusterSearchConnection' @ matching the specified filter criteria.
--
-- /Note:/ Consider using 'crossClusterSearchConnections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diccscrrsCrossClusterSearchConnections :: Lens.Lens' DescribeInboundCrossClusterSearchConnectionsResponse (Core.Maybe [Types.InboundCrossClusterSearchConnection])
diccscrrsCrossClusterSearchConnections = Lens.field @"crossClusterSearchConnections"
{-# INLINEABLE diccscrrsCrossClusterSearchConnections #-}
{-# DEPRECATED crossClusterSearchConnections "Use generic-lens or generic-optics with 'crossClusterSearchConnections' instead"  #-}

-- | If more results are available and NextToken is present, make the next request to the same API with the received NextToken to paginate the remaining results. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diccscrrsNextToken :: Lens.Lens' DescribeInboundCrossClusterSearchConnectionsResponse (Core.Maybe Types.NextToken)
diccscrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE diccscrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diccscrrsResponseStatus :: Lens.Lens' DescribeInboundCrossClusterSearchConnectionsResponse Core.Int
diccscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE diccscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
