{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about reserved Elasticsearch instances for this account.
--
-- This operation returns paginated results.
module Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstances
    (
    -- * Creating a request
      DescribeReservedElasticsearchInstances (..)
    , mkDescribeReservedElasticsearchInstances
    -- ** Request lenses
    , dreiMaxResults
    , dreiNextToken
    , dreiReservedElasticsearchInstanceId

    -- * Destructuring the response
    , DescribeReservedElasticsearchInstancesResponse (..)
    , mkDescribeReservedElasticsearchInstancesResponse
    -- ** Response lenses
    , dreirrsNextToken
    , dreirrsReservedElasticsearchInstances
    , dreirrsResponseStatus
    ) where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for parameters to @DescribeReservedElasticsearchInstances@ 
--
-- /See:/ 'mkDescribeReservedElasticsearchInstances' smart constructor.
data DescribeReservedElasticsearchInstances = DescribeReservedElasticsearchInstances'
  { maxResults :: Core.Maybe Core.Int
    -- ^ Set this value to limit the number of results returned. If not specified, defaults to 100.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ NextToken should be sent in case if earlier API call produced result containing NextToken. It is used for pagination.
  , reservedElasticsearchInstanceId :: Core.Maybe Types.ReservedElasticsearchInstanceId
    -- ^ The reserved instance identifier filter value. Use this parameter to show only the reservation that matches the specified reserved Elasticsearch instance ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReservedElasticsearchInstances' value with any optional fields omitted.
mkDescribeReservedElasticsearchInstances
    :: DescribeReservedElasticsearchInstances
mkDescribeReservedElasticsearchInstances
  = DescribeReservedElasticsearchInstances'{maxResults =
                                              Core.Nothing,
                                            nextToken = Core.Nothing,
                                            reservedElasticsearchInstanceId = Core.Nothing}

-- | Set this value to limit the number of results returned. If not specified, defaults to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreiMaxResults :: Lens.Lens' DescribeReservedElasticsearchInstances (Core.Maybe Core.Int)
dreiMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dreiMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | NextToken should be sent in case if earlier API call produced result containing NextToken. It is used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreiNextToken :: Lens.Lens' DescribeReservedElasticsearchInstances (Core.Maybe Types.NextToken)
dreiNextToken = Lens.field @"nextToken"
{-# INLINEABLE dreiNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The reserved instance identifier filter value. Use this parameter to show only the reservation that matches the specified reserved Elasticsearch instance ID.
--
-- /Note:/ Consider using 'reservedElasticsearchInstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreiReservedElasticsearchInstanceId :: Lens.Lens' DescribeReservedElasticsearchInstances (Core.Maybe Types.ReservedElasticsearchInstanceId)
dreiReservedElasticsearchInstanceId = Lens.field @"reservedElasticsearchInstanceId"
{-# INLINEABLE dreiReservedElasticsearchInstanceId #-}
{-# DEPRECATED reservedElasticsearchInstanceId "Use generic-lens or generic-optics with 'reservedElasticsearchInstanceId' instead"  #-}

instance Core.ToQuery DescribeReservedElasticsearchInstances where
        toQuery DescribeReservedElasticsearchInstances{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "reservationId")
                reservedElasticsearchInstanceId

instance Core.ToHeaders DescribeReservedElasticsearchInstances
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeReservedElasticsearchInstances
         where
        type Rs DescribeReservedElasticsearchInstances =
             DescribeReservedElasticsearchInstancesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2015-01-01/es/reservedInstances",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeReservedElasticsearchInstancesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*>
                     x Core..:? "ReservedElasticsearchInstances"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeReservedElasticsearchInstances
         where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"reservedElasticsearchInstances" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Container for results from @DescribeReservedElasticsearchInstances@ 
--
-- /See:/ 'mkDescribeReservedElasticsearchInstancesResponse' smart constructor.
data DescribeReservedElasticsearchInstancesResponse = DescribeReservedElasticsearchInstancesResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ Provides an identifier to allow retrieval of paginated results.
  , reservedElasticsearchInstances :: Core.Maybe [Types.ReservedElasticsearchInstance]
    -- ^ List of reserved Elasticsearch instances.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeReservedElasticsearchInstancesResponse' value with any optional fields omitted.
mkDescribeReservedElasticsearchInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeReservedElasticsearchInstancesResponse
mkDescribeReservedElasticsearchInstancesResponse responseStatus
  = DescribeReservedElasticsearchInstancesResponse'{nextToken =
                                                      Core.Nothing,
                                                    reservedElasticsearchInstances = Core.Nothing,
                                                    responseStatus}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreirrsNextToken :: Lens.Lens' DescribeReservedElasticsearchInstancesResponse (Core.Maybe Core.Text)
dreirrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dreirrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | List of reserved Elasticsearch instances.
--
-- /Note:/ Consider using 'reservedElasticsearchInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreirrsReservedElasticsearchInstances :: Lens.Lens' DescribeReservedElasticsearchInstancesResponse (Core.Maybe [Types.ReservedElasticsearchInstance])
dreirrsReservedElasticsearchInstances = Lens.field @"reservedElasticsearchInstances"
{-# INLINEABLE dreirrsReservedElasticsearchInstances #-}
{-# DEPRECATED reservedElasticsearchInstances "Use generic-lens or generic-optics with 'reservedElasticsearchInstances' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreirrsResponseStatus :: Lens.Lens' DescribeReservedElasticsearchInstancesResponse Core.Int
dreirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dreirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
