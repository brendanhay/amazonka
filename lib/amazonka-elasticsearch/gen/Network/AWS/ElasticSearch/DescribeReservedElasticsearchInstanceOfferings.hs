{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstanceOfferings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists available reserved Elasticsearch instance offerings.
--
-- This operation returns paginated results.
module Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstanceOfferings
    (
    -- * Creating a request
      DescribeReservedElasticsearchInstanceOfferings (..)
    , mkDescribeReservedElasticsearchInstanceOfferings
    -- ** Request lenses
    , dreioMaxResults
    , dreioNextToken
    , dreioReservedElasticsearchInstanceOfferingId

    -- * Destructuring the response
    , DescribeReservedElasticsearchInstanceOfferingsResponse (..)
    , mkDescribeReservedElasticsearchInstanceOfferingsResponse
    -- ** Response lenses
    , dreiorrsNextToken
    , dreiorrsReservedElasticsearchInstanceOfferings
    , dreiorrsResponseStatus
    ) where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for parameters to @DescribeReservedElasticsearchInstanceOfferings@ 
--
-- /See:/ 'mkDescribeReservedElasticsearchInstanceOfferings' smart constructor.
data DescribeReservedElasticsearchInstanceOfferings = DescribeReservedElasticsearchInstanceOfferings'
  { maxResults :: Core.Maybe Core.Int
    -- ^ Set this value to limit the number of results returned. If not specified, defaults to 100.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ NextToken should be sent in case if earlier API call produced result containing NextToken. It is used for pagination.
  , reservedElasticsearchInstanceOfferingId :: Core.Maybe Types.ReservedElasticsearchInstanceOfferingId
    -- ^ The offering identifier filter value. Use this parameter to show only the available offering that matches the specified reservation identifier.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReservedElasticsearchInstanceOfferings' value with any optional fields omitted.
mkDescribeReservedElasticsearchInstanceOfferings
    :: DescribeReservedElasticsearchInstanceOfferings
mkDescribeReservedElasticsearchInstanceOfferings
  = DescribeReservedElasticsearchInstanceOfferings'{maxResults =
                                                      Core.Nothing,
                                                    nextToken = Core.Nothing,
                                                    reservedElasticsearchInstanceOfferingId =
                                                      Core.Nothing}

-- | Set this value to limit the number of results returned. If not specified, defaults to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreioMaxResults :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferings (Core.Maybe Core.Int)
dreioMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dreioMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | NextToken should be sent in case if earlier API call produced result containing NextToken. It is used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreioNextToken :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferings (Core.Maybe Types.NextToken)
dreioNextToken = Lens.field @"nextToken"
{-# INLINEABLE dreioNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The offering identifier filter value. Use this parameter to show only the available offering that matches the specified reservation identifier.
--
-- /Note:/ Consider using 'reservedElasticsearchInstanceOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreioReservedElasticsearchInstanceOfferingId :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferings (Core.Maybe Types.ReservedElasticsearchInstanceOfferingId)
dreioReservedElasticsearchInstanceOfferingId = Lens.field @"reservedElasticsearchInstanceOfferingId"
{-# INLINEABLE dreioReservedElasticsearchInstanceOfferingId #-}
{-# DEPRECATED reservedElasticsearchInstanceOfferingId "Use generic-lens or generic-optics with 'reservedElasticsearchInstanceOfferingId' instead"  #-}

instance Core.ToQuery
           DescribeReservedElasticsearchInstanceOfferings
         where
        toQuery DescribeReservedElasticsearchInstanceOfferings{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "offeringId")
                reservedElasticsearchInstanceOfferingId

instance Core.ToHeaders
           DescribeReservedElasticsearchInstanceOfferings
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest
           DescribeReservedElasticsearchInstanceOfferings
         where
        type Rs DescribeReservedElasticsearchInstanceOfferings =
             DescribeReservedElasticsearchInstanceOfferingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2015-01-01/es/reservedInstanceOfferings",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeReservedElasticsearchInstanceOfferingsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*>
                     x Core..:? "ReservedElasticsearchInstanceOfferings"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager
           DescribeReservedElasticsearchInstanceOfferings
         where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"reservedElasticsearchInstanceOfferings" Core..
                   Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Container for results from @DescribeReservedElasticsearchInstanceOfferings@ 
--
-- /See:/ 'mkDescribeReservedElasticsearchInstanceOfferingsResponse' smart constructor.
data DescribeReservedElasticsearchInstanceOfferingsResponse = DescribeReservedElasticsearchInstanceOfferingsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ Provides an identifier to allow retrieval of paginated results.
  , reservedElasticsearchInstanceOfferings :: Core.Maybe [Types.ReservedElasticsearchInstanceOffering]
    -- ^ List of reserved Elasticsearch instance offerings
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReservedElasticsearchInstanceOfferingsResponse' value with any optional fields omitted.
mkDescribeReservedElasticsearchInstanceOfferingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeReservedElasticsearchInstanceOfferingsResponse
mkDescribeReservedElasticsearchInstanceOfferingsResponse
  responseStatus
  = DescribeReservedElasticsearchInstanceOfferingsResponse'{nextToken
                                                              = Core.Nothing,
                                                            reservedElasticsearchInstanceOfferings =
                                                              Core.Nothing,
                                                            responseStatus}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreiorrsNextToken :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferingsResponse (Core.Maybe Types.NextToken)
dreiorrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dreiorrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | List of reserved Elasticsearch instance offerings
--
-- /Note:/ Consider using 'reservedElasticsearchInstanceOfferings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreiorrsReservedElasticsearchInstanceOfferings :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferingsResponse (Core.Maybe [Types.ReservedElasticsearchInstanceOffering])
dreiorrsReservedElasticsearchInstanceOfferings = Lens.field @"reservedElasticsearchInstanceOfferings"
{-# INLINEABLE dreiorrsReservedElasticsearchInstanceOfferings #-}
{-# DEPRECATED reservedElasticsearchInstanceOfferings "Use generic-lens or generic-optics with 'reservedElasticsearchInstanceOfferings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreiorrsResponseStatus :: Lens.Lens' DescribeReservedElasticsearchInstanceOfferingsResponse Core.Int
dreiorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dreiorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
