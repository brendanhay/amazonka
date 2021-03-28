{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeAggregationAuthorizations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of authorizations granted to various aggregator accounts and regions.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeAggregationAuthorizations
    (
    -- * Creating a request
      DescribeAggregationAuthorizations (..)
    , mkDescribeAggregationAuthorizations
    -- ** Request lenses
    , daaLimit
    , daaNextToken

    -- * Destructuring the response
    , DescribeAggregationAuthorizationsResponse (..)
    , mkDescribeAggregationAuthorizationsResponse
    -- ** Response lenses
    , daarrsAggregationAuthorizations
    , daarrsNextToken
    , daarrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAggregationAuthorizations' smart constructor.
data DescribeAggregationAuthorizations = DescribeAggregationAuthorizations'
  { limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of AggregationAuthorizations returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAggregationAuthorizations' value with any optional fields omitted.
mkDescribeAggregationAuthorizations
    :: DescribeAggregationAuthorizations
mkDescribeAggregationAuthorizations
  = DescribeAggregationAuthorizations'{limit = Core.Nothing,
                                       nextToken = Core.Nothing}

-- | The maximum number of AggregationAuthorizations returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaLimit :: Lens.Lens' DescribeAggregationAuthorizations (Core.Maybe Core.Natural)
daaLimit = Lens.field @"limit"
{-# INLINEABLE daaLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaNextToken :: Lens.Lens' DescribeAggregationAuthorizations (Core.Maybe Core.Text)
daaNextToken = Lens.field @"nextToken"
{-# INLINEABLE daaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeAggregationAuthorizations where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeAggregationAuthorizations where
        toHeaders DescribeAggregationAuthorizations{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.DescribeAggregationAuthorizations")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeAggregationAuthorizations where
        toJSON DescribeAggregationAuthorizations{..}
          = Core.object
              (Core.catMaybes
                 [("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeAggregationAuthorizations where
        type Rs DescribeAggregationAuthorizations =
             DescribeAggregationAuthorizationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeAggregationAuthorizationsResponse' Core.<$>
                   (x Core..:? "AggregationAuthorizations") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeAggregationAuthorizations where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"aggregationAuthorizations" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeAggregationAuthorizationsResponse' smart constructor.
data DescribeAggregationAuthorizationsResponse = DescribeAggregationAuthorizationsResponse'
  { aggregationAuthorizations :: Core.Maybe [Types.AggregationAuthorization]
    -- ^ Returns a list of authorizations granted to various aggregator accounts and regions.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeAggregationAuthorizationsResponse' value with any optional fields omitted.
mkDescribeAggregationAuthorizationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAggregationAuthorizationsResponse
mkDescribeAggregationAuthorizationsResponse responseStatus
  = DescribeAggregationAuthorizationsResponse'{aggregationAuthorizations
                                                 = Core.Nothing,
                                               nextToken = Core.Nothing, responseStatus}

-- | Returns a list of authorizations granted to various aggregator accounts and regions.
--
-- /Note:/ Consider using 'aggregationAuthorizations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarrsAggregationAuthorizations :: Lens.Lens' DescribeAggregationAuthorizationsResponse (Core.Maybe [Types.AggregationAuthorization])
daarrsAggregationAuthorizations = Lens.field @"aggregationAuthorizations"
{-# INLINEABLE daarrsAggregationAuthorizations #-}
{-# DEPRECATED aggregationAuthorizations "Use generic-lens or generic-optics with 'aggregationAuthorizations' instead"  #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarrsNextToken :: Lens.Lens' DescribeAggregationAuthorizationsResponse (Core.Maybe Core.Text)
daarrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE daarrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarrsResponseStatus :: Lens.Lens' DescribeAggregationAuthorizationsResponse Core.Int
daarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE daarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
