{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListDimensions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the set of dimensions that are defined for your AWS account.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListDimensions
    (
    -- * Creating a request
      ListDimensions (..)
    , mkListDimensions
    -- ** Request lenses
    , ldMaxResults
    , ldNextToken

    -- * Destructuring the response
    , ListDimensionsResponse (..)
    , mkListDimensionsResponse
    -- ** Response lenses
    , ldrrsDimensionNames
    , ldrrsNextToken
    , ldrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDimensions' smart constructor.
data ListDimensions = ListDimensions'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to retrieve at one time.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDimensions' value with any optional fields omitted.
mkListDimensions
    :: ListDimensions
mkListDimensions
  = ListDimensions'{maxResults = Core.Nothing,
                    nextToken = Core.Nothing}

-- | The maximum number of results to retrieve at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMaxResults :: Lens.Lens' ListDimensions (Core.Maybe Core.Natural)
ldMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ldMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldNextToken :: Lens.Lens' ListDimensions (Core.Maybe Types.NextToken)
ldNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListDimensions where
        toQuery ListDimensions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListDimensions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListDimensions where
        type Rs ListDimensions = ListDimensionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/dimensions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListDimensionsResponse' Core.<$>
                   (x Core..:? "dimensionNames") Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListDimensions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"dimensionNames" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListDimensionsResponse' smart constructor.
data ListDimensionsResponse = ListDimensionsResponse'
  { dimensionNames :: Core.Maybe [Types.DimensionName]
    -- ^ A list of the names of the defined dimensions. Use @DescribeDimension@ to get details for a dimension.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDimensionsResponse' value with any optional fields omitted.
mkListDimensionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListDimensionsResponse
mkListDimensionsResponse responseStatus
  = ListDimensionsResponse'{dimensionNames = Core.Nothing,
                            nextToken = Core.Nothing, responseStatus}

-- | A list of the names of the defined dimensions. Use @DescribeDimension@ to get details for a dimension.
--
-- /Note:/ Consider using 'dimensionNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsDimensionNames :: Lens.Lens' ListDimensionsResponse (Core.Maybe [Types.DimensionName])
ldrrsDimensionNames = Lens.field @"dimensionNames"
{-# INLINEABLE ldrrsDimensionNames #-}
{-# DEPRECATED dimensionNames "Use generic-lens or generic-optics with 'dimensionNames' instead"  #-}

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsNextToken :: Lens.Lens' ListDimensionsResponse (Core.Maybe Types.NextToken)
ldrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsResponseStatus :: Lens.Lens' ListDimensionsResponse Core.Int
ldrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ldrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
