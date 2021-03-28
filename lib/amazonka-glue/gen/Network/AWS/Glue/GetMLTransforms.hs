{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetMLTransforms
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a sortable, filterable list of existing AWS Glue machine learning transforms. Machine learning transforms are a special type of transform that use machine learning to learn the details of the transformation to be performed by learning from examples provided by humans. These transformations are then saved by AWS Glue, and you can retrieve their metadata by calling @GetMLTransforms@ .
module Network.AWS.Glue.GetMLTransforms
    (
    -- * Creating a request
      GetMLTransforms (..)
    , mkGetMLTransforms
    -- ** Request lenses
    , gmltFilter
    , gmltMaxResults
    , gmltNextToken
    , gmltSort

    -- * Destructuring the response
    , GetMLTransformsResponse (..)
    , mkGetMLTransformsResponse
    -- ** Response lenses
    , gmltrfrsTransforms
    , gmltrfrsNextToken
    , gmltrfrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetMLTransforms' smart constructor.
data GetMLTransforms = GetMLTransforms'
  { filter :: Core.Maybe Types.TransformFilterCriteria
    -- ^ The filter transformation criteria.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ A paginated token to offset the results.
  , sort :: Core.Maybe Types.TransformSortCriteria
    -- ^ The sorting criteria.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetMLTransforms' value with any optional fields omitted.
mkGetMLTransforms
    :: GetMLTransforms
mkGetMLTransforms
  = GetMLTransforms'{filter = Core.Nothing,
                     maxResults = Core.Nothing, nextToken = Core.Nothing,
                     sort = Core.Nothing}

-- | The filter transformation criteria.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltFilter :: Lens.Lens' GetMLTransforms (Core.Maybe Types.TransformFilterCriteria)
gmltFilter = Lens.field @"filter"
{-# INLINEABLE gmltFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltMaxResults :: Lens.Lens' GetMLTransforms (Core.Maybe Core.Natural)
gmltMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gmltMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A paginated token to offset the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltNextToken :: Lens.Lens' GetMLTransforms (Core.Maybe Types.PaginationToken)
gmltNextToken = Lens.field @"nextToken"
{-# INLINEABLE gmltNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The sorting criteria.
--
-- /Note:/ Consider using 'sort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltSort :: Lens.Lens' GetMLTransforms (Core.Maybe Types.TransformSortCriteria)
gmltSort = Lens.field @"sort"
{-# INLINEABLE gmltSort #-}
{-# DEPRECATED sort "Use generic-lens or generic-optics with 'sort' instead"  #-}

instance Core.ToQuery GetMLTransforms where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetMLTransforms where
        toHeaders GetMLTransforms{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetMLTransforms") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetMLTransforms where
        toJSON GetMLTransforms{..}
          = Core.object
              (Core.catMaybes
                 [("Filter" Core..=) Core.<$> filter,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("Sort" Core..=) Core.<$> sort])

instance Core.AWSRequest GetMLTransforms where
        type Rs GetMLTransforms = GetMLTransformsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetMLTransformsResponse' Core.<$>
                   (x Core..:? "Transforms" Core..!= Core.mempty) Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetMLTransformsResponse' smart constructor.
data GetMLTransformsResponse = GetMLTransformsResponse'
  { transforms :: [Types.MLTransform]
    -- ^ A list of machine learning transforms.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ A pagination token, if more results are available.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetMLTransformsResponse' value with any optional fields omitted.
mkGetMLTransformsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetMLTransformsResponse
mkGetMLTransformsResponse responseStatus
  = GetMLTransformsResponse'{transforms = Core.mempty,
                             nextToken = Core.Nothing, responseStatus}

-- | A list of machine learning transforms.
--
-- /Note:/ Consider using 'transforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrfrsTransforms :: Lens.Lens' GetMLTransformsResponse [Types.MLTransform]
gmltrfrsTransforms = Lens.field @"transforms"
{-# INLINEABLE gmltrfrsTransforms #-}
{-# DEPRECATED transforms "Use generic-lens or generic-optics with 'transforms' instead"  #-}

-- | A pagination token, if more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrfrsNextToken :: Lens.Lens' GetMLTransformsResponse (Core.Maybe Types.PaginationToken)
gmltrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gmltrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrfrsResponseStatus :: Lens.Lens' GetMLTransformsResponse Core.Int
gmltrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gmltrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
