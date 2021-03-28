{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ListMLTransforms
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a sortable, filterable list of existing AWS Glue machine learning transforms in this AWS account, or the resources with the specified tag. This operation takes the optional @Tags@ field, which you can use as a filter of the responses so that tagged resources can be retrieved as a group. If you choose to use tag filtering, only resources with the tags are retrieved. 
module Network.AWS.Glue.ListMLTransforms
    (
    -- * Creating a request
      ListMLTransforms (..)
    , mkListMLTransforms
    -- ** Request lenses
    , lmltFilter
    , lmltMaxResults
    , lmltNextToken
    , lmltSort
    , lmltTags

    -- * Destructuring the response
    , ListMLTransformsResponse (..)
    , mkListMLTransformsResponse
    -- ** Response lenses
    , lmltrrsTransformIds
    , lmltrrsNextToken
    , lmltrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListMLTransforms' smart constructor.
data ListMLTransforms = ListMLTransforms'
  { filter :: Core.Maybe Types.TransformFilterCriteria
    -- ^ A @TransformFilterCriteria@ used to filter the machine learning transforms.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum size of a list to return.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ A continuation token, if this is a continuation request.
  , sort :: Core.Maybe Types.TransformSortCriteria
    -- ^ A @TransformSortCriteria@ used to sort the machine learning transforms.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ Specifies to return only these tagged resources.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListMLTransforms' value with any optional fields omitted.
mkListMLTransforms
    :: ListMLTransforms
mkListMLTransforms
  = ListMLTransforms'{filter = Core.Nothing,
                      maxResults = Core.Nothing, nextToken = Core.Nothing,
                      sort = Core.Nothing, tags = Core.Nothing}

-- | A @TransformFilterCriteria@ used to filter the machine learning transforms.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmltFilter :: Lens.Lens' ListMLTransforms (Core.Maybe Types.TransformFilterCriteria)
lmltFilter = Lens.field @"filter"
{-# INLINEABLE lmltFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The maximum size of a list to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmltMaxResults :: Lens.Lens' ListMLTransforms (Core.Maybe Core.Natural)
lmltMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lmltMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A continuation token, if this is a continuation request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmltNextToken :: Lens.Lens' ListMLTransforms (Core.Maybe Types.PaginationToken)
lmltNextToken = Lens.field @"nextToken"
{-# INLINEABLE lmltNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A @TransformSortCriteria@ used to sort the machine learning transforms.
--
-- /Note:/ Consider using 'sort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmltSort :: Lens.Lens' ListMLTransforms (Core.Maybe Types.TransformSortCriteria)
lmltSort = Lens.field @"sort"
{-# INLINEABLE lmltSort #-}
{-# DEPRECATED sort "Use generic-lens or generic-optics with 'sort' instead"  #-}

-- | Specifies to return only these tagged resources.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmltTags :: Lens.Lens' ListMLTransforms (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
lmltTags = Lens.field @"tags"
{-# INLINEABLE lmltTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery ListMLTransforms where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListMLTransforms where
        toHeaders ListMLTransforms{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.ListMLTransforms") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListMLTransforms where
        toJSON ListMLTransforms{..}
          = Core.object
              (Core.catMaybes
                 [("Filter" Core..=) Core.<$> filter,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("Sort" Core..=) Core.<$> sort, ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest ListMLTransforms where
        type Rs ListMLTransforms = ListMLTransformsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListMLTransformsResponse' Core.<$>
                   (x Core..:? "TransformIds" Core..!= Core.mempty) Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListMLTransformsResponse' smart constructor.
data ListMLTransformsResponse = ListMLTransformsResponse'
  { transformIds :: [Types.HashString]
    -- ^ The identifiers of all the machine learning transforms in the account, or the machine learning transforms with the specified tags.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ A continuation token, if the returned list does not contain the last metric available.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListMLTransformsResponse' value with any optional fields omitted.
mkListMLTransformsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListMLTransformsResponse
mkListMLTransformsResponse responseStatus
  = ListMLTransformsResponse'{transformIds = Core.mempty,
                              nextToken = Core.Nothing, responseStatus}

-- | The identifiers of all the machine learning transforms in the account, or the machine learning transforms with the specified tags.
--
-- /Note:/ Consider using 'transformIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmltrrsTransformIds :: Lens.Lens' ListMLTransformsResponse [Types.HashString]
lmltrrsTransformIds = Lens.field @"transformIds"
{-# INLINEABLE lmltrrsTransformIds #-}
{-# DEPRECATED transformIds "Use generic-lens or generic-optics with 'transformIds' instead"  #-}

-- | A continuation token, if the returned list does not contain the last metric available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmltrrsNextToken :: Lens.Lens' ListMLTransformsResponse (Core.Maybe Types.PaginationToken)
lmltrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lmltrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmltrrsResponseStatus :: Lens.Lens' ListMLTransformsResponse Core.Int
lmltrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lmltrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
