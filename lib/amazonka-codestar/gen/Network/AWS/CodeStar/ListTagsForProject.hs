{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.ListTagsForProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the tags for a project.
module Network.AWS.CodeStar.ListTagsForProject
    (
    -- * Creating a request
      ListTagsForProject (..)
    , mkListTagsForProject
    -- ** Request lenses
    , ltfpId
    , ltfpMaxResults
    , ltfpNextToken

    -- * Destructuring the response
    , ListTagsForProjectResponse (..)
    , mkListTagsForProjectResponse
    -- ** Response lenses
    , ltfprrsNextToken
    , ltfprrsTags
    , ltfprrsResponseStatus
    ) where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTagsForProject' smart constructor.
data ListTagsForProject = ListTagsForProject'
  { id :: Types.Id
    -- ^ The ID of the project to get tags for.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ Reserved for future use.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ Reserved for future use.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForProject' value with any optional fields omitted.
mkListTagsForProject
    :: Types.Id -- ^ 'id'
    -> ListTagsForProject
mkListTagsForProject id
  = ListTagsForProject'{id, maxResults = Core.Nothing,
                        nextToken = Core.Nothing}

-- | The ID of the project to get tags for.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfpId :: Lens.Lens' ListTagsForProject Types.Id
ltfpId = Lens.field @"id"
{-# INLINEABLE ltfpId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfpMaxResults :: Lens.Lens' ListTagsForProject (Core.Maybe Core.Natural)
ltfpMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ltfpMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfpNextToken :: Lens.Lens' ListTagsForProject (Core.Maybe Types.PaginationToken)
ltfpNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltfpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListTagsForProject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTagsForProject where
        toHeaders ListTagsForProject{..}
          = Core.pure
              ("X-Amz-Target", "CodeStar_20170419.ListTagsForProject")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTagsForProject where
        toJSON ListTagsForProject{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("id" Core..= id),
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListTagsForProject where
        type Rs ListTagsForProject = ListTagsForProjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTagsForProjectResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "tags" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListTagsForProjectResponse' smart constructor.
data ListTagsForProjectResponse = ListTagsForProjectResponse'
  { nextToken :: Core.Maybe Types.PaginationToken
    -- ^ Reserved for future use.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ The tags for the project.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForProjectResponse' value with any optional fields omitted.
mkListTagsForProjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTagsForProjectResponse
mkListTagsForProjectResponse responseStatus
  = ListTagsForProjectResponse'{nextToken = Core.Nothing,
                                tags = Core.Nothing, responseStatus}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfprrsNextToken :: Lens.Lens' ListTagsForProjectResponse (Core.Maybe Types.PaginationToken)
ltfprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltfprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The tags for the project.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfprrsTags :: Lens.Lens' ListTagsForProjectResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
ltfprrsTags = Lens.field @"tags"
{-# INLINEABLE ltfprrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfprrsResponseStatus :: Lens.Lens' ListTagsForProjectResponse Core.Int
ltfprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltfprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
