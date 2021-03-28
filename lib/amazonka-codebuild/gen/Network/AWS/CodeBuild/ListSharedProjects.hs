{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.ListSharedProjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of projects that are shared with other AWS accounts or users. 
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListSharedProjects
    (
    -- * Creating a request
      ListSharedProjects (..)
    , mkListSharedProjects
    -- ** Request lenses
    , lspMaxResults
    , lspNextToken
    , lspSortBy
    , lspSortOrder

    -- * Destructuring the response
    , ListSharedProjectsResponse (..)
    , mkListSharedProjectsResponse
    -- ** Response lenses
    , lsprrsNextToken
    , lsprrsProjects
    , lsprrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListSharedProjects' smart constructor.
data ListSharedProjects = ListSharedProjects'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of paginated shared build projects returned per response. Use @nextToken@ to iterate pages in the list of returned @Project@ objects. The default value is 100. 
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned. 
  , sortBy :: Core.Maybe Types.SharedResourceSortByType
    -- ^ The criterion to be used to list build projects shared with the current AWS account or user. Valid values include: 
--
--
--     * @ARN@ : List based on the ARN. 
--
--
--     * @MODIFIED_TIME@ : List based on when information about the shared project was last changed. 
--
--
  , sortOrder :: Core.Maybe Types.SortOrderType
    -- ^ The order in which to list shared build projects. Valid values include:
--
--
--     * @ASCENDING@ : List in ascending order.
--
--
--     * @DESCENDING@ : List in descending order.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSharedProjects' value with any optional fields omitted.
mkListSharedProjects
    :: ListSharedProjects
mkListSharedProjects
  = ListSharedProjects'{maxResults = Core.Nothing,
                        nextToken = Core.Nothing, sortBy = Core.Nothing,
                        sortOrder = Core.Nothing}

-- | The maximum number of paginated shared build projects returned per response. Use @nextToken@ to iterate pages in the list of returned @Project@ objects. The default value is 100. 
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspMaxResults :: Lens.Lens' ListSharedProjects (Core.Maybe Core.Natural)
lspMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lspMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspNextToken :: Lens.Lens' ListSharedProjects (Core.Maybe Types.NextToken)
lspNextToken = Lens.field @"nextToken"
{-# INLINEABLE lspNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The criterion to be used to list build projects shared with the current AWS account or user. Valid values include: 
--
--
--     * @ARN@ : List based on the ARN. 
--
--
--     * @MODIFIED_TIME@ : List based on when information about the shared project was last changed. 
--
--
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspSortBy :: Lens.Lens' ListSharedProjects (Core.Maybe Types.SharedResourceSortByType)
lspSortBy = Lens.field @"sortBy"
{-# INLINEABLE lspSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

-- | The order in which to list shared build projects. Valid values include:
--
--
--     * @ASCENDING@ : List in ascending order.
--
--
--     * @DESCENDING@ : List in descending order.
--
--
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lspSortOrder :: Lens.Lens' ListSharedProjects (Core.Maybe Types.SortOrderType)
lspSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE lspSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

instance Core.ToQuery ListSharedProjects where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListSharedProjects where
        toHeaders ListSharedProjects{..}
          = Core.pure
              ("X-Amz-Target", "CodeBuild_20161006.ListSharedProjects")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListSharedProjects where
        toJSON ListSharedProjects{..}
          = Core.object
              (Core.catMaybes
                 [("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("sortBy" Core..=) Core.<$> sortBy,
                  ("sortOrder" Core..=) Core.<$> sortOrder])

instance Core.AWSRequest ListSharedProjects where
        type Rs ListSharedProjects = ListSharedProjectsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListSharedProjectsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "projects" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListSharedProjects where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"projects" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListSharedProjectsResponse' smart constructor.
data ListSharedProjectsResponse = ListSharedProjectsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned. 
  , projects :: Core.Maybe (Core.NonEmpty Types.NonEmptyString)
    -- ^ The list of ARNs for the build projects shared with the current AWS account or user. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSharedProjectsResponse' value with any optional fields omitted.
mkListSharedProjectsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListSharedProjectsResponse
mkListSharedProjectsResponse responseStatus
  = ListSharedProjectsResponse'{nextToken = Core.Nothing,
                                projects = Core.Nothing, responseStatus}

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsprrsNextToken :: Lens.Lens' ListSharedProjectsResponse (Core.Maybe Core.Text)
lsprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The list of ARNs for the build projects shared with the current AWS account or user. 
--
-- /Note:/ Consider using 'projects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsprrsProjects :: Lens.Lens' ListSharedProjectsResponse (Core.Maybe (Core.NonEmpty Types.NonEmptyString))
lsprrsProjects = Lens.field @"projects"
{-# INLINEABLE lsprrsProjects #-}
{-# DEPRECATED projects "Use generic-lens or generic-optics with 'projects' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsprrsResponseStatus :: Lens.Lens' ListSharedProjectsResponse Core.Int
lsprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
