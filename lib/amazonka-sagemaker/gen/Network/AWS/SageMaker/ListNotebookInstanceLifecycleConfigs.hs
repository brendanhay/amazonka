{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListNotebookInstanceLifecycleConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists notebook instance lifestyle configurations created with the 'CreateNotebookInstanceLifecycleConfig' API.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListNotebookInstanceLifecycleConfigs
    (
    -- * Creating a request
      ListNotebookInstanceLifecycleConfigs (..)
    , mkListNotebookInstanceLifecycleConfigs
    -- ** Request lenses
    , lnilcCreationTimeAfter
    , lnilcCreationTimeBefore
    , lnilcLastModifiedTimeAfter
    , lnilcLastModifiedTimeBefore
    , lnilcMaxResults
    , lnilcNameContains
    , lnilcNextToken
    , lnilcSortBy
    , lnilcSortOrder

    -- * Destructuring the response
    , ListNotebookInstanceLifecycleConfigsResponse (..)
    , mkListNotebookInstanceLifecycleConfigsResponse
    -- ** Response lenses
    , lnilcrrsNextToken
    , lnilcrrsNotebookInstanceLifecycleConfigs
    , lnilcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListNotebookInstanceLifecycleConfigs' smart constructor.
data ListNotebookInstanceLifecycleConfigs = ListNotebookInstanceLifecycleConfigs'
  { creationTimeAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only lifecycle configurations that were created after the specified time (timestamp).
  , creationTimeBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only lifecycle configurations that were created before the specified time (timestamp).
  , lastModifiedTimeAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only lifecycle configurations that were modified after the specified time (timestamp).
  , lastModifiedTimeBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only lifecycle configurations that were modified before the specified time (timestamp).
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of lifecycle configurations to return in the response.
  , nameContains :: Core.Maybe Types.NotebookInstanceLifecycleConfigNameContains
    -- ^ A string in the lifecycle configuration name. This filter returns only lifecycle configurations whose name contains the specified string.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the result of a @ListNotebookInstanceLifecycleConfigs@ request was truncated, the response includes a @NextToken@ . To get the next set of lifecycle configurations, use the token in the next request.
  , sortBy :: Core.Maybe Types.NotebookInstanceLifecycleConfigSortKey
    -- ^ Sorts the list of results. The default is @CreationTime@ .
  , sortOrder :: Core.Maybe Types.NotebookInstanceLifecycleConfigSortOrder
    -- ^ The sort order for results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListNotebookInstanceLifecycleConfigs' value with any optional fields omitted.
mkListNotebookInstanceLifecycleConfigs
    :: ListNotebookInstanceLifecycleConfigs
mkListNotebookInstanceLifecycleConfigs
  = ListNotebookInstanceLifecycleConfigs'{creationTimeAfter =
                                            Core.Nothing,
                                          creationTimeBefore = Core.Nothing,
                                          lastModifiedTimeAfter = Core.Nothing,
                                          lastModifiedTimeBefore = Core.Nothing,
                                          maxResults = Core.Nothing, nameContains = Core.Nothing,
                                          nextToken = Core.Nothing, sortBy = Core.Nothing,
                                          sortOrder = Core.Nothing}

-- | A filter that returns only lifecycle configurations that were created after the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcCreationTimeAfter :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Core.Maybe Core.NominalDiffTime)
lnilcCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# INLINEABLE lnilcCreationTimeAfter #-}
{-# DEPRECATED creationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead"  #-}

-- | A filter that returns only lifecycle configurations that were created before the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcCreationTimeBefore :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Core.Maybe Core.NominalDiffTime)
lnilcCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# INLINEABLE lnilcCreationTimeBefore #-}
{-# DEPRECATED creationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead"  #-}

-- | A filter that returns only lifecycle configurations that were modified after the specified time (timestamp).
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcLastModifiedTimeAfter :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Core.Maybe Core.NominalDiffTime)
lnilcLastModifiedTimeAfter = Lens.field @"lastModifiedTimeAfter"
{-# INLINEABLE lnilcLastModifiedTimeAfter #-}
{-# DEPRECATED lastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead"  #-}

-- | A filter that returns only lifecycle configurations that were modified before the specified time (timestamp).
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcLastModifiedTimeBefore :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Core.Maybe Core.NominalDiffTime)
lnilcLastModifiedTimeBefore = Lens.field @"lastModifiedTimeBefore"
{-# INLINEABLE lnilcLastModifiedTimeBefore #-}
{-# DEPRECATED lastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead"  #-}

-- | The maximum number of lifecycle configurations to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcMaxResults :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Core.Maybe Core.Natural)
lnilcMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lnilcMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A string in the lifecycle configuration name. This filter returns only lifecycle configurations whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcNameContains :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Core.Maybe Types.NotebookInstanceLifecycleConfigNameContains)
lnilcNameContains = Lens.field @"nameContains"
{-# INLINEABLE lnilcNameContains #-}
{-# DEPRECATED nameContains "Use generic-lens or generic-optics with 'nameContains' instead"  #-}

-- | If the result of a @ListNotebookInstanceLifecycleConfigs@ request was truncated, the response includes a @NextToken@ . To get the next set of lifecycle configurations, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcNextToken :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Core.Maybe Types.NextToken)
lnilcNextToken = Lens.field @"nextToken"
{-# INLINEABLE lnilcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Sorts the list of results. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcSortBy :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Core.Maybe Types.NotebookInstanceLifecycleConfigSortKey)
lnilcSortBy = Lens.field @"sortBy"
{-# INLINEABLE lnilcSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

-- | The sort order for results.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcSortOrder :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Core.Maybe Types.NotebookInstanceLifecycleConfigSortOrder)
lnilcSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE lnilcSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

instance Core.ToQuery ListNotebookInstanceLifecycleConfigs where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListNotebookInstanceLifecycleConfigs where
        toHeaders ListNotebookInstanceLifecycleConfigs{..}
          = Core.pure
              ("X-Amz-Target", "SageMaker.ListNotebookInstanceLifecycleConfigs")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListNotebookInstanceLifecycleConfigs where
        toJSON ListNotebookInstanceLifecycleConfigs{..}
          = Core.object
              (Core.catMaybes
                 [("CreationTimeAfter" Core..=) Core.<$> creationTimeAfter,
                  ("CreationTimeBefore" Core..=) Core.<$> creationTimeBefore,
                  ("LastModifiedTimeAfter" Core..=) Core.<$> lastModifiedTimeAfter,
                  ("LastModifiedTimeBefore" Core..=) Core.<$> lastModifiedTimeBefore,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NameContains" Core..=) Core.<$> nameContains,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SortBy" Core..=) Core.<$> sortBy,
                  ("SortOrder" Core..=) Core.<$> sortOrder])

instance Core.AWSRequest ListNotebookInstanceLifecycleConfigs where
        type Rs ListNotebookInstanceLifecycleConfigs =
             ListNotebookInstanceLifecycleConfigsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListNotebookInstanceLifecycleConfigsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*>
                     x Core..:? "NotebookInstanceLifecycleConfigs"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListNotebookInstanceLifecycleConfigs where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"notebookInstanceLifecycleConfigs" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListNotebookInstanceLifecycleConfigsResponse' smart constructor.
data ListNotebookInstanceLifecycleConfigsResponse = ListNotebookInstanceLifecycleConfigsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If the response is truncated, Amazon SageMaker returns this token. To get the next set of lifecycle configurations, use it in the next request. 
  , notebookInstanceLifecycleConfigs :: Core.Maybe [Types.NotebookInstanceLifecycleConfigSummary]
    -- ^ An array of @NotebookInstanceLifecycleConfiguration@ objects, each listing a lifecycle configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListNotebookInstanceLifecycleConfigsResponse' value with any optional fields omitted.
mkListNotebookInstanceLifecycleConfigsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListNotebookInstanceLifecycleConfigsResponse
mkListNotebookInstanceLifecycleConfigsResponse responseStatus
  = ListNotebookInstanceLifecycleConfigsResponse'{nextToken =
                                                    Core.Nothing,
                                                  notebookInstanceLifecycleConfigs = Core.Nothing,
                                                  responseStatus}

-- | If the response is truncated, Amazon SageMaker returns this token. To get the next set of lifecycle configurations, use it in the next request. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcrrsNextToken :: Lens.Lens' ListNotebookInstanceLifecycleConfigsResponse (Core.Maybe Types.NextToken)
lnilcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lnilcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | An array of @NotebookInstanceLifecycleConfiguration@ objects, each listing a lifecycle configuration.
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcrrsNotebookInstanceLifecycleConfigs :: Lens.Lens' ListNotebookInstanceLifecycleConfigsResponse (Core.Maybe [Types.NotebookInstanceLifecycleConfigSummary])
lnilcrrsNotebookInstanceLifecycleConfigs = Lens.field @"notebookInstanceLifecycleConfigs"
{-# INLINEABLE lnilcrrsNotebookInstanceLifecycleConfigs #-}
{-# DEPRECATED notebookInstanceLifecycleConfigs "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigs' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcrrsResponseStatus :: Lens.Lens' ListNotebookInstanceLifecycleConfigsResponse Core.Int
lnilcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lnilcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
