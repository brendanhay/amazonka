{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListNotebookInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the Amazon SageMaker notebook instances in the requester's account in an AWS Region. 
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListNotebookInstances
    (
    -- * Creating a request
      ListNotebookInstances (..)
    , mkListNotebookInstances
    -- ** Request lenses
    , lniAdditionalCodeRepositoryEquals
    , lniCreationTimeAfter
    , lniCreationTimeBefore
    , lniDefaultCodeRepositoryContains
    , lniLastModifiedTimeAfter
    , lniLastModifiedTimeBefore
    , lniMaxResults
    , lniNameContains
    , lniNextToken
    , lniNotebookInstanceLifecycleConfigNameContains
    , lniSortBy
    , lniSortOrder
    , lniStatusEquals

    -- * Destructuring the response
    , ListNotebookInstancesResponse (..)
    , mkListNotebookInstancesResponse
    -- ** Response lenses
    , lnirrsNextToken
    , lnirrsNotebookInstances
    , lnirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListNotebookInstances' smart constructor.
data ListNotebookInstances = ListNotebookInstances'
  { additionalCodeRepositoryEquals :: Core.Maybe Types.AdditionalCodeRepositoryEquals
    -- ^ A filter that returns only notebook instances with associated with the specified git repository.
  , creationTimeAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only notebook instances that were created after the specified time (timestamp).
  , creationTimeBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only notebook instances that were created before the specified time (timestamp). 
  , defaultCodeRepositoryContains :: Core.Maybe Types.CodeRepositoryContains
    -- ^ A string in the name or URL of a Git repository associated with this notebook instance. This filter returns only notebook instances associated with a git repository with a name that contains the specified string.
  , lastModifiedTimeAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only notebook instances that were modified after the specified time (timestamp).
  , lastModifiedTimeBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only notebook instances that were modified before the specified time (timestamp).
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of notebook instances to return.
  , nameContains :: Core.Maybe Types.NameContains
    -- ^ A string in the notebook instances' name. This filter returns only notebook instances whose name contains the specified string.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the previous call to the @ListNotebookInstances@ is truncated, the response includes a @NextToken@ . You can use this token in your subsequent @ListNotebookInstances@ request to fetch the next set of notebook instances. 
  , notebookInstanceLifecycleConfigNameContains :: Core.Maybe Types.NotebookInstanceLifecycleConfigName
    -- ^ A string in the name of a notebook instances lifecycle configuration associated with this notebook instance. This filter returns only notebook instances associated with a lifecycle configuration with a name that contains the specified string.
  , sortBy :: Core.Maybe Types.NotebookInstanceSortKey
    -- ^ The field to sort results by. The default is @Name@ .
  , sortOrder :: Core.Maybe Types.NotebookInstanceSortOrder
    -- ^ The sort order for results. 
  , statusEquals :: Core.Maybe Types.NotebookInstanceStatus
    -- ^ A filter that returns only notebook instances with the specified status.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListNotebookInstances' value with any optional fields omitted.
mkListNotebookInstances
    :: ListNotebookInstances
mkListNotebookInstances
  = ListNotebookInstances'{additionalCodeRepositoryEquals =
                             Core.Nothing,
                           creationTimeAfter = Core.Nothing,
                           creationTimeBefore = Core.Nothing,
                           defaultCodeRepositoryContains = Core.Nothing,
                           lastModifiedTimeAfter = Core.Nothing,
                           lastModifiedTimeBefore = Core.Nothing, maxResults = Core.Nothing,
                           nameContains = Core.Nothing, nextToken = Core.Nothing,
                           notebookInstanceLifecycleConfigNameContains = Core.Nothing,
                           sortBy = Core.Nothing, sortOrder = Core.Nothing,
                           statusEquals = Core.Nothing}

-- | A filter that returns only notebook instances with associated with the specified git repository.
--
-- /Note:/ Consider using 'additionalCodeRepositoryEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniAdditionalCodeRepositoryEquals :: Lens.Lens' ListNotebookInstances (Core.Maybe Types.AdditionalCodeRepositoryEquals)
lniAdditionalCodeRepositoryEquals = Lens.field @"additionalCodeRepositoryEquals"
{-# INLINEABLE lniAdditionalCodeRepositoryEquals #-}
{-# DEPRECATED additionalCodeRepositoryEquals "Use generic-lens or generic-optics with 'additionalCodeRepositoryEquals' instead"  #-}

-- | A filter that returns only notebook instances that were created after the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniCreationTimeAfter :: Lens.Lens' ListNotebookInstances (Core.Maybe Core.NominalDiffTime)
lniCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# INLINEABLE lniCreationTimeAfter #-}
{-# DEPRECATED creationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead"  #-}

-- | A filter that returns only notebook instances that were created before the specified time (timestamp). 
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniCreationTimeBefore :: Lens.Lens' ListNotebookInstances (Core.Maybe Core.NominalDiffTime)
lniCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# INLINEABLE lniCreationTimeBefore #-}
{-# DEPRECATED creationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead"  #-}

-- | A string in the name or URL of a Git repository associated with this notebook instance. This filter returns only notebook instances associated with a git repository with a name that contains the specified string.
--
-- /Note:/ Consider using 'defaultCodeRepositoryContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniDefaultCodeRepositoryContains :: Lens.Lens' ListNotebookInstances (Core.Maybe Types.CodeRepositoryContains)
lniDefaultCodeRepositoryContains = Lens.field @"defaultCodeRepositoryContains"
{-# INLINEABLE lniDefaultCodeRepositoryContains #-}
{-# DEPRECATED defaultCodeRepositoryContains "Use generic-lens or generic-optics with 'defaultCodeRepositoryContains' instead"  #-}

-- | A filter that returns only notebook instances that were modified after the specified time (timestamp).
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniLastModifiedTimeAfter :: Lens.Lens' ListNotebookInstances (Core.Maybe Core.NominalDiffTime)
lniLastModifiedTimeAfter = Lens.field @"lastModifiedTimeAfter"
{-# INLINEABLE lniLastModifiedTimeAfter #-}
{-# DEPRECATED lastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead"  #-}

-- | A filter that returns only notebook instances that were modified before the specified time (timestamp).
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniLastModifiedTimeBefore :: Lens.Lens' ListNotebookInstances (Core.Maybe Core.NominalDiffTime)
lniLastModifiedTimeBefore = Lens.field @"lastModifiedTimeBefore"
{-# INLINEABLE lniLastModifiedTimeBefore #-}
{-# DEPRECATED lastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead"  #-}

-- | The maximum number of notebook instances to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniMaxResults :: Lens.Lens' ListNotebookInstances (Core.Maybe Core.Natural)
lniMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lniMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A string in the notebook instances' name. This filter returns only notebook instances whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniNameContains :: Lens.Lens' ListNotebookInstances (Core.Maybe Types.NameContains)
lniNameContains = Lens.field @"nameContains"
{-# INLINEABLE lniNameContains #-}
{-# DEPRECATED nameContains "Use generic-lens or generic-optics with 'nameContains' instead"  #-}

-- | If the previous call to the @ListNotebookInstances@ is truncated, the response includes a @NextToken@ . You can use this token in your subsequent @ListNotebookInstances@ request to fetch the next set of notebook instances. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniNextToken :: Lens.Lens' ListNotebookInstances (Core.Maybe Types.NextToken)
lniNextToken = Lens.field @"nextToken"
{-# INLINEABLE lniNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A string in the name of a notebook instances lifecycle configuration associated with this notebook instance. This filter returns only notebook instances associated with a lifecycle configuration with a name that contains the specified string.
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigNameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniNotebookInstanceLifecycleConfigNameContains :: Lens.Lens' ListNotebookInstances (Core.Maybe Types.NotebookInstanceLifecycleConfigName)
lniNotebookInstanceLifecycleConfigNameContains = Lens.field @"notebookInstanceLifecycleConfigNameContains"
{-# INLINEABLE lniNotebookInstanceLifecycleConfigNameContains #-}
{-# DEPRECATED notebookInstanceLifecycleConfigNameContains "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigNameContains' instead"  #-}

-- | The field to sort results by. The default is @Name@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniSortBy :: Lens.Lens' ListNotebookInstances (Core.Maybe Types.NotebookInstanceSortKey)
lniSortBy = Lens.field @"sortBy"
{-# INLINEABLE lniSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

-- | The sort order for results. 
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniSortOrder :: Lens.Lens' ListNotebookInstances (Core.Maybe Types.NotebookInstanceSortOrder)
lniSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE lniSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

-- | A filter that returns only notebook instances with the specified status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniStatusEquals :: Lens.Lens' ListNotebookInstances (Core.Maybe Types.NotebookInstanceStatus)
lniStatusEquals = Lens.field @"statusEquals"
{-# INLINEABLE lniStatusEquals #-}
{-# DEPRECATED statusEquals "Use generic-lens or generic-optics with 'statusEquals' instead"  #-}

instance Core.ToQuery ListNotebookInstances where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListNotebookInstances where
        toHeaders ListNotebookInstances{..}
          = Core.pure ("X-Amz-Target", "SageMaker.ListNotebookInstances")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListNotebookInstances where
        toJSON ListNotebookInstances{..}
          = Core.object
              (Core.catMaybes
                 [("AdditionalCodeRepositoryEquals" Core..=) Core.<$>
                    additionalCodeRepositoryEquals,
                  ("CreationTimeAfter" Core..=) Core.<$> creationTimeAfter,
                  ("CreationTimeBefore" Core..=) Core.<$> creationTimeBefore,
                  ("DefaultCodeRepositoryContains" Core..=) Core.<$>
                    defaultCodeRepositoryContains,
                  ("LastModifiedTimeAfter" Core..=) Core.<$> lastModifiedTimeAfter,
                  ("LastModifiedTimeBefore" Core..=) Core.<$> lastModifiedTimeBefore,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NameContains" Core..=) Core.<$> nameContains,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("NotebookInstanceLifecycleConfigNameContains" Core..=) Core.<$>
                    notebookInstanceLifecycleConfigNameContains,
                  ("SortBy" Core..=) Core.<$> sortBy,
                  ("SortOrder" Core..=) Core.<$> sortOrder,
                  ("StatusEquals" Core..=) Core.<$> statusEquals])

instance Core.AWSRequest ListNotebookInstances where
        type Rs ListNotebookInstances = ListNotebookInstancesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListNotebookInstancesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "NotebookInstances"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListNotebookInstances where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"notebookInstances" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListNotebookInstancesResponse' smart constructor.
data ListNotebookInstancesResponse = ListNotebookInstancesResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If the response to the previous @ListNotebookInstances@ request was truncated, Amazon SageMaker returns this token. To retrieve the next set of notebook instances, use the token in the next request.
  , notebookInstances :: Core.Maybe [Types.NotebookInstanceSummary]
    -- ^ An array of @NotebookInstanceSummary@ objects, one for each notebook instance.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListNotebookInstancesResponse' value with any optional fields omitted.
mkListNotebookInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListNotebookInstancesResponse
mkListNotebookInstancesResponse responseStatus
  = ListNotebookInstancesResponse'{nextToken = Core.Nothing,
                                   notebookInstances = Core.Nothing, responseStatus}

-- | If the response to the previous @ListNotebookInstances@ request was truncated, Amazon SageMaker returns this token. To retrieve the next set of notebook instances, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnirrsNextToken :: Lens.Lens' ListNotebookInstancesResponse (Core.Maybe Types.NextToken)
lnirrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lnirrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | An array of @NotebookInstanceSummary@ objects, one for each notebook instance.
--
-- /Note:/ Consider using 'notebookInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnirrsNotebookInstances :: Lens.Lens' ListNotebookInstancesResponse (Core.Maybe [Types.NotebookInstanceSummary])
lnirrsNotebookInstances = Lens.field @"notebookInstances"
{-# INLINEABLE lnirrsNotebookInstances #-}
{-# DEPRECATED notebookInstances "Use generic-lens or generic-optics with 'notebookInstances' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnirrsResponseStatus :: Lens.Lens' ListNotebookInstancesResponse Core.Int
lnirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lnirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
