{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListModelPackages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the model packages that have been created.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListModelPackages
    (
    -- * Creating a request
      ListModelPackages (..)
    , mkListModelPackages
    -- ** Request lenses
    , lmpCreationTimeAfter
    , lmpCreationTimeBefore
    , lmpMaxResults
    , lmpNameContains
    , lmpNextToken
    , lmpSortBy
    , lmpSortOrder

    -- * Destructuring the response
    , ListModelPackagesResponse (..)
    , mkListModelPackagesResponse
    -- ** Response lenses
    , lmprrsModelPackageSummaryList
    , lmprrsNextToken
    , lmprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListModelPackages' smart constructor.
data ListModelPackages = ListModelPackages'
  { creationTimeAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only model packages created after the specified time (timestamp).
  , creationTimeBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only model packages created before the specified time (timestamp).
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of model packages to return in the response.
  , nameContains :: Core.Maybe Types.NameContains
    -- ^ A string in the model package name. This filter returns only model packages whose name contains the specified string.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the response to a previous @ListModelPackages@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of model packages, use the token in the next request.
  , sortBy :: Core.Maybe Types.ModelPackageSortBy
    -- ^ The parameter by which to sort the results. The default is @CreationTime@ .
  , sortOrder :: Core.Maybe Types.SortOrder
    -- ^ The sort order for the results. The default is @Ascending@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListModelPackages' value with any optional fields omitted.
mkListModelPackages
    :: ListModelPackages
mkListModelPackages
  = ListModelPackages'{creationTimeAfter = Core.Nothing,
                       creationTimeBefore = Core.Nothing, maxResults = Core.Nothing,
                       nameContains = Core.Nothing, nextToken = Core.Nothing,
                       sortBy = Core.Nothing, sortOrder = Core.Nothing}

-- | A filter that returns only model packages created after the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpCreationTimeAfter :: Lens.Lens' ListModelPackages (Core.Maybe Core.NominalDiffTime)
lmpCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# INLINEABLE lmpCreationTimeAfter #-}
{-# DEPRECATED creationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead"  #-}

-- | A filter that returns only model packages created before the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpCreationTimeBefore :: Lens.Lens' ListModelPackages (Core.Maybe Core.NominalDiffTime)
lmpCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# INLINEABLE lmpCreationTimeBefore #-}
{-# DEPRECATED creationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead"  #-}

-- | The maximum number of model packages to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpMaxResults :: Lens.Lens' ListModelPackages (Core.Maybe Core.Natural)
lmpMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lmpMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A string in the model package name. This filter returns only model packages whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpNameContains :: Lens.Lens' ListModelPackages (Core.Maybe Types.NameContains)
lmpNameContains = Lens.field @"nameContains"
{-# INLINEABLE lmpNameContains #-}
{-# DEPRECATED nameContains "Use generic-lens or generic-optics with 'nameContains' instead"  #-}

-- | If the response to a previous @ListModelPackages@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of model packages, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpNextToken :: Lens.Lens' ListModelPackages (Core.Maybe Types.NextToken)
lmpNextToken = Lens.field @"nextToken"
{-# INLINEABLE lmpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The parameter by which to sort the results. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpSortBy :: Lens.Lens' ListModelPackages (Core.Maybe Types.ModelPackageSortBy)
lmpSortBy = Lens.field @"sortBy"
{-# INLINEABLE lmpSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

-- | The sort order for the results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpSortOrder :: Lens.Lens' ListModelPackages (Core.Maybe Types.SortOrder)
lmpSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE lmpSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

instance Core.ToQuery ListModelPackages where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListModelPackages where
        toHeaders ListModelPackages{..}
          = Core.pure ("X-Amz-Target", "SageMaker.ListModelPackages") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListModelPackages where
        toJSON ListModelPackages{..}
          = Core.object
              (Core.catMaybes
                 [("CreationTimeAfter" Core..=) Core.<$> creationTimeAfter,
                  ("CreationTimeBefore" Core..=) Core.<$> creationTimeBefore,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NameContains" Core..=) Core.<$> nameContains,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SortBy" Core..=) Core.<$> sortBy,
                  ("SortOrder" Core..=) Core.<$> sortOrder])

instance Core.AWSRequest ListModelPackages where
        type Rs ListModelPackages = ListModelPackagesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListModelPackagesResponse' Core.<$>
                   (x Core..:? "ModelPackageSummaryList" Core..!= Core.mempty)
                     Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListModelPackages where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"modelPackageSummaryList") =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListModelPackagesResponse' smart constructor.
data ListModelPackagesResponse = ListModelPackagesResponse'
  { modelPackageSummaryList :: [Types.ModelPackageSummary]
    -- ^ An array of @ModelPackageSummary@ objects, each of which lists a model package.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of model packages, use it in the subsequent request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListModelPackagesResponse' value with any optional fields omitted.
mkListModelPackagesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListModelPackagesResponse
mkListModelPackagesResponse responseStatus
  = ListModelPackagesResponse'{modelPackageSummaryList = Core.mempty,
                               nextToken = Core.Nothing, responseStatus}

-- | An array of @ModelPackageSummary@ objects, each of which lists a model package.
--
-- /Note:/ Consider using 'modelPackageSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmprrsModelPackageSummaryList :: Lens.Lens' ListModelPackagesResponse [Types.ModelPackageSummary]
lmprrsModelPackageSummaryList = Lens.field @"modelPackageSummaryList"
{-# INLINEABLE lmprrsModelPackageSummaryList #-}
{-# DEPRECATED modelPackageSummaryList "Use generic-lens or generic-optics with 'modelPackageSummaryList' instead"  #-}

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of model packages, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmprrsNextToken :: Lens.Lens' ListModelPackagesResponse (Core.Maybe Types.NextToken)
lmprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lmprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmprrsResponseStatus :: Lens.Lens' ListModelPackagesResponse Core.Int
lmprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lmprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
