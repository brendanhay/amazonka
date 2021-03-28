{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListApps
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists apps.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListApps
    (
    -- * Creating a request
      ListApps (..)
    , mkListApps
    -- ** Request lenses
    , laDomainIdEquals
    , laMaxResults
    , laNextToken
    , laSortBy
    , laSortOrder
    , laUserProfileNameEquals

    -- * Destructuring the response
    , ListAppsResponse (..)
    , mkListAppsResponse
    -- ** Response lenses
    , larrsApps
    , larrsNextToken
    , larrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListApps' smart constructor.
data ListApps = ListApps'
  { domainIdEquals :: Core.Maybe Types.DomainId
    -- ^ A parameter to search for the domain ID.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ Returns a list up to a specified limit.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
  , sortBy :: Core.Maybe Types.AppSortKey
    -- ^ The parameter by which to sort the results. The default is CreationTime.
  , sortOrder :: Core.Maybe Types.SortOrder
    -- ^ The sort order for the results. The default is Ascending.
  , userProfileNameEquals :: Core.Maybe Types.UserProfileName
    -- ^ A parameter to search by user profile name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListApps' value with any optional fields omitted.
mkListApps
    :: ListApps
mkListApps
  = ListApps'{domainIdEquals = Core.Nothing,
              maxResults = Core.Nothing, nextToken = Core.Nothing,
              sortBy = Core.Nothing, sortOrder = Core.Nothing,
              userProfileNameEquals = Core.Nothing}

-- | A parameter to search for the domain ID.
--
-- /Note:/ Consider using 'domainIdEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laDomainIdEquals :: Lens.Lens' ListApps (Core.Maybe Types.DomainId)
laDomainIdEquals = Lens.field @"domainIdEquals"
{-# INLINEABLE laDomainIdEquals #-}
{-# DEPRECATED domainIdEquals "Use generic-lens or generic-optics with 'domainIdEquals' instead"  #-}

-- | Returns a list up to a specified limit.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMaxResults :: Lens.Lens' ListApps (Core.Maybe Core.Natural)
laMaxResults = Lens.field @"maxResults"
{-# INLINEABLE laMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListApps (Core.Maybe Types.NextToken)
laNextToken = Lens.field @"nextToken"
{-# INLINEABLE laNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The parameter by which to sort the results. The default is CreationTime.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laSortBy :: Lens.Lens' ListApps (Core.Maybe Types.AppSortKey)
laSortBy = Lens.field @"sortBy"
{-# INLINEABLE laSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

-- | The sort order for the results. The default is Ascending.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laSortOrder :: Lens.Lens' ListApps (Core.Maybe Types.SortOrder)
laSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE laSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

-- | A parameter to search by user profile name.
--
-- /Note:/ Consider using 'userProfileNameEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laUserProfileNameEquals :: Lens.Lens' ListApps (Core.Maybe Types.UserProfileName)
laUserProfileNameEquals = Lens.field @"userProfileNameEquals"
{-# INLINEABLE laUserProfileNameEquals #-}
{-# DEPRECATED userProfileNameEquals "Use generic-lens or generic-optics with 'userProfileNameEquals' instead"  #-}

instance Core.ToQuery ListApps where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListApps where
        toHeaders ListApps{..}
          = Core.pure ("X-Amz-Target", "SageMaker.ListApps") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListApps where
        toJSON ListApps{..}
          = Core.object
              (Core.catMaybes
                 [("DomainIdEquals" Core..=) Core.<$> domainIdEquals,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SortBy" Core..=) Core.<$> sortBy,
                  ("SortOrder" Core..=) Core.<$> sortOrder,
                  ("UserProfileNameEquals" Core..=) Core.<$> userProfileNameEquals])

instance Core.AWSRequest ListApps where
        type Rs ListApps = ListAppsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListAppsResponse' Core.<$>
                   (x Core..:? "Apps") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListApps where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"apps" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListAppsResponse' smart constructor.
data ListAppsResponse = ListAppsResponse'
  { apps :: Core.Maybe [Types.AppDetails]
    -- ^ The list of apps.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListAppsResponse' value with any optional fields omitted.
mkListAppsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAppsResponse
mkListAppsResponse responseStatus
  = ListAppsResponse'{apps = Core.Nothing, nextToken = Core.Nothing,
                      responseStatus}

-- | The list of apps.
--
-- /Note:/ Consider using 'apps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsApps :: Lens.Lens' ListAppsResponse (Core.Maybe [Types.AppDetails])
larrsApps = Lens.field @"apps"
{-# INLINEABLE larrsApps #-}
{-# DEPRECATED apps "Use generic-lens or generic-optics with 'apps' instead"  #-}

-- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsNextToken :: Lens.Lens' ListAppsResponse (Core.Maybe Types.NextToken)
larrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE larrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsResponseStatus :: Lens.Lens' ListAppsResponse Core.Int
larrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE larrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
