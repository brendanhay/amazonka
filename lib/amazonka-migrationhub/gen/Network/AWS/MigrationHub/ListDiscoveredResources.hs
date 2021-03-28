{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.ListDiscoveredResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists discovered resources associated with the given @MigrationTask@ .
--
-- This operation returns paginated results.
module Network.AWS.MigrationHub.ListDiscoveredResources
    (
    -- * Creating a request
      ListDiscoveredResources (..)
    , mkListDiscoveredResources
    -- ** Request lenses
    , ldrProgressUpdateStream
    , ldrMigrationTaskName
    , ldrMaxResults
    , ldrNextToken

    -- * Destructuring the response
    , ListDiscoveredResourcesResponse (..)
    , mkListDiscoveredResourcesResponse
    -- ** Response lenses
    , ldrrrsDiscoveredResourceList
    , ldrrrsNextToken
    , ldrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDiscoveredResources' smart constructor.
data ListDiscoveredResources = ListDiscoveredResources'
  { progressUpdateStream :: Types.ProgressUpdateStream
    -- ^ The name of the ProgressUpdateStream.
  , migrationTaskName :: Types.MigrationTaskName
    -- ^ The name of the MigrationTask. /Do not store personal data in this field./ 
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results returned per page.
  , nextToken :: Core.Maybe Types.Token
    -- ^ If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDiscoveredResources' value with any optional fields omitted.
mkListDiscoveredResources
    :: Types.ProgressUpdateStream -- ^ 'progressUpdateStream'
    -> Types.MigrationTaskName -- ^ 'migrationTaskName'
    -> ListDiscoveredResources
mkListDiscoveredResources progressUpdateStream migrationTaskName
  = ListDiscoveredResources'{progressUpdateStream, migrationTaskName,
                             maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The name of the ProgressUpdateStream.
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrProgressUpdateStream :: Lens.Lens' ListDiscoveredResources Types.ProgressUpdateStream
ldrProgressUpdateStream = Lens.field @"progressUpdateStream"
{-# INLINEABLE ldrProgressUpdateStream #-}
{-# DEPRECATED progressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead"  #-}

-- | The name of the MigrationTask. /Do not store personal data in this field./ 
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrMigrationTaskName :: Lens.Lens' ListDiscoveredResources Types.MigrationTaskName
ldrMigrationTaskName = Lens.field @"migrationTaskName"
{-# INLINEABLE ldrMigrationTaskName #-}
{-# DEPRECATED migrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead"  #-}

-- | The maximum number of results returned per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrMaxResults :: Lens.Lens' ListDiscoveredResources (Core.Maybe Core.Natural)
ldrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ldrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrNextToken :: Lens.Lens' ListDiscoveredResources (Core.Maybe Types.Token)
ldrNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListDiscoveredResources where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListDiscoveredResources where
        toHeaders ListDiscoveredResources{..}
          = Core.pure
              ("X-Amz-Target", "AWSMigrationHub.ListDiscoveredResources")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListDiscoveredResources where
        toJSON ListDiscoveredResources{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProgressUpdateStream" Core..= progressUpdateStream),
                  Core.Just ("MigrationTaskName" Core..= migrationTaskName),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListDiscoveredResources where
        type Rs ListDiscoveredResources = ListDiscoveredResourcesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListDiscoveredResourcesResponse' Core.<$>
                   (x Core..:? "DiscoveredResourceList") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListDiscoveredResources where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"discoveredResourceList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListDiscoveredResourcesResponse' smart constructor.
data ListDiscoveredResourcesResponse = ListDiscoveredResourcesResponse'
  { discoveredResourceList :: Core.Maybe [Types.DiscoveredResource]
    -- ^ Returned list of discovered resources associated with the given MigrationTask.
  , nextToken :: Core.Maybe Types.Token
    -- ^ If there are more discovered resources than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDiscoveredResourcesResponse' value with any optional fields omitted.
mkListDiscoveredResourcesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListDiscoveredResourcesResponse
mkListDiscoveredResourcesResponse responseStatus
  = ListDiscoveredResourcesResponse'{discoveredResourceList =
                                       Core.Nothing,
                                     nextToken = Core.Nothing, responseStatus}

-- | Returned list of discovered resources associated with the given MigrationTask.
--
-- /Note:/ Consider using 'discoveredResourceList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrrsDiscoveredResourceList :: Lens.Lens' ListDiscoveredResourcesResponse (Core.Maybe [Types.DiscoveredResource])
ldrrrsDiscoveredResourceList = Lens.field @"discoveredResourceList"
{-# INLINEABLE ldrrrsDiscoveredResourceList #-}
{-# DEPRECATED discoveredResourceList "Use generic-lens or generic-optics with 'discoveredResourceList' instead"  #-}

-- | If there are more discovered resources than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrrsNextToken :: Lens.Lens' ListDiscoveredResourcesResponse (Core.Maybe Types.Token)
ldrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrrsResponseStatus :: Lens.Lens' ListDiscoveredResourcesResponse Core.Int
ldrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ldrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
