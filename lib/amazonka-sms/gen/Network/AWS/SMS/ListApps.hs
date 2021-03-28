{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.ListApps
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves summaries for all applications.
--
-- This operation returns paginated results.
module Network.AWS.SMS.ListApps
    (
    -- * Creating a request
      ListApps (..)
    , mkListApps
    -- ** Request lenses
    , laAppIds
    , laMaxResults
    , laNextToken

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
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkListApps' smart constructor.
data ListApps = ListApps'
  { appIds :: Core.Maybe [Types.AppId]
    -- ^ The unique application IDs.
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to return in a single call. The default value is 100. To retrieve the remaining results, make another call with the returned @NextToken@ value. 
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListApps' value with any optional fields omitted.
mkListApps
    :: ListApps
mkListApps
  = ListApps'{appIds = Core.Nothing, maxResults = Core.Nothing,
              nextToken = Core.Nothing}

-- | The unique application IDs.
--
-- /Note:/ Consider using 'appIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laAppIds :: Lens.Lens' ListApps (Core.Maybe [Types.AppId])
laAppIds = Lens.field @"appIds"
{-# INLINEABLE laAppIds #-}
{-# DEPRECATED appIds "Use generic-lens or generic-optics with 'appIds' instead"  #-}

-- | The maximum number of results to return in a single call. The default value is 100. To retrieve the remaining results, make another call with the returned @NextToken@ value. 
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMaxResults :: Lens.Lens' ListApps (Core.Maybe Core.Int)
laMaxResults = Lens.field @"maxResults"
{-# INLINEABLE laMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListApps (Core.Maybe Types.NextToken)
laNextToken = Lens.field @"nextToken"
{-# INLINEABLE laNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListApps where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListApps where
        toHeaders ListApps{..}
          = Core.pure
              ("X-Amz-Target", "AWSServerMigrationService_V2016_10_24.ListApps")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListApps where
        toJSON ListApps{..}
          = Core.object
              (Core.catMaybes
                 [("appIds" Core..=) Core.<$> appIds,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

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
                   (x Core..:? "apps") Core.<*> x Core..:? "nextToken" Core.<*>
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
  { apps :: Core.Maybe [Types.AppSummary]
    -- ^ The application summaries.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token required to retrieve the next set of results. This value is null when there are no more results to return.
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

-- | The application summaries.
--
-- /Note:/ Consider using 'apps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsApps :: Lens.Lens' ListAppsResponse (Core.Maybe [Types.AppSummary])
larrsApps = Lens.field @"apps"
{-# INLINEABLE larrsApps #-}
{-# DEPRECATED apps "Use generic-lens or generic-optics with 'apps' instead"  #-}

-- | The token required to retrieve the next set of results. This value is null when there are no more results to return.
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
