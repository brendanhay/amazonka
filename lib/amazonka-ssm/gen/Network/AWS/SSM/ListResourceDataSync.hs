{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListResourceDataSync
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your resource data sync configurations. Includes information about the last time a sync attempted to start, the last sync status, and the last time a sync successfully completed.
--
-- The number of sync configurations might be too large to return using a single call to @ListResourceDataSync@ . You can limit the number of sync configurations returned by using the @MaxResults@ parameter. To determine whether there are more sync configurations to list, check the value of @NextToken@ in the output. If there are more sync configurations to list, you can request them by specifying the @NextToken@ returned in the call to the parameter of a subsequent call. 
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListResourceDataSync
    (
    -- * Creating a request
      ListResourceDataSync (..)
    , mkListResourceDataSync
    -- ** Request lenses
    , lrdsMaxResults
    , lrdsNextToken
    , lrdsSyncType

    -- * Destructuring the response
    , ListResourceDataSyncResponse (..)
    , mkListResourceDataSyncResponse
    -- ** Response lenses
    , lrdsrrsNextToken
    , lrdsrrsResourceDataSyncItems
    , lrdsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkListResourceDataSync' smart constructor.
data ListResourceDataSync = ListResourceDataSync'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token to start the list. Use this token to get the next set of results. 
  , syncType :: Core.Maybe Types.ResourceDataSyncType
    -- ^ View a list of resource data syncs according to the sync type. Specify @SyncToDestination@ to view resource data syncs that synchronize data to an Amazon S3 bucket. Specify @SyncFromSource@ to view resource data syncs from AWS Organizations or from multiple AWS Regions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourceDataSync' value with any optional fields omitted.
mkListResourceDataSync
    :: ListResourceDataSync
mkListResourceDataSync
  = ListResourceDataSync'{maxResults = Core.Nothing,
                          nextToken = Core.Nothing, syncType = Core.Nothing}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsMaxResults :: Lens.Lens' ListResourceDataSync (Core.Maybe Core.Natural)
lrdsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lrdsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A token to start the list. Use this token to get the next set of results. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsNextToken :: Lens.Lens' ListResourceDataSync (Core.Maybe Types.NextToken)
lrdsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrdsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | View a list of resource data syncs according to the sync type. Specify @SyncToDestination@ to view resource data syncs that synchronize data to an Amazon S3 bucket. Specify @SyncFromSource@ to view resource data syncs from AWS Organizations or from multiple AWS Regions.
--
-- /Note:/ Consider using 'syncType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsSyncType :: Lens.Lens' ListResourceDataSync (Core.Maybe Types.ResourceDataSyncType)
lrdsSyncType = Lens.field @"syncType"
{-# INLINEABLE lrdsSyncType #-}
{-# DEPRECATED syncType "Use generic-lens or generic-optics with 'syncType' instead"  #-}

instance Core.ToQuery ListResourceDataSync where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListResourceDataSync where
        toHeaders ListResourceDataSync{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.ListResourceDataSync")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListResourceDataSync where
        toJSON ListResourceDataSync{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SyncType" Core..=) Core.<$> syncType])

instance Core.AWSRequest ListResourceDataSync where
        type Rs ListResourceDataSync = ListResourceDataSyncResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListResourceDataSyncResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*>
                     x Core..:? "ResourceDataSyncItems"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListResourceDataSync where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"resourceDataSyncItems" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListResourceDataSyncResponse' smart constructor.
data ListResourceDataSyncResponse = ListResourceDataSyncResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. Use this token to get the next set of results.
  , resourceDataSyncItems :: Core.Maybe [Types.ResourceDataSyncItem]
    -- ^ A list of your current Resource Data Sync configurations and their statuses.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListResourceDataSyncResponse' value with any optional fields omitted.
mkListResourceDataSyncResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListResourceDataSyncResponse
mkListResourceDataSyncResponse responseStatus
  = ListResourceDataSyncResponse'{nextToken = Core.Nothing,
                                  resourceDataSyncItems = Core.Nothing, responseStatus}

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsrrsNextToken :: Lens.Lens' ListResourceDataSyncResponse (Core.Maybe Types.NextToken)
lrdsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrdsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of your current Resource Data Sync configurations and their statuses.
--
-- /Note:/ Consider using 'resourceDataSyncItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsrrsResourceDataSyncItems :: Lens.Lens' ListResourceDataSyncResponse (Core.Maybe [Types.ResourceDataSyncItem])
lrdsrrsResourceDataSyncItems = Lens.field @"resourceDataSyncItems"
{-# INLINEABLE lrdsrrsResourceDataSyncItems #-}
{-# DEPRECATED resourceDataSyncItems "Use generic-lens or generic-optics with 'resourceDataSyncItems' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsrrsResponseStatus :: Lens.Lens' ListResourceDataSyncResponse Core.Int
lrdsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrdsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
