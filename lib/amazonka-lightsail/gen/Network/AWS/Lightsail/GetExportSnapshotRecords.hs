{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetExportSnapshotRecords
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the export snapshot record created as a result of the @export snapshot@ operation.
--
-- An export snapshot record can be used to create a new Amazon EC2 instance and its related resources with the @create cloud formation stack@ operation.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetExportSnapshotRecords
    (
    -- * Creating a request
      GetExportSnapshotRecords (..)
    , mkGetExportSnapshotRecords
    -- ** Request lenses
    , gesrPageToken

    -- * Destructuring the response
    , GetExportSnapshotRecordsResponse (..)
    , mkGetExportSnapshotRecordsResponse
    -- ** Response lenses
    , gesrrrsExportSnapshotRecords
    , gesrrrsNextPageToken
    , gesrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetExportSnapshotRecords' smart constructor.
newtype GetExportSnapshotRecords = GetExportSnapshotRecords'
  { pageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetExportSnapshotRecords@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetExportSnapshotRecords' value with any optional fields omitted.
mkGetExportSnapshotRecords
    :: GetExportSnapshotRecords
mkGetExportSnapshotRecords
  = GetExportSnapshotRecords'{pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetExportSnapshotRecords@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gesrPageToken :: Lens.Lens' GetExportSnapshotRecords (Core.Maybe Core.Text)
gesrPageToken = Lens.field @"pageToken"
{-# INLINEABLE gesrPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery GetExportSnapshotRecords where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetExportSnapshotRecords where
        toHeaders GetExportSnapshotRecords{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.GetExportSnapshotRecords")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetExportSnapshotRecords where
        toJSON GetExportSnapshotRecords{..}
          = Core.object
              (Core.catMaybes [("pageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest GetExportSnapshotRecords where
        type Rs GetExportSnapshotRecords = GetExportSnapshotRecordsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetExportSnapshotRecordsResponse' Core.<$>
                   (x Core..:? "exportSnapshotRecords") Core.<*>
                     x Core..:? "nextPageToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetExportSnapshotRecords where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"exportSnapshotRecords" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"pageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | /See:/ 'mkGetExportSnapshotRecordsResponse' smart constructor.
data GetExportSnapshotRecordsResponse = GetExportSnapshotRecordsResponse'
  { exportSnapshotRecords :: Core.Maybe [Types.ExportSnapshotRecord]
    -- ^ A list of objects describing the export snapshot records.
  , nextPageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetExportSnapshotRecords@ request and specify the next page token using the @pageToken@ parameter.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetExportSnapshotRecordsResponse' value with any optional fields omitted.
mkGetExportSnapshotRecordsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetExportSnapshotRecordsResponse
mkGetExportSnapshotRecordsResponse responseStatus
  = GetExportSnapshotRecordsResponse'{exportSnapshotRecords =
                                        Core.Nothing,
                                      nextPageToken = Core.Nothing, responseStatus}

-- | A list of objects describing the export snapshot records.
--
-- /Note:/ Consider using 'exportSnapshotRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gesrrrsExportSnapshotRecords :: Lens.Lens' GetExportSnapshotRecordsResponse (Core.Maybe [Types.ExportSnapshotRecord])
gesrrrsExportSnapshotRecords = Lens.field @"exportSnapshotRecords"
{-# INLINEABLE gesrrrsExportSnapshotRecords #-}
{-# DEPRECATED exportSnapshotRecords "Use generic-lens or generic-optics with 'exportSnapshotRecords' instead"  #-}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetExportSnapshotRecords@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gesrrrsNextPageToken :: Lens.Lens' GetExportSnapshotRecordsResponse (Core.Maybe Core.Text)
gesrrrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE gesrrrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gesrrrsResponseStatus :: Lens.Lens' GetExportSnapshotRecordsResponse Core.Int
gesrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gesrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
