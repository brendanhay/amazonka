{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all of your database snapshots in Amazon Lightsail.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabaseSnapshots
    (
    -- * Creating a request
      GetRelationalDatabaseSnapshots (..)
    , mkGetRelationalDatabaseSnapshots
    -- ** Request lenses
    , grdsPageToken

    -- * Destructuring the response
    , GetRelationalDatabaseSnapshotsResponse (..)
    , mkGetRelationalDatabaseSnapshotsResponse
    -- ** Response lenses
    , grdsrrsNextPageToken
    , grdsrrsRelationalDatabaseSnapshots
    , grdsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRelationalDatabaseSnapshots' smart constructor.
newtype GetRelationalDatabaseSnapshots = GetRelationalDatabaseSnapshots'
  { pageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseSnapshots@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRelationalDatabaseSnapshots' value with any optional fields omitted.
mkGetRelationalDatabaseSnapshots
    :: GetRelationalDatabaseSnapshots
mkGetRelationalDatabaseSnapshots
  = GetRelationalDatabaseSnapshots'{pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseSnapshots@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdsPageToken :: Lens.Lens' GetRelationalDatabaseSnapshots (Core.Maybe Core.Text)
grdsPageToken = Lens.field @"pageToken"
{-# INLINEABLE grdsPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery GetRelationalDatabaseSnapshots where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetRelationalDatabaseSnapshots where
        toHeaders GetRelationalDatabaseSnapshots{..}
          = Core.pure
              ("X-Amz-Target",
               "Lightsail_20161128.GetRelationalDatabaseSnapshots")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetRelationalDatabaseSnapshots where
        toJSON GetRelationalDatabaseSnapshots{..}
          = Core.object
              (Core.catMaybes [("pageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest GetRelationalDatabaseSnapshots where
        type Rs GetRelationalDatabaseSnapshots =
             GetRelationalDatabaseSnapshotsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetRelationalDatabaseSnapshotsResponse' Core.<$>
                   (x Core..:? "nextPageToken") Core.<*>
                     x Core..:? "relationalDatabaseSnapshots"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetRelationalDatabaseSnapshots where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"relationalDatabaseSnapshots" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"pageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | /See:/ 'mkGetRelationalDatabaseSnapshotsResponse' smart constructor.
data GetRelationalDatabaseSnapshotsResponse = GetRelationalDatabaseSnapshotsResponse'
  { nextPageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetRelationalDatabaseSnapshots@ request and specify the next page token using the @pageToken@ parameter.
  , relationalDatabaseSnapshots :: Core.Maybe [Types.RelationalDatabaseSnapshot]
    -- ^ An object describing the result of your get relational database snapshots request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetRelationalDatabaseSnapshotsResponse' value with any optional fields omitted.
mkGetRelationalDatabaseSnapshotsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetRelationalDatabaseSnapshotsResponse
mkGetRelationalDatabaseSnapshotsResponse responseStatus
  = GetRelationalDatabaseSnapshotsResponse'{nextPageToken =
                                              Core.Nothing,
                                            relationalDatabaseSnapshots = Core.Nothing,
                                            responseStatus}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetRelationalDatabaseSnapshots@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdsrrsNextPageToken :: Lens.Lens' GetRelationalDatabaseSnapshotsResponse (Core.Maybe Core.Text)
grdsrrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE grdsrrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | An object describing the result of your get relational database snapshots request.
--
-- /Note:/ Consider using 'relationalDatabaseSnapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdsrrsRelationalDatabaseSnapshots :: Lens.Lens' GetRelationalDatabaseSnapshotsResponse (Core.Maybe [Types.RelationalDatabaseSnapshot])
grdsrrsRelationalDatabaseSnapshots = Lens.field @"relationalDatabaseSnapshots"
{-# INLINEABLE grdsrrsRelationalDatabaseSnapshots #-}
{-# DEPRECATED relationalDatabaseSnapshots "Use generic-lens or generic-optics with 'relationalDatabaseSnapshots' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdsrrsResponseStatus :: Lens.Lens' GetRelationalDatabaseSnapshotsResponse Core.Int
grdsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grdsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
