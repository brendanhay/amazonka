{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetInstanceSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all instance snapshots for the user's account.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetInstanceSnapshots
  ( -- * Creating a request
    GetInstanceSnapshots (..),
    mkGetInstanceSnapshots,

    -- ** Request lenses
    gisPageToken,

    -- * Destructuring the response
    GetInstanceSnapshotsResponse (..),
    mkGetInstanceSnapshotsResponse,

    -- ** Response lenses
    gisrrsInstanceSnapshots,
    gisrrsNextPageToken,
    gisrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetInstanceSnapshots' smart constructor.
newtype GetInstanceSnapshots = GetInstanceSnapshots'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetInstanceSnapshots@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetInstanceSnapshots' value with any optional fields omitted.
mkGetInstanceSnapshots ::
  GetInstanceSnapshots
mkGetInstanceSnapshots =
  GetInstanceSnapshots' {pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetInstanceSnapshots@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisPageToken :: Lens.Lens' GetInstanceSnapshots (Core.Maybe Types.String)
gisPageToken = Lens.field @"pageToken"
{-# DEPRECATED gisPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Core.FromJSON GetInstanceSnapshots where
  toJSON GetInstanceSnapshots {..} =
    Core.object
      (Core.catMaybes [("pageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest GetInstanceSnapshots where
  type Rs GetInstanceSnapshots = GetInstanceSnapshotsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.GetInstanceSnapshots")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstanceSnapshotsResponse'
            Core.<$> (x Core..:? "instanceSnapshots")
            Core.<*> (x Core..:? "nextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetInstanceSnapshots where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
      Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"instanceSnapshots" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"pageToken"
            Lens..~ rs Lens.^. Lens.field @"nextPageToken"
        )

-- | /See:/ 'mkGetInstanceSnapshotsResponse' smart constructor.
data GetInstanceSnapshotsResponse = GetInstanceSnapshotsResponse'
  { -- | An array of key-value pairs containing information about the results of your get instance snapshots request.
    instanceSnapshots :: Core.Maybe [Types.InstanceSnapshot],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetInstanceSnapshots@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetInstanceSnapshotsResponse' value with any optional fields omitted.
mkGetInstanceSnapshotsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetInstanceSnapshotsResponse
mkGetInstanceSnapshotsResponse responseStatus =
  GetInstanceSnapshotsResponse'
    { instanceSnapshots = Core.Nothing,
      nextPageToken = Core.Nothing,
      responseStatus
    }

-- | An array of key-value pairs containing information about the results of your get instance snapshots request.
--
-- /Note:/ Consider using 'instanceSnapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrrsInstanceSnapshots :: Lens.Lens' GetInstanceSnapshotsResponse (Core.Maybe [Types.InstanceSnapshot])
gisrrsInstanceSnapshots = Lens.field @"instanceSnapshots"
{-# DEPRECATED gisrrsInstanceSnapshots "Use generic-lens or generic-optics with 'instanceSnapshots' instead." #-}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetInstanceSnapshots@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrrsNextPageToken :: Lens.Lens' GetInstanceSnapshotsResponse (Core.Maybe Types.String)
gisrrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED gisrrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrrsResponseStatus :: Lens.Lens' GetInstanceSnapshotsResponse Core.Int
gisrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gisrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
