{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetDiskSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all block storage disk snapshots in your AWS account and region.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetDiskSnapshots
  ( -- * Creating a request
    GetDiskSnapshots (..),
    mkGetDiskSnapshots,

    -- ** Request lenses
    gdsPageToken,

    -- * Destructuring the response
    GetDiskSnapshotsResponse (..),
    mkGetDiskSnapshotsResponse,

    -- ** Response lenses
    gdsrrsDiskSnapshots,
    gdsrrsNextPageToken,
    gdsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDiskSnapshots' smart constructor.
newtype GetDiskSnapshots = GetDiskSnapshots'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetDiskSnapshots@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDiskSnapshots' value with any optional fields omitted.
mkGetDiskSnapshots ::
  GetDiskSnapshots
mkGetDiskSnapshots = GetDiskSnapshots' {pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDiskSnapshots@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsPageToken :: Lens.Lens' GetDiskSnapshots (Core.Maybe Types.String)
gdsPageToken = Lens.field @"pageToken"
{-# DEPRECATED gdsPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Core.FromJSON GetDiskSnapshots where
  toJSON GetDiskSnapshots {..} =
    Core.object
      (Core.catMaybes [("pageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest GetDiskSnapshots where
  type Rs GetDiskSnapshots = GetDiskSnapshotsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.GetDiskSnapshots")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDiskSnapshotsResponse'
            Core.<$> (x Core..:? "diskSnapshots")
            Core.<*> (x Core..:? "nextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetDiskSnapshots where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
      Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"diskSnapshots" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"pageToken"
            Lens..~ rs Lens.^. Lens.field @"nextPageToken"
        )

-- | /See:/ 'mkGetDiskSnapshotsResponse' smart constructor.
data GetDiskSnapshotsResponse = GetDiskSnapshotsResponse'
  { -- | An array of objects containing information about all block storage disk snapshots.
    diskSnapshots :: Core.Maybe [Types.DiskSnapshot],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetDiskSnapshots@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetDiskSnapshotsResponse' value with any optional fields omitted.
mkGetDiskSnapshotsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDiskSnapshotsResponse
mkGetDiskSnapshotsResponse responseStatus =
  GetDiskSnapshotsResponse'
    { diskSnapshots = Core.Nothing,
      nextPageToken = Core.Nothing,
      responseStatus
    }

-- | An array of objects containing information about all block storage disk snapshots.
--
-- /Note:/ Consider using 'diskSnapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsDiskSnapshots :: Lens.Lens' GetDiskSnapshotsResponse (Core.Maybe [Types.DiskSnapshot])
gdsrrsDiskSnapshots = Lens.field @"diskSnapshots"
{-# DEPRECATED gdsrrsDiskSnapshots "Use generic-lens or generic-optics with 'diskSnapshots' instead." #-}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetDiskSnapshots@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsNextPageToken :: Lens.Lens' GetDiskSnapshotsResponse (Core.Maybe Types.String)
gdsrrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED gdsrrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsResponseStatus :: Lens.Lens' GetDiskSnapshotsResponse Core.Int
gdsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
