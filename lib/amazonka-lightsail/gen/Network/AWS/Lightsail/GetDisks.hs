{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetDisks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all block storage disks in your AWS account and region.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetDisks
  ( -- * Creating a request
    GetDisks (..),
    mkGetDisks,

    -- ** Request lenses
    gdfPageToken,

    -- * Destructuring the response
    GetDisksResponse (..),
    mkGetDisksResponse,

    -- ** Response lenses
    gdrhrsDisks,
    gdrhrsNextPageToken,
    gdrhrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDisks' smart constructor.
newtype GetDisks = GetDisks'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetDisks@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDisks' value with any optional fields omitted.
mkGetDisks ::
  GetDisks
mkGetDisks = GetDisks' {pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDisks@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdfPageToken :: Lens.Lens' GetDisks (Core.Maybe Types.String)
gdfPageToken = Lens.field @"pageToken"
{-# DEPRECATED gdfPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Core.FromJSON GetDisks where
  toJSON GetDisks {..} =
    Core.object
      (Core.catMaybes [("pageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest GetDisks where
  type Rs GetDisks = GetDisksResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.GetDisks")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDisksResponse'
            Core.<$> (x Core..:? "disks")
            Core.<*> (x Core..:? "nextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetDisks where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
      Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"disks" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"pageToken"
            Lens..~ rs Lens.^. Lens.field @"nextPageToken"
        )

-- | /See:/ 'mkGetDisksResponse' smart constructor.
data GetDisksResponse = GetDisksResponse'
  { -- | An array of objects containing information about all block storage disks.
    disks :: Core.Maybe [Types.Disk],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetDisks@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetDisksResponse' value with any optional fields omitted.
mkGetDisksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDisksResponse
mkGetDisksResponse responseStatus =
  GetDisksResponse'
    { disks = Core.Nothing,
      nextPageToken = Core.Nothing,
      responseStatus
    }

-- | An array of objects containing information about all block storage disks.
--
-- /Note:/ Consider using 'disks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrhrsDisks :: Lens.Lens' GetDisksResponse (Core.Maybe [Types.Disk])
gdrhrsDisks = Lens.field @"disks"
{-# DEPRECATED gdrhrsDisks "Use generic-lens or generic-optics with 'disks' instead." #-}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetDisks@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrhrsNextPageToken :: Lens.Lens' GetDisksResponse (Core.Maybe Types.String)
gdrhrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED gdrhrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrhrsResponseStatus :: Lens.Lens' GetDisksResponse Core.Int
gdrhrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdrhrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
