{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetActiveNames
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the names of all active (not deleted) resources.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetActiveNames
  ( -- * Creating a request
    GetActiveNames (..),
    mkGetActiveNames,

    -- ** Request lenses
    ganPageToken,

    -- * Destructuring the response
    GetActiveNamesResponse (..),
    mkGetActiveNamesResponse,

    -- ** Response lenses
    ganrrsActiveNames,
    ganrrsNextPageToken,
    ganrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetActiveNames' smart constructor.
newtype GetActiveNames = GetActiveNames'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetActiveNames@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Types.PageToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetActiveNames' value with any optional fields omitted.
mkGetActiveNames ::
  GetActiveNames
mkGetActiveNames = GetActiveNames' {pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetActiveNames@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ganPageToken :: Lens.Lens' GetActiveNames (Core.Maybe Types.PageToken)
ganPageToken = Lens.field @"pageToken"
{-# DEPRECATED ganPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Core.FromJSON GetActiveNames where
  toJSON GetActiveNames {..} =
    Core.object
      (Core.catMaybes [("pageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest GetActiveNames where
  type Rs GetActiveNames = GetActiveNamesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.GetActiveNames")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetActiveNamesResponse'
            Core.<$> (x Core..:? "activeNames")
            Core.<*> (x Core..:? "nextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetActiveNames where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
      Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"activeNames" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"pageToken"
            Lens..~ rs Lens.^. Lens.field @"nextPageToken"
        )

-- | /See:/ 'mkGetActiveNamesResponse' smart constructor.
data GetActiveNamesResponse = GetActiveNamesResponse'
  { -- | The list of active names returned by the get active names request.
    activeNames :: Core.Maybe [Types.String],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetActiveNames@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetActiveNamesResponse' value with any optional fields omitted.
mkGetActiveNamesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetActiveNamesResponse
mkGetActiveNamesResponse responseStatus =
  GetActiveNamesResponse'
    { activeNames = Core.Nothing,
      nextPageToken = Core.Nothing,
      responseStatus
    }

-- | The list of active names returned by the get active names request.
--
-- /Note:/ Consider using 'activeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ganrrsActiveNames :: Lens.Lens' GetActiveNamesResponse (Core.Maybe [Types.String])
ganrrsActiveNames = Lens.field @"activeNames"
{-# DEPRECATED ganrrsActiveNames "Use generic-lens or generic-optics with 'activeNames' instead." #-}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetActiveNames@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ganrrsNextPageToken :: Lens.Lens' GetActiveNamesResponse (Core.Maybe Types.String)
ganrrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED ganrrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ganrrsResponseStatus :: Lens.Lens' GetActiveNamesResponse Core.Int
ganrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ganrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
