{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListTapes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists virtual tapes in your virtual tape library (VTL) and your virtual tape shelf (VTS). You specify the tapes to list by specifying one or more tape Amazon Resource Names (ARNs). If you don't specify a tape ARN, the operation lists all virtual tapes in both your VTL and VTS.
--
-- This operation supports pagination. By default, the operation returns a maximum of up to 100 tapes. You can optionally specify the @Limit@ parameter in the body to limit the number of tapes in the response. If the number of tapes returned in the response is truncated, the response includes a @Marker@ element that you can use in your subsequent request to retrieve the next set of tapes. This operation is only supported in the tape gateway type.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.ListTapes
  ( -- * Creating a request
    ListTapes (..),
    mkListTapes,

    -- ** Request lenses
    ltLimit,
    ltMarker,
    ltTapeARNs,

    -- * Destructuring the response
    ListTapesResponse (..),
    mkListTapesResponse,

    -- ** Response lenses
    ltrrsMarker,
    ltrrsTapeInfos,
    ltrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | A JSON object that contains one or more of the following fields:
--
--
--     * 'ListTapesInput$Limit'
--
--
--     * 'ListTapesInput$Marker'
--
--
--     * 'ListTapesInput$TapeARNs'
--
--
--
-- /See:/ 'mkListTapes' smart constructor.
data ListTapes = ListTapes'
  { -- | An optional number limit for the tapes in the list returned by this call.
    limit :: Core.Maybe Core.Natural,
    -- | A string that indicates the position at which to begin the returned list of tapes.
    marker :: Core.Maybe Types.Marker,
    tapeARNs :: Core.Maybe [Types.TapeARN]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTapes' value with any optional fields omitted.
mkListTapes ::
  ListTapes
mkListTapes =
  ListTapes'
    { limit = Core.Nothing,
      marker = Core.Nothing,
      tapeARNs = Core.Nothing
    }

-- | An optional number limit for the tapes in the list returned by this call.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltLimit :: Lens.Lens' ListTapes (Core.Maybe Core.Natural)
ltLimit = Lens.field @"limit"
{-# DEPRECATED ltLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A string that indicates the position at which to begin the returned list of tapes.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMarker :: Lens.Lens' ListTapes (Core.Maybe Types.Marker)
ltMarker = Lens.field @"marker"
{-# DEPRECATED ltMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tapeARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltTapeARNs :: Lens.Lens' ListTapes (Core.Maybe [Types.TapeARN])
ltTapeARNs = Lens.field @"tapeARNs"
{-# DEPRECATED ltTapeARNs "Use generic-lens or generic-optics with 'tapeARNs' instead." #-}

instance Core.FromJSON ListTapes where
  toJSON ListTapes {..} =
    Core.object
      ( Core.catMaybes
          [ ("Limit" Core..=) Core.<$> limit,
            ("Marker" Core..=) Core.<$> marker,
            ("TapeARNs" Core..=) Core.<$> tapeARNs
          ]
      )

instance Core.AWSRequest ListTapes where
  type Rs ListTapes = ListTapesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "StorageGateway_20130630.ListTapes")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTapesResponse'
            Core.<$> (x Core..:? "Marker")
            Core.<*> (x Core..:? "TapeInfos")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListTapes where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"tapeInfos" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | A JSON object containing the following fields:
--
--
--     * 'ListTapesOutput$Marker'
--
--
--     * 'ListTapesOutput$VolumeInfos'
--
--
--
-- /See:/ 'mkListTapesResponse' smart constructor.
data ListTapesResponse = ListTapesResponse'
  { -- | A string that indicates the position at which to begin returning the next list of tapes. Use the marker in your next request to continue pagination of tapes. If there are no more tapes to list, this element does not appear in the response body.
    marker :: Core.Maybe Types.Marker,
    tapeInfos :: Core.Maybe [Types.TapeInfo],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListTapesResponse' value with any optional fields omitted.
mkListTapesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTapesResponse
mkListTapesResponse responseStatus =
  ListTapesResponse'
    { marker = Core.Nothing,
      tapeInfos = Core.Nothing,
      responseStatus
    }

-- | A string that indicates the position at which to begin returning the next list of tapes. Use the marker in your next request to continue pagination of tapes. If there are no more tapes to list, this element does not appear in the response body.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsMarker :: Lens.Lens' ListTapesResponse (Core.Maybe Types.Marker)
ltrrsMarker = Lens.field @"marker"
{-# DEPRECATED ltrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tapeInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsTapeInfos :: Lens.Lens' ListTapesResponse (Core.Maybe [Types.TapeInfo])
ltrrsTapeInfos = Lens.field @"tapeInfos"
{-# DEPRECATED ltrrsTapeInfos "Use generic-lens or generic-optics with 'tapeInfos' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsResponseStatus :: Lens.Lens' ListTapesResponse Core.Int
ltrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
