{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeTapeArchives
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of specified virtual tapes in the virtual tape shelf (VTS). This operation is only supported in the tape gateway type.
--
-- If a specific @TapeARN@ is not specified, AWS Storage Gateway returns a description of all virtual tapes found in the VTS associated with your account.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.DescribeTapeArchives
  ( -- * Creating a request
    DescribeTapeArchives (..),
    mkDescribeTapeArchives,

    -- ** Request lenses
    dtaLimit,
    dtaMarker,
    dtaTapeARNs,

    -- * Destructuring the response
    DescribeTapeArchivesResponse (..),
    mkDescribeTapeArchivesResponse,

    -- ** Response lenses
    dtarrsMarker,
    dtarrsTapeArchives,
    dtarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | DescribeTapeArchivesInput
--
-- /See:/ 'mkDescribeTapeArchives' smart constructor.
data DescribeTapeArchives = DescribeTapeArchives'
  { -- | Specifies that the number of virtual tapes described be limited to the specified number.
    limit :: Core.Maybe Core.Natural,
    -- | An opaque string that indicates the position at which to begin describing virtual tapes.
    marker :: Core.Maybe Types.Marker,
    -- | Specifies one or more unique Amazon Resource Names (ARNs) that represent the virtual tapes you want to describe.
    tapeARNs :: Core.Maybe [Types.TapeARN]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTapeArchives' value with any optional fields omitted.
mkDescribeTapeArchives ::
  DescribeTapeArchives
mkDescribeTapeArchives =
  DescribeTapeArchives'
    { limit = Core.Nothing,
      marker = Core.Nothing,
      tapeARNs = Core.Nothing
    }

-- | Specifies that the number of virtual tapes described be limited to the specified number.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtaLimit :: Lens.Lens' DescribeTapeArchives (Core.Maybe Core.Natural)
dtaLimit = Lens.field @"limit"
{-# DEPRECATED dtaLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | An opaque string that indicates the position at which to begin describing virtual tapes.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtaMarker :: Lens.Lens' DescribeTapeArchives (Core.Maybe Types.Marker)
dtaMarker = Lens.field @"marker"
{-# DEPRECATED dtaMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Specifies one or more unique Amazon Resource Names (ARNs) that represent the virtual tapes you want to describe.
--
-- /Note:/ Consider using 'tapeARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtaTapeARNs :: Lens.Lens' DescribeTapeArchives (Core.Maybe [Types.TapeARN])
dtaTapeARNs = Lens.field @"tapeARNs"
{-# DEPRECATED dtaTapeARNs "Use generic-lens or generic-optics with 'tapeARNs' instead." #-}

instance Core.FromJSON DescribeTapeArchives where
  toJSON DescribeTapeArchives {..} =
    Core.object
      ( Core.catMaybes
          [ ("Limit" Core..=) Core.<$> limit,
            ("Marker" Core..=) Core.<$> marker,
            ("TapeARNs" Core..=) Core.<$> tapeARNs
          ]
      )

instance Core.AWSRequest DescribeTapeArchives where
  type Rs DescribeTapeArchives = DescribeTapeArchivesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.DescribeTapeArchives")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTapeArchivesResponse'
            Core.<$> (x Core..:? "Marker")
            Core.<*> (x Core..:? "TapeArchives")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeTapeArchives where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"tapeArchives" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | DescribeTapeArchivesOutput
--
-- /See:/ 'mkDescribeTapeArchivesResponse' smart constructor.
data DescribeTapeArchivesResponse = DescribeTapeArchivesResponse'
  { -- | An opaque string that indicates the position at which the virtual tapes that were fetched for description ended. Use this marker in your next request to fetch the next set of virtual tapes in the virtual tape shelf (VTS). If there are no more virtual tapes to describe, this field does not appear in the response.
    marker :: Core.Maybe Types.Marker,
    -- | An array of virtual tape objects in the virtual tape shelf (VTS). The description includes of the Amazon Resource Name (ARN) of the virtual tapes. The information returned includes the Amazon Resource Names (ARNs) of the tapes, size of the tapes, status of the tapes, progress of the description, and tape barcode.
    tapeArchives :: Core.Maybe [Types.TapeArchive],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeTapeArchivesResponse' value with any optional fields omitted.
mkDescribeTapeArchivesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTapeArchivesResponse
mkDescribeTapeArchivesResponse responseStatus =
  DescribeTapeArchivesResponse'
    { marker = Core.Nothing,
      tapeArchives = Core.Nothing,
      responseStatus
    }

-- | An opaque string that indicates the position at which the virtual tapes that were fetched for description ended. Use this marker in your next request to fetch the next set of virtual tapes in the virtual tape shelf (VTS). If there are no more virtual tapes to describe, this field does not appear in the response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtarrsMarker :: Lens.Lens' DescribeTapeArchivesResponse (Core.Maybe Types.Marker)
dtarrsMarker = Lens.field @"marker"
{-# DEPRECATED dtarrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | An array of virtual tape objects in the virtual tape shelf (VTS). The description includes of the Amazon Resource Name (ARN) of the virtual tapes. The information returned includes the Amazon Resource Names (ARNs) of the tapes, size of the tapes, status of the tapes, progress of the description, and tape barcode.
--
-- /Note:/ Consider using 'tapeArchives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtarrsTapeArchives :: Lens.Lens' DescribeTapeArchivesResponse (Core.Maybe [Types.TapeArchive])
dtarrsTapeArchives = Lens.field @"tapeArchives"
{-# DEPRECATED dtarrsTapeArchives "Use generic-lens or generic-optics with 'tapeArchives' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtarrsResponseStatus :: Lens.Lens' DescribeTapeArchivesResponse Core.Int
dtarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
