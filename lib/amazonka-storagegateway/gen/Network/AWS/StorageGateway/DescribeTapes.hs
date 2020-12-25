{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeTapes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the specified Amazon Resource Name (ARN) of virtual tapes. If a @TapeARN@ is not specified, returns a description of all virtual tapes associated with the specified gateway. This operation is only supported in the tape gateway type.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.DescribeTapes
  ( -- * Creating a request
    DescribeTapes (..),
    mkDescribeTapes,

    -- ** Request lenses
    dtGatewayARN,
    dtLimit,
    dtMarker,
    dtTapeARNs,

    -- * Destructuring the response
    DescribeTapesResponse (..),
    mkDescribeTapesResponse,

    -- ** Response lenses
    dtrrsMarker,
    dtrrsTapes,
    dtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | DescribeTapesInput
--
-- /See:/ 'mkDescribeTapes' smart constructor.
data DescribeTapes = DescribeTapes'
  { gatewayARN :: Types.GatewayARN,
    -- | Specifies that the number of virtual tapes described be limited to the specified number.
    limit :: Core.Maybe Core.Natural,
    -- | A marker value, obtained in a previous call to @DescribeTapes@ . This marker indicates which page of results to retrieve.
    --
    -- If not specified, the first page of results is retrieved.
    marker :: Core.Maybe Types.Marker,
    -- | Specifies one or more unique Amazon Resource Names (ARNs) that represent the virtual tapes you want to describe. If this parameter is not specified, Tape gateway returns a description of all virtual tapes associated with the specified gateway.
    tapeARNs :: Core.Maybe [Types.TapeARN]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTapes' value with any optional fields omitted.
mkDescribeTapes ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  DescribeTapes
mkDescribeTapes gatewayARN =
  DescribeTapes'
    { gatewayARN,
      limit = Core.Nothing,
      marker = Core.Nothing,
      tapeARNs = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtGatewayARN :: Lens.Lens' DescribeTapes Types.GatewayARN
dtGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED dtGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | Specifies that the number of virtual tapes described be limited to the specified number.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtLimit :: Lens.Lens' DescribeTapes (Core.Maybe Core.Natural)
dtLimit = Lens.field @"limit"
{-# DEPRECATED dtLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A marker value, obtained in a previous call to @DescribeTapes@ . This marker indicates which page of results to retrieve.
--
-- If not specified, the first page of results is retrieved.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtMarker :: Lens.Lens' DescribeTapes (Core.Maybe Types.Marker)
dtMarker = Lens.field @"marker"
{-# DEPRECATED dtMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Specifies one or more unique Amazon Resource Names (ARNs) that represent the virtual tapes you want to describe. If this parameter is not specified, Tape gateway returns a description of all virtual tapes associated with the specified gateway.
--
-- /Note:/ Consider using 'tapeARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTapeARNs :: Lens.Lens' DescribeTapes (Core.Maybe [Types.TapeARN])
dtTapeARNs = Lens.field @"tapeARNs"
{-# DEPRECATED dtTapeARNs "Use generic-lens or generic-optics with 'tapeARNs' instead." #-}

instance Core.FromJSON DescribeTapes where
  toJSON DescribeTapes {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayARN" Core..= gatewayARN),
            ("Limit" Core..=) Core.<$> limit,
            ("Marker" Core..=) Core.<$> marker,
            ("TapeARNs" Core..=) Core.<$> tapeARNs
          ]
      )

instance Core.AWSRequest DescribeTapes where
  type Rs DescribeTapes = DescribeTapesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "StorageGateway_20130630.DescribeTapes")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTapesResponse'
            Core.<$> (x Core..:? "Marker")
            Core.<*> (x Core..:? "Tapes")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeTapes where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"tapes" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | DescribeTapesOutput
--
-- /See:/ 'mkDescribeTapesResponse' smart constructor.
data DescribeTapesResponse = DescribeTapesResponse'
  { -- | An opaque string which can be used as part of a subsequent DescribeTapes call to retrieve the next page of results.
    --
    -- If a response does not contain a marker, then there are no more results to be retrieved.
    marker :: Core.Maybe Types.Marker,
    -- | An array of virtual tape descriptions.
    tapes :: Core.Maybe [Types.Tape],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeTapesResponse' value with any optional fields omitted.
mkDescribeTapesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTapesResponse
mkDescribeTapesResponse responseStatus =
  DescribeTapesResponse'
    { marker = Core.Nothing,
      tapes = Core.Nothing,
      responseStatus
    }

-- | An opaque string which can be used as part of a subsequent DescribeTapes call to retrieve the next page of results.
--
-- If a response does not contain a marker, then there are no more results to be retrieved.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsMarker :: Lens.Lens' DescribeTapesResponse (Core.Maybe Types.Marker)
dtrrsMarker = Lens.field @"marker"
{-# DEPRECATED dtrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | An array of virtual tape descriptions.
--
-- /Note:/ Consider using 'tapes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsTapes :: Lens.Lens' DescribeTapesResponse (Core.Maybe [Types.Tape])
dtrrsTapes = Lens.field @"tapes"
{-# DEPRECATED dtrrsTapes "Use generic-lens or generic-optics with 'tapes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsResponseStatus :: Lens.Lens' DescribeTapesResponse Core.Int
dtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
