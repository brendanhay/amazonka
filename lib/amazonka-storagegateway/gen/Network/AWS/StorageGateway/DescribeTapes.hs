{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeTapes (..)
    , mkDescribeTapes
    -- ** Request lenses
    , dtGatewayARN
    , dtLimit
    , dtMarker
    , dtTapeARNs

    -- * Destructuring the response
    , DescribeTapesResponse (..)
    , mkDescribeTapesResponse
    -- ** Response lenses
    , dtrrsMarker
    , dtrrsTapes
    , dtrrsResponseStatus
    ) where

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
  { gatewayARN :: Types.GatewayARN
  , limit :: Core.Maybe Core.Natural
    -- ^ Specifies that the number of virtual tapes described be limited to the specified number.
  , marker :: Core.Maybe Types.Marker
    -- ^ A marker value, obtained in a previous call to @DescribeTapes@ . This marker indicates which page of results to retrieve.
--
-- If not specified, the first page of results is retrieved.
  , tapeARNs :: Core.Maybe [Types.TapeARN]
    -- ^ Specifies one or more unique Amazon Resource Names (ARNs) that represent the virtual tapes you want to describe. If this parameter is not specified, Tape gateway returns a description of all virtual tapes associated with the specified gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTapes' value with any optional fields omitted.
mkDescribeTapes
    :: Types.GatewayARN -- ^ 'gatewayARN'
    -> DescribeTapes
mkDescribeTapes gatewayARN
  = DescribeTapes'{gatewayARN, limit = Core.Nothing,
                   marker = Core.Nothing, tapeARNs = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtGatewayARN :: Lens.Lens' DescribeTapes Types.GatewayARN
dtGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE dtGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | Specifies that the number of virtual tapes described be limited to the specified number.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtLimit :: Lens.Lens' DescribeTapes (Core.Maybe Core.Natural)
dtLimit = Lens.field @"limit"
{-# INLINEABLE dtLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | A marker value, obtained in a previous call to @DescribeTapes@ . This marker indicates which page of results to retrieve.
--
-- If not specified, the first page of results is retrieved.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtMarker :: Lens.Lens' DescribeTapes (Core.Maybe Types.Marker)
dtMarker = Lens.field @"marker"
{-# INLINEABLE dtMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | Specifies one or more unique Amazon Resource Names (ARNs) that represent the virtual tapes you want to describe. If this parameter is not specified, Tape gateway returns a description of all virtual tapes associated with the specified gateway.
--
-- /Note:/ Consider using 'tapeARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTapeARNs :: Lens.Lens' DescribeTapes (Core.Maybe [Types.TapeARN])
dtTapeARNs = Lens.field @"tapeARNs"
{-# INLINEABLE dtTapeARNs #-}
{-# DEPRECATED tapeARNs "Use generic-lens or generic-optics with 'tapeARNs' instead"  #-}

instance Core.ToQuery DescribeTapes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeTapes where
        toHeaders DescribeTapes{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.DescribeTapes")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeTapes where
        toJSON DescribeTapes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GatewayARN" Core..= gatewayARN),
                  ("Limit" Core..=) Core.<$> limit,
                  ("Marker" Core..=) Core.<$> marker,
                  ("TapeARNs" Core..=) Core.<$> tapeARNs])

instance Core.AWSRequest DescribeTapes where
        type Rs DescribeTapes = DescribeTapesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeTapesResponse' Core.<$>
                   (x Core..:? "Marker") Core.<*> x Core..:? "Tapes" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeTapes where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"tapes" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | DescribeTapesOutput
--
-- /See:/ 'mkDescribeTapesResponse' smart constructor.
data DescribeTapesResponse = DescribeTapesResponse'
  { marker :: Core.Maybe Types.Marker
    -- ^ An opaque string which can be used as part of a subsequent DescribeTapes call to retrieve the next page of results.
--
-- If a response does not contain a marker, then there are no more results to be retrieved.
  , tapes :: Core.Maybe [Types.Tape]
    -- ^ An array of virtual tape descriptions.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeTapesResponse' value with any optional fields omitted.
mkDescribeTapesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeTapesResponse
mkDescribeTapesResponse responseStatus
  = DescribeTapesResponse'{marker = Core.Nothing,
                           tapes = Core.Nothing, responseStatus}

-- | An opaque string which can be used as part of a subsequent DescribeTapes call to retrieve the next page of results.
--
-- If a response does not contain a marker, then there are no more results to be retrieved.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsMarker :: Lens.Lens' DescribeTapesResponse (Core.Maybe Types.Marker)
dtrrsMarker = Lens.field @"marker"
{-# INLINEABLE dtrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | An array of virtual tape descriptions.
--
-- /Note:/ Consider using 'tapes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsTapes :: Lens.Lens' DescribeTapesResponse (Core.Maybe [Types.Tape])
dtrrsTapes = Lens.field @"tapes"
{-# INLINEABLE dtrrsTapes #-}
{-# DEPRECATED tapes "Use generic-lens or generic-optics with 'tapes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsResponseStatus :: Lens.Lens' DescribeTapesResponse Core.Int
dtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
