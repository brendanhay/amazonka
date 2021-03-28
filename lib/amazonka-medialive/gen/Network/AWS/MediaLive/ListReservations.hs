{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.ListReservations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List purchased reservations.
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListReservations
    (
    -- * Creating a request
      ListReservations (..)
    , mkListReservations
    -- ** Request lenses
    , lrChannelClass
    , lrCodec
    , lrMaxResults
    , lrMaximumBitrate
    , lrMaximumFramerate
    , lrNextToken
    , lrResolution
    , lrResourceType
    , lrSpecialFeature
    , lrVideoQuality

    -- * Destructuring the response
    , ListReservationsResponse (..)
    , mkListReservationsResponse
    -- ** Response lenses
    , lrrrsNextToken
    , lrrrsReservations
    , lrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for ListReservationsRequest
--
-- /See:/ 'mkListReservations' smart constructor.
data ListReservations = ListReservations'
  { channelClass :: Core.Maybe Core.Text
    -- ^ Filter by channel class, 'STANDARD' or 'SINGLE_PIPELINE'
  , codec :: Core.Maybe Core.Text
    -- ^ Filter by codec, 'AVC', 'HEVC', 'MPEG2', 'AUDIO', or 'LINK'
  , maxResults :: Core.Maybe Core.Natural
  , maximumBitrate :: Core.Maybe Core.Text
    -- ^ Filter by bitrate, 'MAX_10_MBPS', 'MAX_20_MBPS', or 'MAX_50_MBPS'
  , maximumFramerate :: Core.Maybe Core.Text
    -- ^ Filter by framerate, 'MAX_30_FPS' or 'MAX_60_FPS'
  , nextToken :: Core.Maybe Core.Text
  , resolution :: Core.Maybe Core.Text
    -- ^ Filter by resolution, 'SD', 'HD', 'FHD', or 'UHD'
  , resourceType :: Core.Maybe Core.Text
    -- ^ Filter by resource type, 'INPUT', 'OUTPUT', 'MULTIPLEX', or 'CHANNEL'
  , specialFeature :: Core.Maybe Core.Text
    -- ^ Filter by special feature, 'ADVANCED_AUDIO' or 'AUDIO_NORMALIZATION'
  , videoQuality :: Core.Maybe Core.Text
    -- ^ Filter by video quality, 'STANDARD', 'ENHANCED', or 'PREMIUM'
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListReservations' value with any optional fields omitted.
mkListReservations
    :: ListReservations
mkListReservations
  = ListReservations'{channelClass = Core.Nothing,
                      codec = Core.Nothing, maxResults = Core.Nothing,
                      maximumBitrate = Core.Nothing, maximumFramerate = Core.Nothing,
                      nextToken = Core.Nothing, resolution = Core.Nothing,
                      resourceType = Core.Nothing, specialFeature = Core.Nothing,
                      videoQuality = Core.Nothing}

-- | Filter by channel class, 'STANDARD' or 'SINGLE_PIPELINE'
--
-- /Note:/ Consider using 'channelClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrChannelClass :: Lens.Lens' ListReservations (Core.Maybe Core.Text)
lrChannelClass = Lens.field @"channelClass"
{-# INLINEABLE lrChannelClass #-}
{-# DEPRECATED channelClass "Use generic-lens or generic-optics with 'channelClass' instead"  #-}

-- | Filter by codec, 'AVC', 'HEVC', 'MPEG2', 'AUDIO', or 'LINK'
--
-- /Note:/ Consider using 'codec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrCodec :: Lens.Lens' ListReservations (Core.Maybe Core.Text)
lrCodec = Lens.field @"codec"
{-# INLINEABLE lrCodec #-}
{-# DEPRECATED codec "Use generic-lens or generic-optics with 'codec' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrMaxResults :: Lens.Lens' ListReservations (Core.Maybe Core.Natural)
lrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Filter by bitrate, 'MAX_10_MBPS', 'MAX_20_MBPS', or 'MAX_50_MBPS'
--
-- /Note:/ Consider using 'maximumBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrMaximumBitrate :: Lens.Lens' ListReservations (Core.Maybe Core.Text)
lrMaximumBitrate = Lens.field @"maximumBitrate"
{-# INLINEABLE lrMaximumBitrate #-}
{-# DEPRECATED maximumBitrate "Use generic-lens or generic-optics with 'maximumBitrate' instead"  #-}

-- | Filter by framerate, 'MAX_30_FPS' or 'MAX_60_FPS'
--
-- /Note:/ Consider using 'maximumFramerate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrMaximumFramerate :: Lens.Lens' ListReservations (Core.Maybe Core.Text)
lrMaximumFramerate = Lens.field @"maximumFramerate"
{-# INLINEABLE lrMaximumFramerate #-}
{-# DEPRECATED maximumFramerate "Use generic-lens or generic-optics with 'maximumFramerate' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNextToken :: Lens.Lens' ListReservations (Core.Maybe Core.Text)
lrNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Filter by resolution, 'SD', 'HD', 'FHD', or 'UHD'
--
-- /Note:/ Consider using 'resolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrResolution :: Lens.Lens' ListReservations (Core.Maybe Core.Text)
lrResolution = Lens.field @"resolution"
{-# INLINEABLE lrResolution #-}
{-# DEPRECATED resolution "Use generic-lens or generic-optics with 'resolution' instead"  #-}

-- | Filter by resource type, 'INPUT', 'OUTPUT', 'MULTIPLEX', or 'CHANNEL'
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrResourceType :: Lens.Lens' ListReservations (Core.Maybe Core.Text)
lrResourceType = Lens.field @"resourceType"
{-# INLINEABLE lrResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | Filter by special feature, 'ADVANCED_AUDIO' or 'AUDIO_NORMALIZATION'
--
-- /Note:/ Consider using 'specialFeature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrSpecialFeature :: Lens.Lens' ListReservations (Core.Maybe Core.Text)
lrSpecialFeature = Lens.field @"specialFeature"
{-# INLINEABLE lrSpecialFeature #-}
{-# DEPRECATED specialFeature "Use generic-lens or generic-optics with 'specialFeature' instead"  #-}

-- | Filter by video quality, 'STANDARD', 'ENHANCED', or 'PREMIUM'
--
-- /Note:/ Consider using 'videoQuality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrVideoQuality :: Lens.Lens' ListReservations (Core.Maybe Core.Text)
lrVideoQuality = Lens.field @"videoQuality"
{-# INLINEABLE lrVideoQuality #-}
{-# DEPRECATED videoQuality "Use generic-lens or generic-optics with 'videoQuality' instead"  #-}

instance Core.ToQuery ListReservations where
        toQuery ListReservations{..}
          = Core.maybe Core.mempty (Core.toQueryPair "channelClass")
              channelClass
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "codec") codec
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "maximumBitrate")
                maximumBitrate
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "maximumFramerate")
                maximumFramerate
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "resolution") resolution
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "resourceType")
                resourceType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "specialFeature")
                specialFeature
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "videoQuality")
                videoQuality

instance Core.ToHeaders ListReservations where
        toHeaders ListReservations{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListReservations where
        type Rs ListReservations = ListReservationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/prod/reservations",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListReservationsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "reservations"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListReservations where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"reservations" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Placeholder documentation for ListReservationsResponse
--
-- /See:/ 'mkListReservationsResponse' smart constructor.
data ListReservationsResponse = ListReservationsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ Token to retrieve the next page of results
  , reservations :: Core.Maybe [Types.Reservation]
    -- ^ List of reservations
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListReservationsResponse' value with any optional fields omitted.
mkListReservationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListReservationsResponse
mkListReservationsResponse responseStatus
  = ListReservationsResponse'{nextToken = Core.Nothing,
                              reservations = Core.Nothing, responseStatus}

-- | Token to retrieve the next page of results
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsNextToken :: Lens.Lens' ListReservationsResponse (Core.Maybe Core.Text)
lrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | List of reservations
--
-- /Note:/ Consider using 'reservations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsReservations :: Lens.Lens' ListReservationsResponse (Core.Maybe [Types.Reservation])
lrrrsReservations = Lens.field @"reservations"
{-# INLINEABLE lrrrsReservations #-}
{-# DEPRECATED reservations "Use generic-lens or generic-optics with 'reservations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsResponseStatus :: Lens.Lens' ListReservationsResponse Core.Int
lrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
