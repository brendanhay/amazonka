{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.ListOfferings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List offerings available for purchase.
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListOfferings
    (
    -- * Creating a request
      ListOfferings (..)
    , mkListOfferings
    -- ** Request lenses
    , loChannelClass
    , loChannelConfiguration
    , loCodec
    , loDuration
    , loMaxResults
    , loMaximumBitrate
    , loMaximumFramerate
    , loNextToken
    , loResolution
    , loResourceType
    , loSpecialFeature
    , loVideoQuality

    -- * Destructuring the response
    , ListOfferingsResponse (..)
    , mkListOfferingsResponse
    -- ** Response lenses
    , lorrsNextToken
    , lorrsOfferings
    , lorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for ListOfferingsRequest
--
-- /See:/ 'mkListOfferings' smart constructor.
data ListOfferings = ListOfferings'
  { channelClass :: Core.Maybe Core.Text
    -- ^ Filter by channel class, 'STANDARD' or 'SINGLE_PIPELINE'
  , channelConfiguration :: Core.Maybe Core.Text
    -- ^ Filter to offerings that match the configuration of an existing channel, e.g. '2345678' (a channel ID)
  , codec :: Core.Maybe Core.Text
    -- ^ Filter by codec, 'AVC', 'HEVC', 'MPEG2', 'AUDIO', or 'LINK'
  , duration :: Core.Maybe Core.Text
    -- ^ Filter by offering duration, e.g. '12'
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

-- | Creates a 'ListOfferings' value with any optional fields omitted.
mkListOfferings
    :: ListOfferings
mkListOfferings
  = ListOfferings'{channelClass = Core.Nothing,
                   channelConfiguration = Core.Nothing, codec = Core.Nothing,
                   duration = Core.Nothing, maxResults = Core.Nothing,
                   maximumBitrate = Core.Nothing, maximumFramerate = Core.Nothing,
                   nextToken = Core.Nothing, resolution = Core.Nothing,
                   resourceType = Core.Nothing, specialFeature = Core.Nothing,
                   videoQuality = Core.Nothing}

-- | Filter by channel class, 'STANDARD' or 'SINGLE_PIPELINE'
--
-- /Note:/ Consider using 'channelClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loChannelClass :: Lens.Lens' ListOfferings (Core.Maybe Core.Text)
loChannelClass = Lens.field @"channelClass"
{-# INLINEABLE loChannelClass #-}
{-# DEPRECATED channelClass "Use generic-lens or generic-optics with 'channelClass' instead"  #-}

-- | Filter to offerings that match the configuration of an existing channel, e.g. '2345678' (a channel ID)
--
-- /Note:/ Consider using 'channelConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loChannelConfiguration :: Lens.Lens' ListOfferings (Core.Maybe Core.Text)
loChannelConfiguration = Lens.field @"channelConfiguration"
{-# INLINEABLE loChannelConfiguration #-}
{-# DEPRECATED channelConfiguration "Use generic-lens or generic-optics with 'channelConfiguration' instead"  #-}

-- | Filter by codec, 'AVC', 'HEVC', 'MPEG2', 'AUDIO', or 'LINK'
--
-- /Note:/ Consider using 'codec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loCodec :: Lens.Lens' ListOfferings (Core.Maybe Core.Text)
loCodec = Lens.field @"codec"
{-# INLINEABLE loCodec #-}
{-# DEPRECATED codec "Use generic-lens or generic-optics with 'codec' instead"  #-}

-- | Filter by offering duration, e.g. '12'
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loDuration :: Lens.Lens' ListOfferings (Core.Maybe Core.Text)
loDuration = Lens.field @"duration"
{-# INLINEABLE loDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loMaxResults :: Lens.Lens' ListOfferings (Core.Maybe Core.Natural)
loMaxResults = Lens.field @"maxResults"
{-# INLINEABLE loMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Filter by bitrate, 'MAX_10_MBPS', 'MAX_20_MBPS', or 'MAX_50_MBPS'
--
-- /Note:/ Consider using 'maximumBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loMaximumBitrate :: Lens.Lens' ListOfferings (Core.Maybe Core.Text)
loMaximumBitrate = Lens.field @"maximumBitrate"
{-# INLINEABLE loMaximumBitrate #-}
{-# DEPRECATED maximumBitrate "Use generic-lens or generic-optics with 'maximumBitrate' instead"  #-}

-- | Filter by framerate, 'MAX_30_FPS' or 'MAX_60_FPS'
--
-- /Note:/ Consider using 'maximumFramerate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loMaximumFramerate :: Lens.Lens' ListOfferings (Core.Maybe Core.Text)
loMaximumFramerate = Lens.field @"maximumFramerate"
{-# INLINEABLE loMaximumFramerate #-}
{-# DEPRECATED maximumFramerate "Use generic-lens or generic-optics with 'maximumFramerate' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loNextToken :: Lens.Lens' ListOfferings (Core.Maybe Core.Text)
loNextToken = Lens.field @"nextToken"
{-# INLINEABLE loNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Filter by resolution, 'SD', 'HD', 'FHD', or 'UHD'
--
-- /Note:/ Consider using 'resolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loResolution :: Lens.Lens' ListOfferings (Core.Maybe Core.Text)
loResolution = Lens.field @"resolution"
{-# INLINEABLE loResolution #-}
{-# DEPRECATED resolution "Use generic-lens or generic-optics with 'resolution' instead"  #-}

-- | Filter by resource type, 'INPUT', 'OUTPUT', 'MULTIPLEX', or 'CHANNEL'
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loResourceType :: Lens.Lens' ListOfferings (Core.Maybe Core.Text)
loResourceType = Lens.field @"resourceType"
{-# INLINEABLE loResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | Filter by special feature, 'ADVANCED_AUDIO' or 'AUDIO_NORMALIZATION'
--
-- /Note:/ Consider using 'specialFeature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loSpecialFeature :: Lens.Lens' ListOfferings (Core.Maybe Core.Text)
loSpecialFeature = Lens.field @"specialFeature"
{-# INLINEABLE loSpecialFeature #-}
{-# DEPRECATED specialFeature "Use generic-lens or generic-optics with 'specialFeature' instead"  #-}

-- | Filter by video quality, 'STANDARD', 'ENHANCED', or 'PREMIUM'
--
-- /Note:/ Consider using 'videoQuality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loVideoQuality :: Lens.Lens' ListOfferings (Core.Maybe Core.Text)
loVideoQuality = Lens.field @"videoQuality"
{-# INLINEABLE loVideoQuality #-}
{-# DEPRECATED videoQuality "Use generic-lens or generic-optics with 'videoQuality' instead"  #-}

instance Core.ToQuery ListOfferings where
        toQuery ListOfferings{..}
          = Core.maybe Core.mempty (Core.toQueryPair "channelClass")
              channelClass
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "channelConfiguration")
                channelConfiguration
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "codec") codec
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "duration") duration
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

instance Core.ToHeaders ListOfferings where
        toHeaders ListOfferings{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListOfferings where
        type Rs ListOfferings = ListOfferingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/prod/offerings",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListOfferingsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "offerings" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListOfferings where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"offerings" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Placeholder documentation for ListOfferingsResponse
--
-- /See:/ 'mkListOfferingsResponse' smart constructor.
data ListOfferingsResponse = ListOfferingsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ Token to retrieve the next page of results
  , offerings :: Core.Maybe [Types.Offering]
    -- ^ List of offerings
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListOfferingsResponse' value with any optional fields omitted.
mkListOfferingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListOfferingsResponse
mkListOfferingsResponse responseStatus
  = ListOfferingsResponse'{nextToken = Core.Nothing,
                           offerings = Core.Nothing, responseStatus}

-- | Token to retrieve the next page of results
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsNextToken :: Lens.Lens' ListOfferingsResponse (Core.Maybe Core.Text)
lorrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lorrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | List of offerings
--
-- /Note:/ Consider using 'offerings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsOfferings :: Lens.Lens' ListOfferingsResponse (Core.Maybe [Types.Offering])
lorrsOfferings = Lens.field @"offerings"
{-# INLINEABLE lorrsOfferings #-}
{-# DEPRECATED offerings "Use generic-lens or generic-optics with 'offerings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsResponseStatus :: Lens.Lens' ListOfferingsResponse Core.Int
lorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
