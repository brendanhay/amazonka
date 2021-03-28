{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationResourceSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.ReservationResourceSpecification
  ( ReservationResourceSpecification (..)
  -- * Smart constructor
  , mkReservationResourceSpecification
  -- * Lenses
  , rrsChannelClass
  , rrsCodec
  , rrsMaximumBitrate
  , rrsMaximumFramerate
  , rrsResolution
  , rrsResourceType
  , rrsSpecialFeature
  , rrsVideoQuality
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.ChannelClass as Types
import qualified Network.AWS.MediaLive.Types.ReservationCodec as Types
import qualified Network.AWS.MediaLive.Types.ReservationMaximumBitrate as Types
import qualified Network.AWS.MediaLive.Types.ReservationMaximumFramerate as Types
import qualified Network.AWS.MediaLive.Types.ReservationResolution as Types
import qualified Network.AWS.MediaLive.Types.ReservationResourceType as Types
import qualified Network.AWS.MediaLive.Types.ReservationSpecialFeature as Types
import qualified Network.AWS.MediaLive.Types.ReservationVideoQuality as Types
import qualified Network.AWS.Prelude as Core

-- | Resource configuration (codec, resolution, bitrate, ...)
--
-- /See:/ 'mkReservationResourceSpecification' smart constructor.
data ReservationResourceSpecification = ReservationResourceSpecification'
  { channelClass :: Core.Maybe Types.ChannelClass
    -- ^ Channel class, e.g. 'STANDARD'
  , codec :: Core.Maybe Types.ReservationCodec
    -- ^ Codec, e.g. 'AVC'
  , maximumBitrate :: Core.Maybe Types.ReservationMaximumBitrate
    -- ^ Maximum bitrate, e.g. 'MAX_20_MBPS'
  , maximumFramerate :: Core.Maybe Types.ReservationMaximumFramerate
    -- ^ Maximum framerate, e.g. 'MAX_30_FPS' (Outputs only)
  , resolution :: Core.Maybe Types.ReservationResolution
    -- ^ Resolution, e.g. 'HD'
  , resourceType :: Core.Maybe Types.ReservationResourceType
    -- ^ Resource type, 'INPUT', 'OUTPUT', 'MULTIPLEX', or 'CHANNEL'
  , specialFeature :: Core.Maybe Types.ReservationSpecialFeature
    -- ^ Special feature, e.g. 'AUDIO_NORMALIZATION' (Channels only)
  , videoQuality :: Core.Maybe Types.ReservationVideoQuality
    -- ^ Video quality, e.g. 'STANDARD' (Outputs only)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReservationResourceSpecification' value with any optional fields omitted.
mkReservationResourceSpecification
    :: ReservationResourceSpecification
mkReservationResourceSpecification
  = ReservationResourceSpecification'{channelClass = Core.Nothing,
                                      codec = Core.Nothing, maximumBitrate = Core.Nothing,
                                      maximumFramerate = Core.Nothing, resolution = Core.Nothing,
                                      resourceType = Core.Nothing, specialFeature = Core.Nothing,
                                      videoQuality = Core.Nothing}

-- | Channel class, e.g. 'STANDARD'
--
-- /Note:/ Consider using 'channelClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsChannelClass :: Lens.Lens' ReservationResourceSpecification (Core.Maybe Types.ChannelClass)
rrsChannelClass = Lens.field @"channelClass"
{-# INLINEABLE rrsChannelClass #-}
{-# DEPRECATED channelClass "Use generic-lens or generic-optics with 'channelClass' instead"  #-}

-- | Codec, e.g. 'AVC'
--
-- /Note:/ Consider using 'codec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsCodec :: Lens.Lens' ReservationResourceSpecification (Core.Maybe Types.ReservationCodec)
rrsCodec = Lens.field @"codec"
{-# INLINEABLE rrsCodec #-}
{-# DEPRECATED codec "Use generic-lens or generic-optics with 'codec' instead"  #-}

-- | Maximum bitrate, e.g. 'MAX_20_MBPS'
--
-- /Note:/ Consider using 'maximumBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsMaximumBitrate :: Lens.Lens' ReservationResourceSpecification (Core.Maybe Types.ReservationMaximumBitrate)
rrsMaximumBitrate = Lens.field @"maximumBitrate"
{-# INLINEABLE rrsMaximumBitrate #-}
{-# DEPRECATED maximumBitrate "Use generic-lens or generic-optics with 'maximumBitrate' instead"  #-}

-- | Maximum framerate, e.g. 'MAX_30_FPS' (Outputs only)
--
-- /Note:/ Consider using 'maximumFramerate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsMaximumFramerate :: Lens.Lens' ReservationResourceSpecification (Core.Maybe Types.ReservationMaximumFramerate)
rrsMaximumFramerate = Lens.field @"maximumFramerate"
{-# INLINEABLE rrsMaximumFramerate #-}
{-# DEPRECATED maximumFramerate "Use generic-lens or generic-optics with 'maximumFramerate' instead"  #-}

-- | Resolution, e.g. 'HD'
--
-- /Note:/ Consider using 'resolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResolution :: Lens.Lens' ReservationResourceSpecification (Core.Maybe Types.ReservationResolution)
rrsResolution = Lens.field @"resolution"
{-# INLINEABLE rrsResolution #-}
{-# DEPRECATED resolution "Use generic-lens or generic-optics with 'resolution' instead"  #-}

-- | Resource type, 'INPUT', 'OUTPUT', 'MULTIPLEX', or 'CHANNEL'
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResourceType :: Lens.Lens' ReservationResourceSpecification (Core.Maybe Types.ReservationResourceType)
rrsResourceType = Lens.field @"resourceType"
{-# INLINEABLE rrsResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | Special feature, e.g. 'AUDIO_NORMALIZATION' (Channels only)
--
-- /Note:/ Consider using 'specialFeature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsSpecialFeature :: Lens.Lens' ReservationResourceSpecification (Core.Maybe Types.ReservationSpecialFeature)
rrsSpecialFeature = Lens.field @"specialFeature"
{-# INLINEABLE rrsSpecialFeature #-}
{-# DEPRECATED specialFeature "Use generic-lens or generic-optics with 'specialFeature' instead"  #-}

-- | Video quality, e.g. 'STANDARD' (Outputs only)
--
-- /Note:/ Consider using 'videoQuality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsVideoQuality :: Lens.Lens' ReservationResourceSpecification (Core.Maybe Types.ReservationVideoQuality)
rrsVideoQuality = Lens.field @"videoQuality"
{-# INLINEABLE rrsVideoQuality #-}
{-# DEPRECATED videoQuality "Use generic-lens or generic-optics with 'videoQuality' instead"  #-}

instance Core.FromJSON ReservationResourceSpecification where
        parseJSON
          = Core.withObject "ReservationResourceSpecification" Core.$
              \ x ->
                ReservationResourceSpecification' Core.<$>
                  (x Core..:? "channelClass") Core.<*> x Core..:? "codec" Core.<*>
                    x Core..:? "maximumBitrate"
                    Core.<*> x Core..:? "maximumFramerate"
                    Core.<*> x Core..:? "resolution"
                    Core.<*> x Core..:? "resourceType"
                    Core.<*> x Core..:? "specialFeature"
                    Core.<*> x Core..:? "videoQuality"
