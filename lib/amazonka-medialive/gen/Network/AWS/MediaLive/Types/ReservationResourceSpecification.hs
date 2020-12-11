-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationResourceSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationResourceSpecification
  ( ReservationResourceSpecification (..),

    -- * Smart constructor
    mkReservationResourceSpecification,

    -- * Lenses
    rrsVideoQuality,
    rrsMaximumFramerate,
    rrsResourceType,
    rrsResolution,
    rrsCodec,
    rrsSpecialFeature,
    rrsChannelClass,
    rrsMaximumBitrate,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.ChannelClass
import Network.AWS.MediaLive.Types.ReservationCodec
import Network.AWS.MediaLive.Types.ReservationMaximumBitrate
import Network.AWS.MediaLive.Types.ReservationMaximumFramerate
import Network.AWS.MediaLive.Types.ReservationResolution
import Network.AWS.MediaLive.Types.ReservationResourceType
import Network.AWS.MediaLive.Types.ReservationSpecialFeature
import Network.AWS.MediaLive.Types.ReservationVideoQuality
import qualified Network.AWS.Prelude as Lude

-- | Resource configuration (codec, resolution, bitrate, ...)
--
-- /See:/ 'mkReservationResourceSpecification' smart constructor.
data ReservationResourceSpecification = ReservationResourceSpecification'
  { videoQuality ::
      Lude.Maybe
        ReservationVideoQuality,
    maximumFramerate ::
      Lude.Maybe
        ReservationMaximumFramerate,
    resourceType ::
      Lude.Maybe
        ReservationResourceType,
    resolution ::
      Lude.Maybe
        ReservationResolution,
    codec ::
      Lude.Maybe
        ReservationCodec,
    specialFeature ::
      Lude.Maybe
        ReservationSpecialFeature,
    channelClass ::
      Lude.Maybe ChannelClass,
    maximumBitrate ::
      Lude.Maybe
        ReservationMaximumBitrate
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservationResourceSpecification' with the minimum fields required to make a request.
--
-- * 'channelClass' - Channel class, e.g. 'STANDARD'
-- * 'codec' - Codec, e.g. 'AVC'
-- * 'maximumBitrate' - Maximum bitrate, e.g. 'MAX_20_MBPS'
-- * 'maximumFramerate' - Maximum framerate, e.g. 'MAX_30_FPS' (Outputs only)
-- * 'resolution' - Resolution, e.g. 'HD'
-- * 'resourceType' - Resource type, 'INPUT', 'OUTPUT', 'MULTIPLEX', or 'CHANNEL'
-- * 'specialFeature' - Special feature, e.g. 'AUDIO_NORMALIZATION' (Channels only)
-- * 'videoQuality' - Video quality, e.g. 'STANDARD' (Outputs only)
mkReservationResourceSpecification ::
  ReservationResourceSpecification
mkReservationResourceSpecification =
  ReservationResourceSpecification'
    { videoQuality = Lude.Nothing,
      maximumFramerate = Lude.Nothing,
      resourceType = Lude.Nothing,
      resolution = Lude.Nothing,
      codec = Lude.Nothing,
      specialFeature = Lude.Nothing,
      channelClass = Lude.Nothing,
      maximumBitrate = Lude.Nothing
    }

-- | Video quality, e.g. 'STANDARD' (Outputs only)
--
-- /Note:/ Consider using 'videoQuality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsVideoQuality :: Lens.Lens' ReservationResourceSpecification (Lude.Maybe ReservationVideoQuality)
rrsVideoQuality = Lens.lens (videoQuality :: ReservationResourceSpecification -> Lude.Maybe ReservationVideoQuality) (\s a -> s {videoQuality = a} :: ReservationResourceSpecification)
{-# DEPRECATED rrsVideoQuality "Use generic-lens or generic-optics with 'videoQuality' instead." #-}

-- | Maximum framerate, e.g. 'MAX_30_FPS' (Outputs only)
--
-- /Note:/ Consider using 'maximumFramerate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsMaximumFramerate :: Lens.Lens' ReservationResourceSpecification (Lude.Maybe ReservationMaximumFramerate)
rrsMaximumFramerate = Lens.lens (maximumFramerate :: ReservationResourceSpecification -> Lude.Maybe ReservationMaximumFramerate) (\s a -> s {maximumFramerate = a} :: ReservationResourceSpecification)
{-# DEPRECATED rrsMaximumFramerate "Use generic-lens or generic-optics with 'maximumFramerate' instead." #-}

-- | Resource type, 'INPUT', 'OUTPUT', 'MULTIPLEX', or 'CHANNEL'
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResourceType :: Lens.Lens' ReservationResourceSpecification (Lude.Maybe ReservationResourceType)
rrsResourceType = Lens.lens (resourceType :: ReservationResourceSpecification -> Lude.Maybe ReservationResourceType) (\s a -> s {resourceType = a} :: ReservationResourceSpecification)
{-# DEPRECATED rrsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Resolution, e.g. 'HD'
--
-- /Note:/ Consider using 'resolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResolution :: Lens.Lens' ReservationResourceSpecification (Lude.Maybe ReservationResolution)
rrsResolution = Lens.lens (resolution :: ReservationResourceSpecification -> Lude.Maybe ReservationResolution) (\s a -> s {resolution = a} :: ReservationResourceSpecification)
{-# DEPRECATED rrsResolution "Use generic-lens or generic-optics with 'resolution' instead." #-}

-- | Codec, e.g. 'AVC'
--
-- /Note:/ Consider using 'codec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsCodec :: Lens.Lens' ReservationResourceSpecification (Lude.Maybe ReservationCodec)
rrsCodec = Lens.lens (codec :: ReservationResourceSpecification -> Lude.Maybe ReservationCodec) (\s a -> s {codec = a} :: ReservationResourceSpecification)
{-# DEPRECATED rrsCodec "Use generic-lens or generic-optics with 'codec' instead." #-}

-- | Special feature, e.g. 'AUDIO_NORMALIZATION' (Channels only)
--
-- /Note:/ Consider using 'specialFeature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsSpecialFeature :: Lens.Lens' ReservationResourceSpecification (Lude.Maybe ReservationSpecialFeature)
rrsSpecialFeature = Lens.lens (specialFeature :: ReservationResourceSpecification -> Lude.Maybe ReservationSpecialFeature) (\s a -> s {specialFeature = a} :: ReservationResourceSpecification)
{-# DEPRECATED rrsSpecialFeature "Use generic-lens or generic-optics with 'specialFeature' instead." #-}

-- | Channel class, e.g. 'STANDARD'
--
-- /Note:/ Consider using 'channelClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsChannelClass :: Lens.Lens' ReservationResourceSpecification (Lude.Maybe ChannelClass)
rrsChannelClass = Lens.lens (channelClass :: ReservationResourceSpecification -> Lude.Maybe ChannelClass) (\s a -> s {channelClass = a} :: ReservationResourceSpecification)
{-# DEPRECATED rrsChannelClass "Use generic-lens or generic-optics with 'channelClass' instead." #-}

-- | Maximum bitrate, e.g. 'MAX_20_MBPS'
--
-- /Note:/ Consider using 'maximumBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsMaximumBitrate :: Lens.Lens' ReservationResourceSpecification (Lude.Maybe ReservationMaximumBitrate)
rrsMaximumBitrate = Lens.lens (maximumBitrate :: ReservationResourceSpecification -> Lude.Maybe ReservationMaximumBitrate) (\s a -> s {maximumBitrate = a} :: ReservationResourceSpecification)
{-# DEPRECATED rrsMaximumBitrate "Use generic-lens or generic-optics with 'maximumBitrate' instead." #-}

instance Lude.FromJSON ReservationResourceSpecification where
  parseJSON =
    Lude.withObject
      "ReservationResourceSpecification"
      ( \x ->
          ReservationResourceSpecification'
            Lude.<$> (x Lude..:? "videoQuality")
            Lude.<*> (x Lude..:? "maximumFramerate")
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "resolution")
            Lude.<*> (x Lude..:? "codec")
            Lude.<*> (x Lude..:? "specialFeature")
            Lude.<*> (x Lude..:? "channelClass")
            Lude.<*> (x Lude..:? "maximumBitrate")
      )
