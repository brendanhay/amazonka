{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationResourceSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationResourceSpecification where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.ChannelClass
import Network.AWS.MediaLive.Types.ReservationCodec
import Network.AWS.MediaLive.Types.ReservationMaximumBitrate
import Network.AWS.MediaLive.Types.ReservationMaximumFramerate
import Network.AWS.MediaLive.Types.ReservationResolution
import Network.AWS.MediaLive.Types.ReservationResourceType
import Network.AWS.MediaLive.Types.ReservationSpecialFeature
import Network.AWS.MediaLive.Types.ReservationVideoQuality
import qualified Network.AWS.Prelude as Prelude

-- | Resource configuration (codec, resolution, bitrate, ...)
--
-- /See:/ 'newReservationResourceSpecification' smart constructor.
data ReservationResourceSpecification = ReservationResourceSpecification'
  { -- | Maximum framerate, e.g. \'MAX_30_FPS\' (Outputs only)
    maximumFramerate :: Prelude.Maybe ReservationMaximumFramerate,
    -- | Video quality, e.g. \'STANDARD\' (Outputs only)
    videoQuality :: Prelude.Maybe ReservationVideoQuality,
    -- | Codec, e.g. \'AVC\'
    codec :: Prelude.Maybe ReservationCodec,
    -- | Maximum bitrate, e.g. \'MAX_20_MBPS\'
    maximumBitrate :: Prelude.Maybe ReservationMaximumBitrate,
    -- | Special feature, e.g. \'AUDIO_NORMALIZATION\' (Channels only)
    specialFeature :: Prelude.Maybe ReservationSpecialFeature,
    -- | Channel class, e.g. \'STANDARD\'
    channelClass :: Prelude.Maybe ChannelClass,
    -- | Resource type, \'INPUT\', \'OUTPUT\', \'MULTIPLEX\', or \'CHANNEL\'
    resourceType :: Prelude.Maybe ReservationResourceType,
    -- | Resolution, e.g. \'HD\'
    resolution :: Prelude.Maybe ReservationResolution
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReservationResourceSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumFramerate', 'reservationResourceSpecification_maximumFramerate' - Maximum framerate, e.g. \'MAX_30_FPS\' (Outputs only)
--
-- 'videoQuality', 'reservationResourceSpecification_videoQuality' - Video quality, e.g. \'STANDARD\' (Outputs only)
--
-- 'codec', 'reservationResourceSpecification_codec' - Codec, e.g. \'AVC\'
--
-- 'maximumBitrate', 'reservationResourceSpecification_maximumBitrate' - Maximum bitrate, e.g. \'MAX_20_MBPS\'
--
-- 'specialFeature', 'reservationResourceSpecification_specialFeature' - Special feature, e.g. \'AUDIO_NORMALIZATION\' (Channels only)
--
-- 'channelClass', 'reservationResourceSpecification_channelClass' - Channel class, e.g. \'STANDARD\'
--
-- 'resourceType', 'reservationResourceSpecification_resourceType' - Resource type, \'INPUT\', \'OUTPUT\', \'MULTIPLEX\', or \'CHANNEL\'
--
-- 'resolution', 'reservationResourceSpecification_resolution' - Resolution, e.g. \'HD\'
newReservationResourceSpecification ::
  ReservationResourceSpecification
newReservationResourceSpecification =
  ReservationResourceSpecification'
    { maximumFramerate =
        Prelude.Nothing,
      videoQuality = Prelude.Nothing,
      codec = Prelude.Nothing,
      maximumBitrate = Prelude.Nothing,
      specialFeature = Prelude.Nothing,
      channelClass = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      resolution = Prelude.Nothing
    }

-- | Maximum framerate, e.g. \'MAX_30_FPS\' (Outputs only)
reservationResourceSpecification_maximumFramerate :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ReservationMaximumFramerate)
reservationResourceSpecification_maximumFramerate = Lens.lens (\ReservationResourceSpecification' {maximumFramerate} -> maximumFramerate) (\s@ReservationResourceSpecification' {} a -> s {maximumFramerate = a} :: ReservationResourceSpecification)

-- | Video quality, e.g. \'STANDARD\' (Outputs only)
reservationResourceSpecification_videoQuality :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ReservationVideoQuality)
reservationResourceSpecification_videoQuality = Lens.lens (\ReservationResourceSpecification' {videoQuality} -> videoQuality) (\s@ReservationResourceSpecification' {} a -> s {videoQuality = a} :: ReservationResourceSpecification)

-- | Codec, e.g. \'AVC\'
reservationResourceSpecification_codec :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ReservationCodec)
reservationResourceSpecification_codec = Lens.lens (\ReservationResourceSpecification' {codec} -> codec) (\s@ReservationResourceSpecification' {} a -> s {codec = a} :: ReservationResourceSpecification)

-- | Maximum bitrate, e.g. \'MAX_20_MBPS\'
reservationResourceSpecification_maximumBitrate :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ReservationMaximumBitrate)
reservationResourceSpecification_maximumBitrate = Lens.lens (\ReservationResourceSpecification' {maximumBitrate} -> maximumBitrate) (\s@ReservationResourceSpecification' {} a -> s {maximumBitrate = a} :: ReservationResourceSpecification)

-- | Special feature, e.g. \'AUDIO_NORMALIZATION\' (Channels only)
reservationResourceSpecification_specialFeature :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ReservationSpecialFeature)
reservationResourceSpecification_specialFeature = Lens.lens (\ReservationResourceSpecification' {specialFeature} -> specialFeature) (\s@ReservationResourceSpecification' {} a -> s {specialFeature = a} :: ReservationResourceSpecification)

-- | Channel class, e.g. \'STANDARD\'
reservationResourceSpecification_channelClass :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ChannelClass)
reservationResourceSpecification_channelClass = Lens.lens (\ReservationResourceSpecification' {channelClass} -> channelClass) (\s@ReservationResourceSpecification' {} a -> s {channelClass = a} :: ReservationResourceSpecification)

-- | Resource type, \'INPUT\', \'OUTPUT\', \'MULTIPLEX\', or \'CHANNEL\'
reservationResourceSpecification_resourceType :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ReservationResourceType)
reservationResourceSpecification_resourceType = Lens.lens (\ReservationResourceSpecification' {resourceType} -> resourceType) (\s@ReservationResourceSpecification' {} a -> s {resourceType = a} :: ReservationResourceSpecification)

-- | Resolution, e.g. \'HD\'
reservationResourceSpecification_resolution :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ReservationResolution)
reservationResourceSpecification_resolution = Lens.lens (\ReservationResourceSpecification' {resolution} -> resolution) (\s@ReservationResourceSpecification' {} a -> s {resolution = a} :: ReservationResourceSpecification)

instance
  Prelude.FromJSON
    ReservationResourceSpecification
  where
  parseJSON =
    Prelude.withObject
      "ReservationResourceSpecification"
      ( \x ->
          ReservationResourceSpecification'
            Prelude.<$> (x Prelude..:? "maximumFramerate")
            Prelude.<*> (x Prelude..:? "videoQuality")
            Prelude.<*> (x Prelude..:? "codec")
            Prelude.<*> (x Prelude..:? "maximumBitrate")
            Prelude.<*> (x Prelude..:? "specialFeature")
            Prelude.<*> (x Prelude..:? "channelClass")
            Prelude.<*> (x Prelude..:? "resourceType")
            Prelude.<*> (x Prelude..:? "resolution")
      )

instance
  Prelude.Hashable
    ReservationResourceSpecification

instance
  Prelude.NFData
    ReservationResourceSpecification
