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
-- Module      : Amazonka.MediaLive.Types.ReservationResourceSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.ReservationResourceSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.ChannelClass
import Amazonka.MediaLive.Types.ReservationCodec
import Amazonka.MediaLive.Types.ReservationMaximumBitrate
import Amazonka.MediaLive.Types.ReservationMaximumFramerate
import Amazonka.MediaLive.Types.ReservationResolution
import Amazonka.MediaLive.Types.ReservationResourceType
import Amazonka.MediaLive.Types.ReservationSpecialFeature
import Amazonka.MediaLive.Types.ReservationVideoQuality
import qualified Amazonka.Prelude as Prelude

-- | Resource configuration (codec, resolution, bitrate, ...)
--
-- /See:/ 'newReservationResourceSpecification' smart constructor.
data ReservationResourceSpecification = ReservationResourceSpecification'
  { -- | Resource type, \'INPUT\', \'OUTPUT\', \'MULTIPLEX\', or \'CHANNEL\'
    resourceType :: Prelude.Maybe ReservationResourceType,
    -- | Channel class, e.g. \'STANDARD\'
    channelClass :: Prelude.Maybe ChannelClass,
    -- | Codec, e.g. \'AVC\'
    codec :: Prelude.Maybe ReservationCodec,
    -- | Maximum framerate, e.g. \'MAX_30_FPS\' (Outputs only)
    maximumFramerate :: Prelude.Maybe ReservationMaximumFramerate,
    -- | Video quality, e.g. \'STANDARD\' (Outputs only)
    videoQuality :: Prelude.Maybe ReservationVideoQuality,
    -- | Maximum bitrate, e.g. \'MAX_20_MBPS\'
    maximumBitrate :: Prelude.Maybe ReservationMaximumBitrate,
    -- | Special feature, e.g. \'AUDIO_NORMALIZATION\' (Channels only)
    specialFeature :: Prelude.Maybe ReservationSpecialFeature,
    -- | Resolution, e.g. \'HD\'
    resolution :: Prelude.Maybe ReservationResolution
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservationResourceSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'reservationResourceSpecification_resourceType' - Resource type, \'INPUT\', \'OUTPUT\', \'MULTIPLEX\', or \'CHANNEL\'
--
-- 'channelClass', 'reservationResourceSpecification_channelClass' - Channel class, e.g. \'STANDARD\'
--
-- 'codec', 'reservationResourceSpecification_codec' - Codec, e.g. \'AVC\'
--
-- 'maximumFramerate', 'reservationResourceSpecification_maximumFramerate' - Maximum framerate, e.g. \'MAX_30_FPS\' (Outputs only)
--
-- 'videoQuality', 'reservationResourceSpecification_videoQuality' - Video quality, e.g. \'STANDARD\' (Outputs only)
--
-- 'maximumBitrate', 'reservationResourceSpecification_maximumBitrate' - Maximum bitrate, e.g. \'MAX_20_MBPS\'
--
-- 'specialFeature', 'reservationResourceSpecification_specialFeature' - Special feature, e.g. \'AUDIO_NORMALIZATION\' (Channels only)
--
-- 'resolution', 'reservationResourceSpecification_resolution' - Resolution, e.g. \'HD\'
newReservationResourceSpecification ::
  ReservationResourceSpecification
newReservationResourceSpecification =
  ReservationResourceSpecification'
    { resourceType =
        Prelude.Nothing,
      channelClass = Prelude.Nothing,
      codec = Prelude.Nothing,
      maximumFramerate = Prelude.Nothing,
      videoQuality = Prelude.Nothing,
      maximumBitrate = Prelude.Nothing,
      specialFeature = Prelude.Nothing,
      resolution = Prelude.Nothing
    }

-- | Resource type, \'INPUT\', \'OUTPUT\', \'MULTIPLEX\', or \'CHANNEL\'
reservationResourceSpecification_resourceType :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ReservationResourceType)
reservationResourceSpecification_resourceType = Lens.lens (\ReservationResourceSpecification' {resourceType} -> resourceType) (\s@ReservationResourceSpecification' {} a -> s {resourceType = a} :: ReservationResourceSpecification)

-- | Channel class, e.g. \'STANDARD\'
reservationResourceSpecification_channelClass :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ChannelClass)
reservationResourceSpecification_channelClass = Lens.lens (\ReservationResourceSpecification' {channelClass} -> channelClass) (\s@ReservationResourceSpecification' {} a -> s {channelClass = a} :: ReservationResourceSpecification)

-- | Codec, e.g. \'AVC\'
reservationResourceSpecification_codec :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ReservationCodec)
reservationResourceSpecification_codec = Lens.lens (\ReservationResourceSpecification' {codec} -> codec) (\s@ReservationResourceSpecification' {} a -> s {codec = a} :: ReservationResourceSpecification)

-- | Maximum framerate, e.g. \'MAX_30_FPS\' (Outputs only)
reservationResourceSpecification_maximumFramerate :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ReservationMaximumFramerate)
reservationResourceSpecification_maximumFramerate = Lens.lens (\ReservationResourceSpecification' {maximumFramerate} -> maximumFramerate) (\s@ReservationResourceSpecification' {} a -> s {maximumFramerate = a} :: ReservationResourceSpecification)

-- | Video quality, e.g. \'STANDARD\' (Outputs only)
reservationResourceSpecification_videoQuality :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ReservationVideoQuality)
reservationResourceSpecification_videoQuality = Lens.lens (\ReservationResourceSpecification' {videoQuality} -> videoQuality) (\s@ReservationResourceSpecification' {} a -> s {videoQuality = a} :: ReservationResourceSpecification)

-- | Maximum bitrate, e.g. \'MAX_20_MBPS\'
reservationResourceSpecification_maximumBitrate :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ReservationMaximumBitrate)
reservationResourceSpecification_maximumBitrate = Lens.lens (\ReservationResourceSpecification' {maximumBitrate} -> maximumBitrate) (\s@ReservationResourceSpecification' {} a -> s {maximumBitrate = a} :: ReservationResourceSpecification)

-- | Special feature, e.g. \'AUDIO_NORMALIZATION\' (Channels only)
reservationResourceSpecification_specialFeature :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ReservationSpecialFeature)
reservationResourceSpecification_specialFeature = Lens.lens (\ReservationResourceSpecification' {specialFeature} -> specialFeature) (\s@ReservationResourceSpecification' {} a -> s {specialFeature = a} :: ReservationResourceSpecification)

-- | Resolution, e.g. \'HD\'
reservationResourceSpecification_resolution :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ReservationResolution)
reservationResourceSpecification_resolution = Lens.lens (\ReservationResourceSpecification' {resolution} -> resolution) (\s@ReservationResourceSpecification' {} a -> s {resolution = a} :: ReservationResourceSpecification)

instance
  Core.FromJSON
    ReservationResourceSpecification
  where
  parseJSON =
    Core.withObject
      "ReservationResourceSpecification"
      ( \x ->
          ReservationResourceSpecification'
            Prelude.<$> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "channelClass")
            Prelude.<*> (x Core..:? "codec")
            Prelude.<*> (x Core..:? "maximumFramerate")
            Prelude.<*> (x Core..:? "videoQuality")
            Prelude.<*> (x Core..:? "maximumBitrate")
            Prelude.<*> (x Core..:? "specialFeature")
            Prelude.<*> (x Core..:? "resolution")
      )

instance
  Prelude.Hashable
    ReservationResourceSpecification
  where
  hashWithSalt
    _salt
    ReservationResourceSpecification' {..} =
      _salt `Prelude.hashWithSalt` resourceType
        `Prelude.hashWithSalt` channelClass
        `Prelude.hashWithSalt` codec
        `Prelude.hashWithSalt` maximumFramerate
        `Prelude.hashWithSalt` videoQuality
        `Prelude.hashWithSalt` maximumBitrate
        `Prelude.hashWithSalt` specialFeature
        `Prelude.hashWithSalt` resolution

instance
  Prelude.NFData
    ReservationResourceSpecification
  where
  rnf ReservationResourceSpecification' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf channelClass
      `Prelude.seq` Prelude.rnf codec
      `Prelude.seq` Prelude.rnf maximumFramerate
      `Prelude.seq` Prelude.rnf videoQuality
      `Prelude.seq` Prelude.rnf maximumBitrate
      `Prelude.seq` Prelude.rnf specialFeature
      `Prelude.seq` Prelude.rnf resolution
