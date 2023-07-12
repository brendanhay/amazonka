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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.ReservationResourceSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | Channel class, e.g. \'STANDARD\'
    channelClass :: Prelude.Maybe ChannelClass,
    -- | Codec, e.g. \'AVC\'
    codec :: Prelude.Maybe ReservationCodec,
    -- | Maximum bitrate, e.g. \'MAX_20_MBPS\'
    maximumBitrate :: Prelude.Maybe ReservationMaximumBitrate,
    -- | Maximum framerate, e.g. \'MAX_30_FPS\' (Outputs only)
    maximumFramerate :: Prelude.Maybe ReservationMaximumFramerate,
    -- | Resolution, e.g. \'HD\'
    resolution :: Prelude.Maybe ReservationResolution,
    -- | Resource type, \'INPUT\', \'OUTPUT\', \'MULTIPLEX\', or \'CHANNEL\'
    resourceType :: Prelude.Maybe ReservationResourceType,
    -- | Special feature, e.g. \'AUDIO_NORMALIZATION\' (Channels only)
    specialFeature :: Prelude.Maybe ReservationSpecialFeature,
    -- | Video quality, e.g. \'STANDARD\' (Outputs only)
    videoQuality :: Prelude.Maybe ReservationVideoQuality
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
-- 'channelClass', 'reservationResourceSpecification_channelClass' - Channel class, e.g. \'STANDARD\'
--
-- 'codec', 'reservationResourceSpecification_codec' - Codec, e.g. \'AVC\'
--
-- 'maximumBitrate', 'reservationResourceSpecification_maximumBitrate' - Maximum bitrate, e.g. \'MAX_20_MBPS\'
--
-- 'maximumFramerate', 'reservationResourceSpecification_maximumFramerate' - Maximum framerate, e.g. \'MAX_30_FPS\' (Outputs only)
--
-- 'resolution', 'reservationResourceSpecification_resolution' - Resolution, e.g. \'HD\'
--
-- 'resourceType', 'reservationResourceSpecification_resourceType' - Resource type, \'INPUT\', \'OUTPUT\', \'MULTIPLEX\', or \'CHANNEL\'
--
-- 'specialFeature', 'reservationResourceSpecification_specialFeature' - Special feature, e.g. \'AUDIO_NORMALIZATION\' (Channels only)
--
-- 'videoQuality', 'reservationResourceSpecification_videoQuality' - Video quality, e.g. \'STANDARD\' (Outputs only)
newReservationResourceSpecification ::
  ReservationResourceSpecification
newReservationResourceSpecification =
  ReservationResourceSpecification'
    { channelClass =
        Prelude.Nothing,
      codec = Prelude.Nothing,
      maximumBitrate = Prelude.Nothing,
      maximumFramerate = Prelude.Nothing,
      resolution = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      specialFeature = Prelude.Nothing,
      videoQuality = Prelude.Nothing
    }

-- | Channel class, e.g. \'STANDARD\'
reservationResourceSpecification_channelClass :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ChannelClass)
reservationResourceSpecification_channelClass = Lens.lens (\ReservationResourceSpecification' {channelClass} -> channelClass) (\s@ReservationResourceSpecification' {} a -> s {channelClass = a} :: ReservationResourceSpecification)

-- | Codec, e.g. \'AVC\'
reservationResourceSpecification_codec :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ReservationCodec)
reservationResourceSpecification_codec = Lens.lens (\ReservationResourceSpecification' {codec} -> codec) (\s@ReservationResourceSpecification' {} a -> s {codec = a} :: ReservationResourceSpecification)

-- | Maximum bitrate, e.g. \'MAX_20_MBPS\'
reservationResourceSpecification_maximumBitrate :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ReservationMaximumBitrate)
reservationResourceSpecification_maximumBitrate = Lens.lens (\ReservationResourceSpecification' {maximumBitrate} -> maximumBitrate) (\s@ReservationResourceSpecification' {} a -> s {maximumBitrate = a} :: ReservationResourceSpecification)

-- | Maximum framerate, e.g. \'MAX_30_FPS\' (Outputs only)
reservationResourceSpecification_maximumFramerate :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ReservationMaximumFramerate)
reservationResourceSpecification_maximumFramerate = Lens.lens (\ReservationResourceSpecification' {maximumFramerate} -> maximumFramerate) (\s@ReservationResourceSpecification' {} a -> s {maximumFramerate = a} :: ReservationResourceSpecification)

-- | Resolution, e.g. \'HD\'
reservationResourceSpecification_resolution :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ReservationResolution)
reservationResourceSpecification_resolution = Lens.lens (\ReservationResourceSpecification' {resolution} -> resolution) (\s@ReservationResourceSpecification' {} a -> s {resolution = a} :: ReservationResourceSpecification)

-- | Resource type, \'INPUT\', \'OUTPUT\', \'MULTIPLEX\', or \'CHANNEL\'
reservationResourceSpecification_resourceType :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ReservationResourceType)
reservationResourceSpecification_resourceType = Lens.lens (\ReservationResourceSpecification' {resourceType} -> resourceType) (\s@ReservationResourceSpecification' {} a -> s {resourceType = a} :: ReservationResourceSpecification)

-- | Special feature, e.g. \'AUDIO_NORMALIZATION\' (Channels only)
reservationResourceSpecification_specialFeature :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ReservationSpecialFeature)
reservationResourceSpecification_specialFeature = Lens.lens (\ReservationResourceSpecification' {specialFeature} -> specialFeature) (\s@ReservationResourceSpecification' {} a -> s {specialFeature = a} :: ReservationResourceSpecification)

-- | Video quality, e.g. \'STANDARD\' (Outputs only)
reservationResourceSpecification_videoQuality :: Lens.Lens' ReservationResourceSpecification (Prelude.Maybe ReservationVideoQuality)
reservationResourceSpecification_videoQuality = Lens.lens (\ReservationResourceSpecification' {videoQuality} -> videoQuality) (\s@ReservationResourceSpecification' {} a -> s {videoQuality = a} :: ReservationResourceSpecification)

instance
  Data.FromJSON
    ReservationResourceSpecification
  where
  parseJSON =
    Data.withObject
      "ReservationResourceSpecification"
      ( \x ->
          ReservationResourceSpecification'
            Prelude.<$> (x Data..:? "channelClass")
            Prelude.<*> (x Data..:? "codec")
            Prelude.<*> (x Data..:? "maximumBitrate")
            Prelude.<*> (x Data..:? "maximumFramerate")
            Prelude.<*> (x Data..:? "resolution")
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "specialFeature")
            Prelude.<*> (x Data..:? "videoQuality")
      )

instance
  Prelude.Hashable
    ReservationResourceSpecification
  where
  hashWithSalt
    _salt
    ReservationResourceSpecification' {..} =
      _salt
        `Prelude.hashWithSalt` channelClass
        `Prelude.hashWithSalt` codec
        `Prelude.hashWithSalt` maximumBitrate
        `Prelude.hashWithSalt` maximumFramerate
        `Prelude.hashWithSalt` resolution
        `Prelude.hashWithSalt` resourceType
        `Prelude.hashWithSalt` specialFeature
        `Prelude.hashWithSalt` videoQuality

instance
  Prelude.NFData
    ReservationResourceSpecification
  where
  rnf ReservationResourceSpecification' {..} =
    Prelude.rnf channelClass
      `Prelude.seq` Prelude.rnf codec
      `Prelude.seq` Prelude.rnf maximumBitrate
      `Prelude.seq` Prelude.rnf maximumFramerate
      `Prelude.seq` Prelude.rnf resolution
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf specialFeature
      `Prelude.seq` Prelude.rnf videoQuality
