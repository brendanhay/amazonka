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
-- Module      : Amazonka.IVS.Types.VideoConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Types.VideoConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Object specifying a stream’s video configuration, as set up by the
-- broadcaster (usually in an encoder). This is part of the
-- IngestConfiguration object and used for monitoring stream health.
--
-- /See:/ 'newVideoConfiguration' smart constructor.
data VideoConfiguration = VideoConfiguration'
  { -- | Indicates the degree of required decoder performance for a profile.
    -- Normally this is set automatically by the encoder. For details, see the
    -- H.264 specification.
    avcLevel :: Prelude.Maybe Prelude.Text,
    -- | Indicates to the decoder the requirements for decoding the stream. For
    -- definitions of the valid values, see the H.264 specification.
    avcProfile :: Prelude.Maybe Prelude.Text,
    -- | Codec used for the video encoding.
    codec :: Prelude.Maybe Prelude.Text,
    -- | Software or hardware used to encode the video.
    encoder :: Prelude.Maybe Prelude.Text,
    -- | The expected ingest bitrate (bits per second). This is configured in the
    -- encoder.
    targetBitrate :: Prelude.Maybe Prelude.Integer,
    -- | The expected ingest framerate. This is configured in the encoder.
    targetFramerate :: Prelude.Maybe Prelude.Integer,
    -- | Video-resolution height in pixels.
    videoHeight :: Prelude.Maybe Prelude.Integer,
    -- | Video-resolution width in pixels.
    videoWidth :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VideoConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'avcLevel', 'videoConfiguration_avcLevel' - Indicates the degree of required decoder performance for a profile.
-- Normally this is set automatically by the encoder. For details, see the
-- H.264 specification.
--
-- 'avcProfile', 'videoConfiguration_avcProfile' - Indicates to the decoder the requirements for decoding the stream. For
-- definitions of the valid values, see the H.264 specification.
--
-- 'codec', 'videoConfiguration_codec' - Codec used for the video encoding.
--
-- 'encoder', 'videoConfiguration_encoder' - Software or hardware used to encode the video.
--
-- 'targetBitrate', 'videoConfiguration_targetBitrate' - The expected ingest bitrate (bits per second). This is configured in the
-- encoder.
--
-- 'targetFramerate', 'videoConfiguration_targetFramerate' - The expected ingest framerate. This is configured in the encoder.
--
-- 'videoHeight', 'videoConfiguration_videoHeight' - Video-resolution height in pixels.
--
-- 'videoWidth', 'videoConfiguration_videoWidth' - Video-resolution width in pixels.
newVideoConfiguration ::
  VideoConfiguration
newVideoConfiguration =
  VideoConfiguration'
    { avcLevel = Prelude.Nothing,
      avcProfile = Prelude.Nothing,
      codec = Prelude.Nothing,
      encoder = Prelude.Nothing,
      targetBitrate = Prelude.Nothing,
      targetFramerate = Prelude.Nothing,
      videoHeight = Prelude.Nothing,
      videoWidth = Prelude.Nothing
    }

-- | Indicates the degree of required decoder performance for a profile.
-- Normally this is set automatically by the encoder. For details, see the
-- H.264 specification.
videoConfiguration_avcLevel :: Lens.Lens' VideoConfiguration (Prelude.Maybe Prelude.Text)
videoConfiguration_avcLevel = Lens.lens (\VideoConfiguration' {avcLevel} -> avcLevel) (\s@VideoConfiguration' {} a -> s {avcLevel = a} :: VideoConfiguration)

-- | Indicates to the decoder the requirements for decoding the stream. For
-- definitions of the valid values, see the H.264 specification.
videoConfiguration_avcProfile :: Lens.Lens' VideoConfiguration (Prelude.Maybe Prelude.Text)
videoConfiguration_avcProfile = Lens.lens (\VideoConfiguration' {avcProfile} -> avcProfile) (\s@VideoConfiguration' {} a -> s {avcProfile = a} :: VideoConfiguration)

-- | Codec used for the video encoding.
videoConfiguration_codec :: Lens.Lens' VideoConfiguration (Prelude.Maybe Prelude.Text)
videoConfiguration_codec = Lens.lens (\VideoConfiguration' {codec} -> codec) (\s@VideoConfiguration' {} a -> s {codec = a} :: VideoConfiguration)

-- | Software or hardware used to encode the video.
videoConfiguration_encoder :: Lens.Lens' VideoConfiguration (Prelude.Maybe Prelude.Text)
videoConfiguration_encoder = Lens.lens (\VideoConfiguration' {encoder} -> encoder) (\s@VideoConfiguration' {} a -> s {encoder = a} :: VideoConfiguration)

-- | The expected ingest bitrate (bits per second). This is configured in the
-- encoder.
videoConfiguration_targetBitrate :: Lens.Lens' VideoConfiguration (Prelude.Maybe Prelude.Integer)
videoConfiguration_targetBitrate = Lens.lens (\VideoConfiguration' {targetBitrate} -> targetBitrate) (\s@VideoConfiguration' {} a -> s {targetBitrate = a} :: VideoConfiguration)

-- | The expected ingest framerate. This is configured in the encoder.
videoConfiguration_targetFramerate :: Lens.Lens' VideoConfiguration (Prelude.Maybe Prelude.Integer)
videoConfiguration_targetFramerate = Lens.lens (\VideoConfiguration' {targetFramerate} -> targetFramerate) (\s@VideoConfiguration' {} a -> s {targetFramerate = a} :: VideoConfiguration)

-- | Video-resolution height in pixels.
videoConfiguration_videoHeight :: Lens.Lens' VideoConfiguration (Prelude.Maybe Prelude.Integer)
videoConfiguration_videoHeight = Lens.lens (\VideoConfiguration' {videoHeight} -> videoHeight) (\s@VideoConfiguration' {} a -> s {videoHeight = a} :: VideoConfiguration)

-- | Video-resolution width in pixels.
videoConfiguration_videoWidth :: Lens.Lens' VideoConfiguration (Prelude.Maybe Prelude.Integer)
videoConfiguration_videoWidth = Lens.lens (\VideoConfiguration' {videoWidth} -> videoWidth) (\s@VideoConfiguration' {} a -> s {videoWidth = a} :: VideoConfiguration)

instance Data.FromJSON VideoConfiguration where
  parseJSON =
    Data.withObject
      "VideoConfiguration"
      ( \x ->
          VideoConfiguration'
            Prelude.<$> (x Data..:? "avcLevel")
            Prelude.<*> (x Data..:? "avcProfile")
            Prelude.<*> (x Data..:? "codec")
            Prelude.<*> (x Data..:? "encoder")
            Prelude.<*> (x Data..:? "targetBitrate")
            Prelude.<*> (x Data..:? "targetFramerate")
            Prelude.<*> (x Data..:? "videoHeight")
            Prelude.<*> (x Data..:? "videoWidth")
      )

instance Prelude.Hashable VideoConfiguration where
  hashWithSalt _salt VideoConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` avcLevel
      `Prelude.hashWithSalt` avcProfile
      `Prelude.hashWithSalt` codec
      `Prelude.hashWithSalt` encoder
      `Prelude.hashWithSalt` targetBitrate
      `Prelude.hashWithSalt` targetFramerate
      `Prelude.hashWithSalt` videoHeight
      `Prelude.hashWithSalt` videoWidth

instance Prelude.NFData VideoConfiguration where
  rnf VideoConfiguration' {..} =
    Prelude.rnf avcLevel
      `Prelude.seq` Prelude.rnf avcProfile
      `Prelude.seq` Prelude.rnf codec
      `Prelude.seq` Prelude.rnf encoder
      `Prelude.seq` Prelude.rnf targetBitrate
      `Prelude.seq` Prelude.rnf targetFramerate
      `Prelude.seq` Prelude.rnf videoHeight
      `Prelude.seq` Prelude.rnf videoWidth
