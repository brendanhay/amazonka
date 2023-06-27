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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.KinesisVideoStreamSourceRuntimeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.KinesisVideoStreamSourceRuntimeConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.MediaEncoding
import Amazonka.ChimeSdkMediaPipelines.Types.StreamConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The runtime configuration settings for the Kinesis video stream source.
--
-- /See:/ 'newKinesisVideoStreamSourceRuntimeConfiguration' smart constructor.
data KinesisVideoStreamSourceRuntimeConfiguration = KinesisVideoStreamSourceRuntimeConfiguration'
  { -- | The streams in the source runtime configuration of a Kinesis video
    -- stream.
    streams :: Prelude.NonEmpty StreamConfiguration,
    -- | Specifies the encoding of your input audio. Supported format: PCM (only
    -- signed 16-bit little-endian audio formats, which does not include WAV)
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/how-input.html#how-input-audio Media formats>
    -- in the /Amazon Transcribe Developer Guide/.
    mediaEncoding :: MediaEncoding,
    -- | The sample rate of the input audio (in hertz). Low-quality audio, such
    -- as telephone audio, is typically around 8,000 Hz. High-quality audio
    -- typically ranges from 16,000 Hz to 48,000 Hz. Note that the sample rate
    -- you specify must match that of your audio.
    --
    -- Valid Range: Minimum value of 8000. Maximum value of 48000.
    mediaSampleRate :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisVideoStreamSourceRuntimeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streams', 'kinesisVideoStreamSourceRuntimeConfiguration_streams' - The streams in the source runtime configuration of a Kinesis video
-- stream.
--
-- 'mediaEncoding', 'kinesisVideoStreamSourceRuntimeConfiguration_mediaEncoding' - Specifies the encoding of your input audio. Supported format: PCM (only
-- signed 16-bit little-endian audio formats, which does not include WAV)
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/how-input.html#how-input-audio Media formats>
-- in the /Amazon Transcribe Developer Guide/.
--
-- 'mediaSampleRate', 'kinesisVideoStreamSourceRuntimeConfiguration_mediaSampleRate' - The sample rate of the input audio (in hertz). Low-quality audio, such
-- as telephone audio, is typically around 8,000 Hz. High-quality audio
-- typically ranges from 16,000 Hz to 48,000 Hz. Note that the sample rate
-- you specify must match that of your audio.
--
-- Valid Range: Minimum value of 8000. Maximum value of 48000.
newKinesisVideoStreamSourceRuntimeConfiguration ::
  -- | 'streams'
  Prelude.NonEmpty StreamConfiguration ->
  -- | 'mediaEncoding'
  MediaEncoding ->
  -- | 'mediaSampleRate'
  Prelude.Natural ->
  KinesisVideoStreamSourceRuntimeConfiguration
newKinesisVideoStreamSourceRuntimeConfiguration
  pStreams_
  pMediaEncoding_
  pMediaSampleRate_ =
    KinesisVideoStreamSourceRuntimeConfiguration'
      { streams =
          Lens.coerced
            Lens.# pStreams_,
        mediaEncoding =
          pMediaEncoding_,
        mediaSampleRate =
          pMediaSampleRate_
      }

-- | The streams in the source runtime configuration of a Kinesis video
-- stream.
kinesisVideoStreamSourceRuntimeConfiguration_streams :: Lens.Lens' KinesisVideoStreamSourceRuntimeConfiguration (Prelude.NonEmpty StreamConfiguration)
kinesisVideoStreamSourceRuntimeConfiguration_streams = Lens.lens (\KinesisVideoStreamSourceRuntimeConfiguration' {streams} -> streams) (\s@KinesisVideoStreamSourceRuntimeConfiguration' {} a -> s {streams = a} :: KinesisVideoStreamSourceRuntimeConfiguration) Prelude.. Lens.coerced

-- | Specifies the encoding of your input audio. Supported format: PCM (only
-- signed 16-bit little-endian audio formats, which does not include WAV)
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/how-input.html#how-input-audio Media formats>
-- in the /Amazon Transcribe Developer Guide/.
kinesisVideoStreamSourceRuntimeConfiguration_mediaEncoding :: Lens.Lens' KinesisVideoStreamSourceRuntimeConfiguration MediaEncoding
kinesisVideoStreamSourceRuntimeConfiguration_mediaEncoding = Lens.lens (\KinesisVideoStreamSourceRuntimeConfiguration' {mediaEncoding} -> mediaEncoding) (\s@KinesisVideoStreamSourceRuntimeConfiguration' {} a -> s {mediaEncoding = a} :: KinesisVideoStreamSourceRuntimeConfiguration)

-- | The sample rate of the input audio (in hertz). Low-quality audio, such
-- as telephone audio, is typically around 8,000 Hz. High-quality audio
-- typically ranges from 16,000 Hz to 48,000 Hz. Note that the sample rate
-- you specify must match that of your audio.
--
-- Valid Range: Minimum value of 8000. Maximum value of 48000.
kinesisVideoStreamSourceRuntimeConfiguration_mediaSampleRate :: Lens.Lens' KinesisVideoStreamSourceRuntimeConfiguration Prelude.Natural
kinesisVideoStreamSourceRuntimeConfiguration_mediaSampleRate = Lens.lens (\KinesisVideoStreamSourceRuntimeConfiguration' {mediaSampleRate} -> mediaSampleRate) (\s@KinesisVideoStreamSourceRuntimeConfiguration' {} a -> s {mediaSampleRate = a} :: KinesisVideoStreamSourceRuntimeConfiguration)

instance
  Data.FromJSON
    KinesisVideoStreamSourceRuntimeConfiguration
  where
  parseJSON =
    Data.withObject
      "KinesisVideoStreamSourceRuntimeConfiguration"
      ( \x ->
          KinesisVideoStreamSourceRuntimeConfiguration'
            Prelude.<$> (x Data..: "Streams")
            Prelude.<*> (x Data..: "MediaEncoding")
            Prelude.<*> (x Data..: "MediaSampleRate")
      )

instance
  Prelude.Hashable
    KinesisVideoStreamSourceRuntimeConfiguration
  where
  hashWithSalt
    _salt
    KinesisVideoStreamSourceRuntimeConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` streams
        `Prelude.hashWithSalt` mediaEncoding
        `Prelude.hashWithSalt` mediaSampleRate

instance
  Prelude.NFData
    KinesisVideoStreamSourceRuntimeConfiguration
  where
  rnf KinesisVideoStreamSourceRuntimeConfiguration' {..} =
    Prelude.rnf streams
      `Prelude.seq` Prelude.rnf mediaEncoding
      `Prelude.seq` Prelude.rnf mediaSampleRate

instance
  Data.ToJSON
    KinesisVideoStreamSourceRuntimeConfiguration
  where
  toJSON
    KinesisVideoStreamSourceRuntimeConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just ("Streams" Data..= streams),
              Prelude.Just ("MediaEncoding" Data..= mediaEncoding),
              Prelude.Just
                ("MediaSampleRate" Data..= mediaSampleRate)
            ]
        )
