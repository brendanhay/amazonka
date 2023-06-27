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
-- Module      : Amazonka.Rekognition.Types.AudioMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.AudioMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Metadata information about an audio stream. An array of @AudioMetadata@
-- objects for the audio streams found in a stored video is returned by
-- GetSegmentDetection.
--
-- /See:/ 'newAudioMetadata' smart constructor.
data AudioMetadata = AudioMetadata'
  { -- | The audio codec used to encode or decode the audio stream.
    codec :: Prelude.Maybe Prelude.Text,
    -- | The duration of the audio stream in milliseconds.
    durationMillis :: Prelude.Maybe Prelude.Natural,
    -- | The number of audio channels in the segment.
    numberOfChannels :: Prelude.Maybe Prelude.Natural,
    -- | The sample rate for the audio stream.
    sampleRate :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AudioMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codec', 'audioMetadata_codec' - The audio codec used to encode or decode the audio stream.
--
-- 'durationMillis', 'audioMetadata_durationMillis' - The duration of the audio stream in milliseconds.
--
-- 'numberOfChannels', 'audioMetadata_numberOfChannels' - The number of audio channels in the segment.
--
-- 'sampleRate', 'audioMetadata_sampleRate' - The sample rate for the audio stream.
newAudioMetadata ::
  AudioMetadata
newAudioMetadata =
  AudioMetadata'
    { codec = Prelude.Nothing,
      durationMillis = Prelude.Nothing,
      numberOfChannels = Prelude.Nothing,
      sampleRate = Prelude.Nothing
    }

-- | The audio codec used to encode or decode the audio stream.
audioMetadata_codec :: Lens.Lens' AudioMetadata (Prelude.Maybe Prelude.Text)
audioMetadata_codec = Lens.lens (\AudioMetadata' {codec} -> codec) (\s@AudioMetadata' {} a -> s {codec = a} :: AudioMetadata)

-- | The duration of the audio stream in milliseconds.
audioMetadata_durationMillis :: Lens.Lens' AudioMetadata (Prelude.Maybe Prelude.Natural)
audioMetadata_durationMillis = Lens.lens (\AudioMetadata' {durationMillis} -> durationMillis) (\s@AudioMetadata' {} a -> s {durationMillis = a} :: AudioMetadata)

-- | The number of audio channels in the segment.
audioMetadata_numberOfChannels :: Lens.Lens' AudioMetadata (Prelude.Maybe Prelude.Natural)
audioMetadata_numberOfChannels = Lens.lens (\AudioMetadata' {numberOfChannels} -> numberOfChannels) (\s@AudioMetadata' {} a -> s {numberOfChannels = a} :: AudioMetadata)

-- | The sample rate for the audio stream.
audioMetadata_sampleRate :: Lens.Lens' AudioMetadata (Prelude.Maybe Prelude.Natural)
audioMetadata_sampleRate = Lens.lens (\AudioMetadata' {sampleRate} -> sampleRate) (\s@AudioMetadata' {} a -> s {sampleRate = a} :: AudioMetadata)

instance Data.FromJSON AudioMetadata where
  parseJSON =
    Data.withObject
      "AudioMetadata"
      ( \x ->
          AudioMetadata'
            Prelude.<$> (x Data..:? "Codec")
            Prelude.<*> (x Data..:? "DurationMillis")
            Prelude.<*> (x Data..:? "NumberOfChannels")
            Prelude.<*> (x Data..:? "SampleRate")
      )

instance Prelude.Hashable AudioMetadata where
  hashWithSalt _salt AudioMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` codec
      `Prelude.hashWithSalt` durationMillis
      `Prelude.hashWithSalt` numberOfChannels
      `Prelude.hashWithSalt` sampleRate

instance Prelude.NFData AudioMetadata where
  rnf AudioMetadata' {..} =
    Prelude.rnf codec
      `Prelude.seq` Prelude.rnf durationMillis
      `Prelude.seq` Prelude.rnf numberOfChannels
      `Prelude.seq` Prelude.rnf sampleRate
