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
-- Module      : Network.AWS.Rekognition.Types.AudioMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.AudioMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Metadata information about an audio stream. An array of @AudioMetadata@
-- objects for the audio streams found in a stored video is returned by
-- GetSegmentDetection.
--
-- /See:/ 'newAudioMetadata' smart constructor.
data AudioMetadata = AudioMetadata'
  { -- | The audio codec used to encode or decode the audio stream.
    codec :: Prelude.Maybe Prelude.Text,
    -- | The sample rate for the audio stream.
    sampleRate :: Prelude.Maybe Prelude.Natural,
    -- | The duration of the audio stream in milliseconds.
    durationMillis :: Prelude.Maybe Prelude.Natural,
    -- | The number of audio channels in the segment.
    numberOfChannels :: Prelude.Maybe Prelude.Natural
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
-- 'sampleRate', 'audioMetadata_sampleRate' - The sample rate for the audio stream.
--
-- 'durationMillis', 'audioMetadata_durationMillis' - The duration of the audio stream in milliseconds.
--
-- 'numberOfChannels', 'audioMetadata_numberOfChannels' - The number of audio channels in the segment.
newAudioMetadata ::
  AudioMetadata
newAudioMetadata =
  AudioMetadata'
    { codec = Prelude.Nothing,
      sampleRate = Prelude.Nothing,
      durationMillis = Prelude.Nothing,
      numberOfChannels = Prelude.Nothing
    }

-- | The audio codec used to encode or decode the audio stream.
audioMetadata_codec :: Lens.Lens' AudioMetadata (Prelude.Maybe Prelude.Text)
audioMetadata_codec = Lens.lens (\AudioMetadata' {codec} -> codec) (\s@AudioMetadata' {} a -> s {codec = a} :: AudioMetadata)

-- | The sample rate for the audio stream.
audioMetadata_sampleRate :: Lens.Lens' AudioMetadata (Prelude.Maybe Prelude.Natural)
audioMetadata_sampleRate = Lens.lens (\AudioMetadata' {sampleRate} -> sampleRate) (\s@AudioMetadata' {} a -> s {sampleRate = a} :: AudioMetadata)

-- | The duration of the audio stream in milliseconds.
audioMetadata_durationMillis :: Lens.Lens' AudioMetadata (Prelude.Maybe Prelude.Natural)
audioMetadata_durationMillis = Lens.lens (\AudioMetadata' {durationMillis} -> durationMillis) (\s@AudioMetadata' {} a -> s {durationMillis = a} :: AudioMetadata)

-- | The number of audio channels in the segment.
audioMetadata_numberOfChannels :: Lens.Lens' AudioMetadata (Prelude.Maybe Prelude.Natural)
audioMetadata_numberOfChannels = Lens.lens (\AudioMetadata' {numberOfChannels} -> numberOfChannels) (\s@AudioMetadata' {} a -> s {numberOfChannels = a} :: AudioMetadata)

instance Core.FromJSON AudioMetadata where
  parseJSON =
    Core.withObject
      "AudioMetadata"
      ( \x ->
          AudioMetadata'
            Prelude.<$> (x Core..:? "Codec")
            Prelude.<*> (x Core..:? "SampleRate")
            Prelude.<*> (x Core..:? "DurationMillis")
            Prelude.<*> (x Core..:? "NumberOfChannels")
      )

instance Prelude.Hashable AudioMetadata

instance Prelude.NFData AudioMetadata
