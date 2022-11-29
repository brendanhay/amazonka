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
-- Module      : Amazonka.IVS.Types.AudioConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Types.AudioConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Object specifying a stream’s audio configuration, as set up by the
-- broadcaster (usually in an encoder). This is part of the
-- IngestConfiguration object and used for monitoring stream health.
--
-- /See:/ 'newAudioConfiguration' smart constructor.
data AudioConfiguration = AudioConfiguration'
  { -- | The expected ingest bitrate (bits per second). This is configured in the
    -- encoder.
    targetBitrate :: Prelude.Maybe Prelude.Integer,
    -- | Number of audio channels.
    channels :: Prelude.Maybe Prelude.Integer,
    -- | Number of audio samples recorded per second.
    sampleRate :: Prelude.Maybe Prelude.Integer,
    -- | Codec used for the audio encoding.
    codec :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AudioConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetBitrate', 'audioConfiguration_targetBitrate' - The expected ingest bitrate (bits per second). This is configured in the
-- encoder.
--
-- 'channels', 'audioConfiguration_channels' - Number of audio channels.
--
-- 'sampleRate', 'audioConfiguration_sampleRate' - Number of audio samples recorded per second.
--
-- 'codec', 'audioConfiguration_codec' - Codec used for the audio encoding.
newAudioConfiguration ::
  AudioConfiguration
newAudioConfiguration =
  AudioConfiguration'
    { targetBitrate =
        Prelude.Nothing,
      channels = Prelude.Nothing,
      sampleRate = Prelude.Nothing,
      codec = Prelude.Nothing
    }

-- | The expected ingest bitrate (bits per second). This is configured in the
-- encoder.
audioConfiguration_targetBitrate :: Lens.Lens' AudioConfiguration (Prelude.Maybe Prelude.Integer)
audioConfiguration_targetBitrate = Lens.lens (\AudioConfiguration' {targetBitrate} -> targetBitrate) (\s@AudioConfiguration' {} a -> s {targetBitrate = a} :: AudioConfiguration)

-- | Number of audio channels.
audioConfiguration_channels :: Lens.Lens' AudioConfiguration (Prelude.Maybe Prelude.Integer)
audioConfiguration_channels = Lens.lens (\AudioConfiguration' {channels} -> channels) (\s@AudioConfiguration' {} a -> s {channels = a} :: AudioConfiguration)

-- | Number of audio samples recorded per second.
audioConfiguration_sampleRate :: Lens.Lens' AudioConfiguration (Prelude.Maybe Prelude.Integer)
audioConfiguration_sampleRate = Lens.lens (\AudioConfiguration' {sampleRate} -> sampleRate) (\s@AudioConfiguration' {} a -> s {sampleRate = a} :: AudioConfiguration)

-- | Codec used for the audio encoding.
audioConfiguration_codec :: Lens.Lens' AudioConfiguration (Prelude.Maybe Prelude.Text)
audioConfiguration_codec = Lens.lens (\AudioConfiguration' {codec} -> codec) (\s@AudioConfiguration' {} a -> s {codec = a} :: AudioConfiguration)

instance Core.FromJSON AudioConfiguration where
  parseJSON =
    Core.withObject
      "AudioConfiguration"
      ( \x ->
          AudioConfiguration'
            Prelude.<$> (x Core..:? "targetBitrate")
            Prelude.<*> (x Core..:? "channels")
            Prelude.<*> (x Core..:? "sampleRate")
            Prelude.<*> (x Core..:? "codec")
      )

instance Prelude.Hashable AudioConfiguration where
  hashWithSalt _salt AudioConfiguration' {..} =
    _salt `Prelude.hashWithSalt` targetBitrate
      `Prelude.hashWithSalt` channels
      `Prelude.hashWithSalt` sampleRate
      `Prelude.hashWithSalt` codec

instance Prelude.NFData AudioConfiguration where
  rnf AudioConfiguration' {..} =
    Prelude.rnf targetBitrate
      `Prelude.seq` Prelude.rnf channels
      `Prelude.seq` Prelude.rnf sampleRate
      `Prelude.seq` Prelude.rnf codec
