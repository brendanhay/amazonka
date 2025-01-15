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
-- Module      : Amazonka.MediaConvert.Types.VorbisSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.VorbisSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value Vorbis.
--
-- /See:/ 'newVorbisSettings' smart constructor.
data VorbisSettings = VorbisSettings'
  { -- | Optional. Specify the number of channels in this output audio track.
    -- Choosing Mono on the console gives you 1 output channel; choosing Stereo
    -- gives you 2. In the API, valid values are 1 and 2. The default value is
    -- 2.
    channels :: Prelude.Maybe Prelude.Natural,
    -- | Optional. Specify the audio sample rate in Hz. Valid values are 22050,
    -- 32000, 44100, and 48000. The default value is 48000.
    sampleRate :: Prelude.Maybe Prelude.Natural,
    -- | Optional. Specify the variable audio quality of this Vorbis output from
    -- -1 (lowest quality, ~45 kbit\/s) to 10 (highest quality, ~500 kbit\/s).
    -- The default value is 4 (~128 kbit\/s). Values 5 and 6 are approximately
    -- 160 and 192 kbit\/s, respectively.
    vbrQuality :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VorbisSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channels', 'vorbisSettings_channels' - Optional. Specify the number of channels in this output audio track.
-- Choosing Mono on the console gives you 1 output channel; choosing Stereo
-- gives you 2. In the API, valid values are 1 and 2. The default value is
-- 2.
--
-- 'sampleRate', 'vorbisSettings_sampleRate' - Optional. Specify the audio sample rate in Hz. Valid values are 22050,
-- 32000, 44100, and 48000. The default value is 48000.
--
-- 'vbrQuality', 'vorbisSettings_vbrQuality' - Optional. Specify the variable audio quality of this Vorbis output from
-- -1 (lowest quality, ~45 kbit\/s) to 10 (highest quality, ~500 kbit\/s).
-- The default value is 4 (~128 kbit\/s). Values 5 and 6 are approximately
-- 160 and 192 kbit\/s, respectively.
newVorbisSettings ::
  VorbisSettings
newVorbisSettings =
  VorbisSettings'
    { channels = Prelude.Nothing,
      sampleRate = Prelude.Nothing,
      vbrQuality = Prelude.Nothing
    }

-- | Optional. Specify the number of channels in this output audio track.
-- Choosing Mono on the console gives you 1 output channel; choosing Stereo
-- gives you 2. In the API, valid values are 1 and 2. The default value is
-- 2.
vorbisSettings_channels :: Lens.Lens' VorbisSettings (Prelude.Maybe Prelude.Natural)
vorbisSettings_channels = Lens.lens (\VorbisSettings' {channels} -> channels) (\s@VorbisSettings' {} a -> s {channels = a} :: VorbisSettings)

-- | Optional. Specify the audio sample rate in Hz. Valid values are 22050,
-- 32000, 44100, and 48000. The default value is 48000.
vorbisSettings_sampleRate :: Lens.Lens' VorbisSettings (Prelude.Maybe Prelude.Natural)
vorbisSettings_sampleRate = Lens.lens (\VorbisSettings' {sampleRate} -> sampleRate) (\s@VorbisSettings' {} a -> s {sampleRate = a} :: VorbisSettings)

-- | Optional. Specify the variable audio quality of this Vorbis output from
-- -1 (lowest quality, ~45 kbit\/s) to 10 (highest quality, ~500 kbit\/s).
-- The default value is 4 (~128 kbit\/s). Values 5 and 6 are approximately
-- 160 and 192 kbit\/s, respectively.
vorbisSettings_vbrQuality :: Lens.Lens' VorbisSettings (Prelude.Maybe Prelude.Int)
vorbisSettings_vbrQuality = Lens.lens (\VorbisSettings' {vbrQuality} -> vbrQuality) (\s@VorbisSettings' {} a -> s {vbrQuality = a} :: VorbisSettings)

instance Data.FromJSON VorbisSettings where
  parseJSON =
    Data.withObject
      "VorbisSettings"
      ( \x ->
          VorbisSettings'
            Prelude.<$> (x Data..:? "channels")
            Prelude.<*> (x Data..:? "sampleRate")
            Prelude.<*> (x Data..:? "vbrQuality")
      )

instance Prelude.Hashable VorbisSettings where
  hashWithSalt _salt VorbisSettings' {..} =
    _salt
      `Prelude.hashWithSalt` channels
      `Prelude.hashWithSalt` sampleRate
      `Prelude.hashWithSalt` vbrQuality

instance Prelude.NFData VorbisSettings where
  rnf VorbisSettings' {..} =
    Prelude.rnf channels `Prelude.seq`
      Prelude.rnf sampleRate `Prelude.seq`
        Prelude.rnf vbrQuality

instance Data.ToJSON VorbisSettings where
  toJSON VorbisSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("channels" Data..=) Prelude.<$> channels,
            ("sampleRate" Data..=) Prelude.<$> sampleRate,
            ("vbrQuality" Data..=) Prelude.<$> vbrQuality
          ]
      )
