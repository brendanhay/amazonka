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
-- Module      : Network.AWS.MediaConvert.Types.VorbisSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.VorbisSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value Vorbis.
--
-- /See:/ 'newVorbisSettings' smart constructor.
data VorbisSettings = VorbisSettings'
  { -- | Optional. Specify the number of channels in this output audio track.
    -- Choosing Mono on the console gives you 1 output channel; choosing Stereo
    -- gives you 2. In the API, valid values are 1 and 2. The default value is
    -- 2.
    channels :: Core.Maybe Core.Natural,
    -- | Optional. Specify the audio sample rate in Hz. Valid values are 22050,
    -- 32000, 44100, and 48000. The default value is 48000.
    sampleRate :: Core.Maybe Core.Natural,
    -- | Optional. Specify the variable audio quality of this Vorbis output from
    -- -1 (lowest quality, ~45 kbit\/s) to 10 (highest quality, ~500 kbit\/s).
    -- The default value is 4 (~128 kbit\/s). Values 5 and 6 are approximately
    -- 160 and 192 kbit\/s, respectively.
    vbrQuality :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { channels = Core.Nothing,
      sampleRate = Core.Nothing,
      vbrQuality = Core.Nothing
    }

-- | Optional. Specify the number of channels in this output audio track.
-- Choosing Mono on the console gives you 1 output channel; choosing Stereo
-- gives you 2. In the API, valid values are 1 and 2. The default value is
-- 2.
vorbisSettings_channels :: Lens.Lens' VorbisSettings (Core.Maybe Core.Natural)
vorbisSettings_channels = Lens.lens (\VorbisSettings' {channels} -> channels) (\s@VorbisSettings' {} a -> s {channels = a} :: VorbisSettings)

-- | Optional. Specify the audio sample rate in Hz. Valid values are 22050,
-- 32000, 44100, and 48000. The default value is 48000.
vorbisSettings_sampleRate :: Lens.Lens' VorbisSettings (Core.Maybe Core.Natural)
vorbisSettings_sampleRate = Lens.lens (\VorbisSettings' {sampleRate} -> sampleRate) (\s@VorbisSettings' {} a -> s {sampleRate = a} :: VorbisSettings)

-- | Optional. Specify the variable audio quality of this Vorbis output from
-- -1 (lowest quality, ~45 kbit\/s) to 10 (highest quality, ~500 kbit\/s).
-- The default value is 4 (~128 kbit\/s). Values 5 and 6 are approximately
-- 160 and 192 kbit\/s, respectively.
vorbisSettings_vbrQuality :: Lens.Lens' VorbisSettings (Core.Maybe Core.Int)
vorbisSettings_vbrQuality = Lens.lens (\VorbisSettings' {vbrQuality} -> vbrQuality) (\s@VorbisSettings' {} a -> s {vbrQuality = a} :: VorbisSettings)

instance Core.FromJSON VorbisSettings where
  parseJSON =
    Core.withObject
      "VorbisSettings"
      ( \x ->
          VorbisSettings'
            Core.<$> (x Core..:? "channels")
            Core.<*> (x Core..:? "sampleRate")
            Core.<*> (x Core..:? "vbrQuality")
      )

instance Core.Hashable VorbisSettings

instance Core.NFData VorbisSettings

instance Core.ToJSON VorbisSettings where
  toJSON VorbisSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("channels" Core..=) Core.<$> channels,
            ("sampleRate" Core..=) Core.<$> sampleRate,
            ("vbrQuality" Core..=) Core.<$> vbrQuality
          ]
      )
