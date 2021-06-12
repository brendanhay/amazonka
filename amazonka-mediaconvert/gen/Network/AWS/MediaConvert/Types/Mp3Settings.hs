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
-- Module      : Network.AWS.MediaConvert.Types.Mp3Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mp3Settings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.Mp3RateControlMode

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value MP3.
--
-- /See:/ 'newMp3Settings' smart constructor.
data Mp3Settings = Mp3Settings'
  { -- | Specify whether the service encodes this MP3 audio output with a
    -- constant bitrate (CBR) or a variable bitrate (VBR).
    rateControlMode :: Core.Maybe Mp3RateControlMode,
    -- | Specify the number of channels in this output audio track. Choosing Mono
    -- on the console gives you 1 output channel; choosing Stereo gives you 2.
    -- In the API, valid values are 1 and 2.
    channels :: Core.Maybe Core.Natural,
    -- | Sample rate in hz.
    sampleRate :: Core.Maybe Core.Natural,
    -- | Required when you set Bitrate control mode (rateControlMode) to VBR.
    -- Specify the audio quality of this MP3 output from 0 (highest quality) to
    -- 9 (lowest quality).
    vbrQuality :: Core.Maybe Core.Natural,
    -- | Specify the average bitrate in bits per second.
    bitrate :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Mp3Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rateControlMode', 'mp3Settings_rateControlMode' - Specify whether the service encodes this MP3 audio output with a
-- constant bitrate (CBR) or a variable bitrate (VBR).
--
-- 'channels', 'mp3Settings_channels' - Specify the number of channels in this output audio track. Choosing Mono
-- on the console gives you 1 output channel; choosing Stereo gives you 2.
-- In the API, valid values are 1 and 2.
--
-- 'sampleRate', 'mp3Settings_sampleRate' - Sample rate in hz.
--
-- 'vbrQuality', 'mp3Settings_vbrQuality' - Required when you set Bitrate control mode (rateControlMode) to VBR.
-- Specify the audio quality of this MP3 output from 0 (highest quality) to
-- 9 (lowest quality).
--
-- 'bitrate', 'mp3Settings_bitrate' - Specify the average bitrate in bits per second.
newMp3Settings ::
  Mp3Settings
newMp3Settings =
  Mp3Settings'
    { rateControlMode = Core.Nothing,
      channels = Core.Nothing,
      sampleRate = Core.Nothing,
      vbrQuality = Core.Nothing,
      bitrate = Core.Nothing
    }

-- | Specify whether the service encodes this MP3 audio output with a
-- constant bitrate (CBR) or a variable bitrate (VBR).
mp3Settings_rateControlMode :: Lens.Lens' Mp3Settings (Core.Maybe Mp3RateControlMode)
mp3Settings_rateControlMode = Lens.lens (\Mp3Settings' {rateControlMode} -> rateControlMode) (\s@Mp3Settings' {} a -> s {rateControlMode = a} :: Mp3Settings)

-- | Specify the number of channels in this output audio track. Choosing Mono
-- on the console gives you 1 output channel; choosing Stereo gives you 2.
-- In the API, valid values are 1 and 2.
mp3Settings_channels :: Lens.Lens' Mp3Settings (Core.Maybe Core.Natural)
mp3Settings_channels = Lens.lens (\Mp3Settings' {channels} -> channels) (\s@Mp3Settings' {} a -> s {channels = a} :: Mp3Settings)

-- | Sample rate in hz.
mp3Settings_sampleRate :: Lens.Lens' Mp3Settings (Core.Maybe Core.Natural)
mp3Settings_sampleRate = Lens.lens (\Mp3Settings' {sampleRate} -> sampleRate) (\s@Mp3Settings' {} a -> s {sampleRate = a} :: Mp3Settings)

-- | Required when you set Bitrate control mode (rateControlMode) to VBR.
-- Specify the audio quality of this MP3 output from 0 (highest quality) to
-- 9 (lowest quality).
mp3Settings_vbrQuality :: Lens.Lens' Mp3Settings (Core.Maybe Core.Natural)
mp3Settings_vbrQuality = Lens.lens (\Mp3Settings' {vbrQuality} -> vbrQuality) (\s@Mp3Settings' {} a -> s {vbrQuality = a} :: Mp3Settings)

-- | Specify the average bitrate in bits per second.
mp3Settings_bitrate :: Lens.Lens' Mp3Settings (Core.Maybe Core.Natural)
mp3Settings_bitrate = Lens.lens (\Mp3Settings' {bitrate} -> bitrate) (\s@Mp3Settings' {} a -> s {bitrate = a} :: Mp3Settings)

instance Core.FromJSON Mp3Settings where
  parseJSON =
    Core.withObject
      "Mp3Settings"
      ( \x ->
          Mp3Settings'
            Core.<$> (x Core..:? "rateControlMode")
            Core.<*> (x Core..:? "channels")
            Core.<*> (x Core..:? "sampleRate")
            Core.<*> (x Core..:? "vbrQuality")
            Core.<*> (x Core..:? "bitrate")
      )

instance Core.Hashable Mp3Settings

instance Core.NFData Mp3Settings

instance Core.ToJSON Mp3Settings where
  toJSON Mp3Settings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("rateControlMode" Core..=)
              Core.<$> rateControlMode,
            ("channels" Core..=) Core.<$> channels,
            ("sampleRate" Core..=) Core.<$> sampleRate,
            ("vbrQuality" Core..=) Core.<$> vbrQuality,
            ("bitrate" Core..=) Core.<$> bitrate
          ]
      )
