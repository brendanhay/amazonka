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
-- Module      : Amazonka.MediaConvert.Types.Mp3Settings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Mp3Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.Mp3RateControlMode
import qualified Amazonka.Prelude as Prelude

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value MP3.
--
-- /See:/ 'newMp3Settings' smart constructor.
data Mp3Settings = Mp3Settings'
  { -- | Specify the average bitrate in bits per second.
    bitrate :: Prelude.Maybe Prelude.Natural,
    -- | Specify the number of channels in this output audio track. Choosing Mono
    -- on the console gives you 1 output channel; choosing Stereo gives you 2.
    -- In the API, valid values are 1 and 2.
    channels :: Prelude.Maybe Prelude.Natural,
    -- | Specify whether the service encodes this MP3 audio output with a
    -- constant bitrate (CBR) or a variable bitrate (VBR).
    rateControlMode :: Prelude.Maybe Mp3RateControlMode,
    -- | Sample rate in hz.
    sampleRate :: Prelude.Maybe Prelude.Natural,
    -- | Required when you set Bitrate control mode (rateControlMode) to VBR.
    -- Specify the audio quality of this MP3 output from 0 (highest quality) to
    -- 9 (lowest quality).
    vbrQuality :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Mp3Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bitrate', 'mp3Settings_bitrate' - Specify the average bitrate in bits per second.
--
-- 'channels', 'mp3Settings_channels' - Specify the number of channels in this output audio track. Choosing Mono
-- on the console gives you 1 output channel; choosing Stereo gives you 2.
-- In the API, valid values are 1 and 2.
--
-- 'rateControlMode', 'mp3Settings_rateControlMode' - Specify whether the service encodes this MP3 audio output with a
-- constant bitrate (CBR) or a variable bitrate (VBR).
--
-- 'sampleRate', 'mp3Settings_sampleRate' - Sample rate in hz.
--
-- 'vbrQuality', 'mp3Settings_vbrQuality' - Required when you set Bitrate control mode (rateControlMode) to VBR.
-- Specify the audio quality of this MP3 output from 0 (highest quality) to
-- 9 (lowest quality).
newMp3Settings ::
  Mp3Settings
newMp3Settings =
  Mp3Settings'
    { bitrate = Prelude.Nothing,
      channels = Prelude.Nothing,
      rateControlMode = Prelude.Nothing,
      sampleRate = Prelude.Nothing,
      vbrQuality = Prelude.Nothing
    }

-- | Specify the average bitrate in bits per second.
mp3Settings_bitrate :: Lens.Lens' Mp3Settings (Prelude.Maybe Prelude.Natural)
mp3Settings_bitrate = Lens.lens (\Mp3Settings' {bitrate} -> bitrate) (\s@Mp3Settings' {} a -> s {bitrate = a} :: Mp3Settings)

-- | Specify the number of channels in this output audio track. Choosing Mono
-- on the console gives you 1 output channel; choosing Stereo gives you 2.
-- In the API, valid values are 1 and 2.
mp3Settings_channels :: Lens.Lens' Mp3Settings (Prelude.Maybe Prelude.Natural)
mp3Settings_channels = Lens.lens (\Mp3Settings' {channels} -> channels) (\s@Mp3Settings' {} a -> s {channels = a} :: Mp3Settings)

-- | Specify whether the service encodes this MP3 audio output with a
-- constant bitrate (CBR) or a variable bitrate (VBR).
mp3Settings_rateControlMode :: Lens.Lens' Mp3Settings (Prelude.Maybe Mp3RateControlMode)
mp3Settings_rateControlMode = Lens.lens (\Mp3Settings' {rateControlMode} -> rateControlMode) (\s@Mp3Settings' {} a -> s {rateControlMode = a} :: Mp3Settings)

-- | Sample rate in hz.
mp3Settings_sampleRate :: Lens.Lens' Mp3Settings (Prelude.Maybe Prelude.Natural)
mp3Settings_sampleRate = Lens.lens (\Mp3Settings' {sampleRate} -> sampleRate) (\s@Mp3Settings' {} a -> s {sampleRate = a} :: Mp3Settings)

-- | Required when you set Bitrate control mode (rateControlMode) to VBR.
-- Specify the audio quality of this MP3 output from 0 (highest quality) to
-- 9 (lowest quality).
mp3Settings_vbrQuality :: Lens.Lens' Mp3Settings (Prelude.Maybe Prelude.Natural)
mp3Settings_vbrQuality = Lens.lens (\Mp3Settings' {vbrQuality} -> vbrQuality) (\s@Mp3Settings' {} a -> s {vbrQuality = a} :: Mp3Settings)

instance Data.FromJSON Mp3Settings where
  parseJSON =
    Data.withObject
      "Mp3Settings"
      ( \x ->
          Mp3Settings'
            Prelude.<$> (x Data..:? "bitrate")
            Prelude.<*> (x Data..:? "channels")
            Prelude.<*> (x Data..:? "rateControlMode")
            Prelude.<*> (x Data..:? "sampleRate")
            Prelude.<*> (x Data..:? "vbrQuality")
      )

instance Prelude.Hashable Mp3Settings where
  hashWithSalt _salt Mp3Settings' {..} =
    _salt
      `Prelude.hashWithSalt` bitrate
      `Prelude.hashWithSalt` channels
      `Prelude.hashWithSalt` rateControlMode
      `Prelude.hashWithSalt` sampleRate
      `Prelude.hashWithSalt` vbrQuality

instance Prelude.NFData Mp3Settings where
  rnf Mp3Settings' {..} =
    Prelude.rnf bitrate
      `Prelude.seq` Prelude.rnf channels
      `Prelude.seq` Prelude.rnf rateControlMode
      `Prelude.seq` Prelude.rnf sampleRate
      `Prelude.seq` Prelude.rnf vbrQuality

instance Data.ToJSON Mp3Settings where
  toJSON Mp3Settings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bitrate" Data..=) Prelude.<$> bitrate,
            ("channels" Data..=) Prelude.<$> channels,
            ("rateControlMode" Data..=)
              Prelude.<$> rateControlMode,
            ("sampleRate" Data..=) Prelude.<$> sampleRate,
            ("vbrQuality" Data..=) Prelude.<$> vbrQuality
          ]
      )
