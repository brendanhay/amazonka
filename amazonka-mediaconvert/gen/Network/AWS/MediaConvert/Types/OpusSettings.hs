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
-- Module      : Network.AWS.MediaConvert.Types.OpusSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OpusSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value OPUS.
--
-- /See:/ 'newOpusSettings' smart constructor.
data OpusSettings = OpusSettings'
  { -- | Specify the number of channels in this output audio track. Choosing Mono
    -- on the console gives you 1 output channel; choosing Stereo gives you 2.
    -- In the API, valid values are 1 and 2.
    channels :: Prelude.Maybe Prelude.Natural,
    -- | Optional. Sample rate in hz. Valid values are 16000, 24000, and 48000.
    -- The default value is 48000.
    sampleRate :: Prelude.Maybe Prelude.Natural,
    -- | Optional. Specify the average bitrate in bits per second. Valid values
    -- are multiples of 8000, from 32000 through 192000. The default value is
    -- 96000, which we recommend for quality and bandwidth.
    bitrate :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpusSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channels', 'opusSettings_channels' - Specify the number of channels in this output audio track. Choosing Mono
-- on the console gives you 1 output channel; choosing Stereo gives you 2.
-- In the API, valid values are 1 and 2.
--
-- 'sampleRate', 'opusSettings_sampleRate' - Optional. Sample rate in hz. Valid values are 16000, 24000, and 48000.
-- The default value is 48000.
--
-- 'bitrate', 'opusSettings_bitrate' - Optional. Specify the average bitrate in bits per second. Valid values
-- are multiples of 8000, from 32000 through 192000. The default value is
-- 96000, which we recommend for quality and bandwidth.
newOpusSettings ::
  OpusSettings
newOpusSettings =
  OpusSettings'
    { channels = Prelude.Nothing,
      sampleRate = Prelude.Nothing,
      bitrate = Prelude.Nothing
    }

-- | Specify the number of channels in this output audio track. Choosing Mono
-- on the console gives you 1 output channel; choosing Stereo gives you 2.
-- In the API, valid values are 1 and 2.
opusSettings_channels :: Lens.Lens' OpusSettings (Prelude.Maybe Prelude.Natural)
opusSettings_channels = Lens.lens (\OpusSettings' {channels} -> channels) (\s@OpusSettings' {} a -> s {channels = a} :: OpusSettings)

-- | Optional. Sample rate in hz. Valid values are 16000, 24000, and 48000.
-- The default value is 48000.
opusSettings_sampleRate :: Lens.Lens' OpusSettings (Prelude.Maybe Prelude.Natural)
opusSettings_sampleRate = Lens.lens (\OpusSettings' {sampleRate} -> sampleRate) (\s@OpusSettings' {} a -> s {sampleRate = a} :: OpusSettings)

-- | Optional. Specify the average bitrate in bits per second. Valid values
-- are multiples of 8000, from 32000 through 192000. The default value is
-- 96000, which we recommend for quality and bandwidth.
opusSettings_bitrate :: Lens.Lens' OpusSettings (Prelude.Maybe Prelude.Natural)
opusSettings_bitrate = Lens.lens (\OpusSettings' {bitrate} -> bitrate) (\s@OpusSettings' {} a -> s {bitrate = a} :: OpusSettings)

instance Core.FromJSON OpusSettings where
  parseJSON =
    Core.withObject
      "OpusSettings"
      ( \x ->
          OpusSettings'
            Prelude.<$> (x Core..:? "channels")
            Prelude.<*> (x Core..:? "sampleRate")
            Prelude.<*> (x Core..:? "bitrate")
      )

instance Prelude.Hashable OpusSettings

instance Prelude.NFData OpusSettings

instance Core.ToJSON OpusSettings where
  toJSON OpusSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("channels" Core..=) Prelude.<$> channels,
            ("sampleRate" Core..=) Prelude.<$> sampleRate,
            ("bitrate" Core..=) Prelude.<$> bitrate
          ]
      )
