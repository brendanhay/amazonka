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
-- Module      : Amazonka.MediaConvert.Types.OpusSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.OpusSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value OPUS.
--
-- /See:/ 'newOpusSettings' smart constructor.
data OpusSettings = OpusSettings'
  { -- | Optional. Specify the average bitrate in bits per second. Valid values
    -- are multiples of 8000, from 32000 through 192000. The default value is
    -- 96000, which we recommend for quality and bandwidth.
    bitrate :: Prelude.Maybe Prelude.Natural,
    -- | Specify the number of channels in this output audio track. Choosing Mono
    -- on the console gives you 1 output channel; choosing Stereo gives you 2.
    -- In the API, valid values are 1 and 2.
    channels :: Prelude.Maybe Prelude.Natural,
    -- | Optional. Sample rate in hz. Valid values are 16000, 24000, and 48000.
    -- The default value is 48000.
    sampleRate :: Prelude.Maybe Prelude.Natural
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
-- 'bitrate', 'opusSettings_bitrate' - Optional. Specify the average bitrate in bits per second. Valid values
-- are multiples of 8000, from 32000 through 192000. The default value is
-- 96000, which we recommend for quality and bandwidth.
--
-- 'channels', 'opusSettings_channels' - Specify the number of channels in this output audio track. Choosing Mono
-- on the console gives you 1 output channel; choosing Stereo gives you 2.
-- In the API, valid values are 1 and 2.
--
-- 'sampleRate', 'opusSettings_sampleRate' - Optional. Sample rate in hz. Valid values are 16000, 24000, and 48000.
-- The default value is 48000.
newOpusSettings ::
  OpusSettings
newOpusSettings =
  OpusSettings'
    { bitrate = Prelude.Nothing,
      channels = Prelude.Nothing,
      sampleRate = Prelude.Nothing
    }

-- | Optional. Specify the average bitrate in bits per second. Valid values
-- are multiples of 8000, from 32000 through 192000. The default value is
-- 96000, which we recommend for quality and bandwidth.
opusSettings_bitrate :: Lens.Lens' OpusSettings (Prelude.Maybe Prelude.Natural)
opusSettings_bitrate = Lens.lens (\OpusSettings' {bitrate} -> bitrate) (\s@OpusSettings' {} a -> s {bitrate = a} :: OpusSettings)

-- | Specify the number of channels in this output audio track. Choosing Mono
-- on the console gives you 1 output channel; choosing Stereo gives you 2.
-- In the API, valid values are 1 and 2.
opusSettings_channels :: Lens.Lens' OpusSettings (Prelude.Maybe Prelude.Natural)
opusSettings_channels = Lens.lens (\OpusSettings' {channels} -> channels) (\s@OpusSettings' {} a -> s {channels = a} :: OpusSettings)

-- | Optional. Sample rate in hz. Valid values are 16000, 24000, and 48000.
-- The default value is 48000.
opusSettings_sampleRate :: Lens.Lens' OpusSettings (Prelude.Maybe Prelude.Natural)
opusSettings_sampleRate = Lens.lens (\OpusSettings' {sampleRate} -> sampleRate) (\s@OpusSettings' {} a -> s {sampleRate = a} :: OpusSettings)

instance Data.FromJSON OpusSettings where
  parseJSON =
    Data.withObject
      "OpusSettings"
      ( \x ->
          OpusSettings'
            Prelude.<$> (x Data..:? "bitrate")
            Prelude.<*> (x Data..:? "channels")
            Prelude.<*> (x Data..:? "sampleRate")
      )

instance Prelude.Hashable OpusSettings where
  hashWithSalt _salt OpusSettings' {..} =
    _salt
      `Prelude.hashWithSalt` bitrate
      `Prelude.hashWithSalt` channels
      `Prelude.hashWithSalt` sampleRate

instance Prelude.NFData OpusSettings where
  rnf OpusSettings' {..} =
    Prelude.rnf bitrate
      `Prelude.seq` Prelude.rnf channels
      `Prelude.seq` Prelude.rnf sampleRate

instance Data.ToJSON OpusSettings where
  toJSON OpusSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bitrate" Data..=) Prelude.<$> bitrate,
            ("channels" Data..=) Prelude.<$> channels,
            ("sampleRate" Data..=) Prelude.<$> sampleRate
          ]
      )
