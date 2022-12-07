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
-- Module      : Amazonka.MediaConvert.Types.WavSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.WavSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.WavFormat
import qualified Amazonka.Prelude as Prelude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value WAV.
--
-- /See:/ 'newWavSettings' smart constructor.
data WavSettings = WavSettings'
  { -- | Specify Bit depth (BitDepth), in bits per sample, to choose the encoding
    -- quality for this audio track.
    bitDepth :: Prelude.Maybe Prelude.Natural,
    -- | Specify the number of channels in this output audio track. Valid values
    -- are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up
    -- to 64.
    channels :: Prelude.Maybe Prelude.Natural,
    -- | The service defaults to using RIFF for WAV outputs. If your output audio
    -- is likely to exceed 4 GB in file size, or if you otherwise need the
    -- extended support of the RF64 format, set your output WAV file format to
    -- RF64.
    format :: Prelude.Maybe WavFormat,
    -- | Sample rate in Hz.
    sampleRate :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WavSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bitDepth', 'wavSettings_bitDepth' - Specify Bit depth (BitDepth), in bits per sample, to choose the encoding
-- quality for this audio track.
--
-- 'channels', 'wavSettings_channels' - Specify the number of channels in this output audio track. Valid values
-- are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up
-- to 64.
--
-- 'format', 'wavSettings_format' - The service defaults to using RIFF for WAV outputs. If your output audio
-- is likely to exceed 4 GB in file size, or if you otherwise need the
-- extended support of the RF64 format, set your output WAV file format to
-- RF64.
--
-- 'sampleRate', 'wavSettings_sampleRate' - Sample rate in Hz.
newWavSettings ::
  WavSettings
newWavSettings =
  WavSettings'
    { bitDepth = Prelude.Nothing,
      channels = Prelude.Nothing,
      format = Prelude.Nothing,
      sampleRate = Prelude.Nothing
    }

-- | Specify Bit depth (BitDepth), in bits per sample, to choose the encoding
-- quality for this audio track.
wavSettings_bitDepth :: Lens.Lens' WavSettings (Prelude.Maybe Prelude.Natural)
wavSettings_bitDepth = Lens.lens (\WavSettings' {bitDepth} -> bitDepth) (\s@WavSettings' {} a -> s {bitDepth = a} :: WavSettings)

-- | Specify the number of channels in this output audio track. Valid values
-- are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up
-- to 64.
wavSettings_channels :: Lens.Lens' WavSettings (Prelude.Maybe Prelude.Natural)
wavSettings_channels = Lens.lens (\WavSettings' {channels} -> channels) (\s@WavSettings' {} a -> s {channels = a} :: WavSettings)

-- | The service defaults to using RIFF for WAV outputs. If your output audio
-- is likely to exceed 4 GB in file size, or if you otherwise need the
-- extended support of the RF64 format, set your output WAV file format to
-- RF64.
wavSettings_format :: Lens.Lens' WavSettings (Prelude.Maybe WavFormat)
wavSettings_format = Lens.lens (\WavSettings' {format} -> format) (\s@WavSettings' {} a -> s {format = a} :: WavSettings)

-- | Sample rate in Hz.
wavSettings_sampleRate :: Lens.Lens' WavSettings (Prelude.Maybe Prelude.Natural)
wavSettings_sampleRate = Lens.lens (\WavSettings' {sampleRate} -> sampleRate) (\s@WavSettings' {} a -> s {sampleRate = a} :: WavSettings)

instance Data.FromJSON WavSettings where
  parseJSON =
    Data.withObject
      "WavSettings"
      ( \x ->
          WavSettings'
            Prelude.<$> (x Data..:? "bitDepth")
            Prelude.<*> (x Data..:? "channels")
            Prelude.<*> (x Data..:? "format")
            Prelude.<*> (x Data..:? "sampleRate")
      )

instance Prelude.Hashable WavSettings where
  hashWithSalt _salt WavSettings' {..} =
    _salt `Prelude.hashWithSalt` bitDepth
      `Prelude.hashWithSalt` channels
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` sampleRate

instance Prelude.NFData WavSettings where
  rnf WavSettings' {..} =
    Prelude.rnf bitDepth
      `Prelude.seq` Prelude.rnf channels
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf sampleRate

instance Data.ToJSON WavSettings where
  toJSON WavSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bitDepth" Data..=) Prelude.<$> bitDepth,
            ("channels" Data..=) Prelude.<$> channels,
            ("format" Data..=) Prelude.<$> format,
            ("sampleRate" Data..=) Prelude.<$> sampleRate
          ]
      )
