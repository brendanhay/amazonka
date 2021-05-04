{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaConvert.Types.WavSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.WavSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.WavFormat
import qualified Network.AWS.Prelude as Prelude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value WAV.
--
-- /See:/ 'newWavSettings' smart constructor.
data WavSettings = WavSettings'
  { -- | The service defaults to using RIFF for WAV outputs. If your output audio
    -- is likely to exceed 4 GB in file size, or if you otherwise need the
    -- extended support of the RF64 format, set your output WAV file format to
    -- RF64.
    format :: Prelude.Maybe WavFormat,
    -- | Specify the number of channels in this output audio track. Valid values
    -- are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up
    -- to 64.
    channels :: Prelude.Maybe Prelude.Natural,
    -- | Specify Bit depth (BitDepth), in bits per sample, to choose the encoding
    -- quality for this audio track.
    bitDepth :: Prelude.Maybe Prelude.Natural,
    -- | Sample rate in Hz.
    sampleRate :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WavSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'wavSettings_format' - The service defaults to using RIFF for WAV outputs. If your output audio
-- is likely to exceed 4 GB in file size, or if you otherwise need the
-- extended support of the RF64 format, set your output WAV file format to
-- RF64.
--
-- 'channels', 'wavSettings_channels' - Specify the number of channels in this output audio track. Valid values
-- are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up
-- to 64.
--
-- 'bitDepth', 'wavSettings_bitDepth' - Specify Bit depth (BitDepth), in bits per sample, to choose the encoding
-- quality for this audio track.
--
-- 'sampleRate', 'wavSettings_sampleRate' - Sample rate in Hz.
newWavSettings ::
  WavSettings
newWavSettings =
  WavSettings'
    { format = Prelude.Nothing,
      channels = Prelude.Nothing,
      bitDepth = Prelude.Nothing,
      sampleRate = Prelude.Nothing
    }

-- | The service defaults to using RIFF for WAV outputs. If your output audio
-- is likely to exceed 4 GB in file size, or if you otherwise need the
-- extended support of the RF64 format, set your output WAV file format to
-- RF64.
wavSettings_format :: Lens.Lens' WavSettings (Prelude.Maybe WavFormat)
wavSettings_format = Lens.lens (\WavSettings' {format} -> format) (\s@WavSettings' {} a -> s {format = a} :: WavSettings)

-- | Specify the number of channels in this output audio track. Valid values
-- are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up
-- to 64.
wavSettings_channels :: Lens.Lens' WavSettings (Prelude.Maybe Prelude.Natural)
wavSettings_channels = Lens.lens (\WavSettings' {channels} -> channels) (\s@WavSettings' {} a -> s {channels = a} :: WavSettings)

-- | Specify Bit depth (BitDepth), in bits per sample, to choose the encoding
-- quality for this audio track.
wavSettings_bitDepth :: Lens.Lens' WavSettings (Prelude.Maybe Prelude.Natural)
wavSettings_bitDepth = Lens.lens (\WavSettings' {bitDepth} -> bitDepth) (\s@WavSettings' {} a -> s {bitDepth = a} :: WavSettings)

-- | Sample rate in Hz.
wavSettings_sampleRate :: Lens.Lens' WavSettings (Prelude.Maybe Prelude.Natural)
wavSettings_sampleRate = Lens.lens (\WavSettings' {sampleRate} -> sampleRate) (\s@WavSettings' {} a -> s {sampleRate = a} :: WavSettings)

instance Prelude.FromJSON WavSettings where
  parseJSON =
    Prelude.withObject
      "WavSettings"
      ( \x ->
          WavSettings'
            Prelude.<$> (x Prelude..:? "format")
            Prelude.<*> (x Prelude..:? "channels")
            Prelude.<*> (x Prelude..:? "bitDepth")
            Prelude.<*> (x Prelude..:? "sampleRate")
      )

instance Prelude.Hashable WavSettings

instance Prelude.NFData WavSettings

instance Prelude.ToJSON WavSettings where
  toJSON WavSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("format" Prelude..=) Prelude.<$> format,
            ("channels" Prelude..=) Prelude.<$> channels,
            ("bitDepth" Prelude..=) Prelude.<$> bitDepth,
            ("sampleRate" Prelude..=) Prelude.<$> sampleRate
          ]
      )
