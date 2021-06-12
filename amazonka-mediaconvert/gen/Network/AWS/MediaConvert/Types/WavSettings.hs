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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.WavFormat

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value WAV.
--
-- /See:/ 'newWavSettings' smart constructor.
data WavSettings = WavSettings'
  { -- | The service defaults to using RIFF for WAV outputs. If your output audio
    -- is likely to exceed 4 GB in file size, or if you otherwise need the
    -- extended support of the RF64 format, set your output WAV file format to
    -- RF64.
    format :: Core.Maybe WavFormat,
    -- | Specify the number of channels in this output audio track. Valid values
    -- are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up
    -- to 64.
    channels :: Core.Maybe Core.Natural,
    -- | Specify Bit depth (BitDepth), in bits per sample, to choose the encoding
    -- quality for this audio track.
    bitDepth :: Core.Maybe Core.Natural,
    -- | Sample rate in Hz.
    sampleRate :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { format = Core.Nothing,
      channels = Core.Nothing,
      bitDepth = Core.Nothing,
      sampleRate = Core.Nothing
    }

-- | The service defaults to using RIFF for WAV outputs. If your output audio
-- is likely to exceed 4 GB in file size, or if you otherwise need the
-- extended support of the RF64 format, set your output WAV file format to
-- RF64.
wavSettings_format :: Lens.Lens' WavSettings (Core.Maybe WavFormat)
wavSettings_format = Lens.lens (\WavSettings' {format} -> format) (\s@WavSettings' {} a -> s {format = a} :: WavSettings)

-- | Specify the number of channels in this output audio track. Valid values
-- are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up
-- to 64.
wavSettings_channels :: Lens.Lens' WavSettings (Core.Maybe Core.Natural)
wavSettings_channels = Lens.lens (\WavSettings' {channels} -> channels) (\s@WavSettings' {} a -> s {channels = a} :: WavSettings)

-- | Specify Bit depth (BitDepth), in bits per sample, to choose the encoding
-- quality for this audio track.
wavSettings_bitDepth :: Lens.Lens' WavSettings (Core.Maybe Core.Natural)
wavSettings_bitDepth = Lens.lens (\WavSettings' {bitDepth} -> bitDepth) (\s@WavSettings' {} a -> s {bitDepth = a} :: WavSettings)

-- | Sample rate in Hz.
wavSettings_sampleRate :: Lens.Lens' WavSettings (Core.Maybe Core.Natural)
wavSettings_sampleRate = Lens.lens (\WavSettings' {sampleRate} -> sampleRate) (\s@WavSettings' {} a -> s {sampleRate = a} :: WavSettings)

instance Core.FromJSON WavSettings where
  parseJSON =
    Core.withObject
      "WavSettings"
      ( \x ->
          WavSettings'
            Core.<$> (x Core..:? "format")
            Core.<*> (x Core..:? "channels")
            Core.<*> (x Core..:? "bitDepth")
            Core.<*> (x Core..:? "sampleRate")
      )

instance Core.Hashable WavSettings

instance Core.NFData WavSettings

instance Core.ToJSON WavSettings where
  toJSON WavSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("format" Core..=) Core.<$> format,
            ("channels" Core..=) Core.<$> channels,
            ("bitDepth" Core..=) Core.<$> bitDepth,
            ("sampleRate" Core..=) Core.<$> sampleRate
          ]
      )
