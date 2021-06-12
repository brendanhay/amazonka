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
-- Module      : Network.AWS.MediaLive.Types.WavSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.WavSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.WavCodingMode

-- | Wav Settings
--
-- /See:/ 'newWavSettings' smart constructor.
data WavSettings = WavSettings'
  { -- | The audio coding mode for the WAV audio. The mode determines the number
    -- of channels in the audio.
    codingMode :: Core.Maybe WavCodingMode,
    -- | Bits per sample.
    bitDepth :: Core.Maybe Core.Double,
    -- | Sample rate in Hz.
    sampleRate :: Core.Maybe Core.Double
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
-- 'codingMode', 'wavSettings_codingMode' - The audio coding mode for the WAV audio. The mode determines the number
-- of channels in the audio.
--
-- 'bitDepth', 'wavSettings_bitDepth' - Bits per sample.
--
-- 'sampleRate', 'wavSettings_sampleRate' - Sample rate in Hz.
newWavSettings ::
  WavSettings
newWavSettings =
  WavSettings'
    { codingMode = Core.Nothing,
      bitDepth = Core.Nothing,
      sampleRate = Core.Nothing
    }

-- | The audio coding mode for the WAV audio. The mode determines the number
-- of channels in the audio.
wavSettings_codingMode :: Lens.Lens' WavSettings (Core.Maybe WavCodingMode)
wavSettings_codingMode = Lens.lens (\WavSettings' {codingMode} -> codingMode) (\s@WavSettings' {} a -> s {codingMode = a} :: WavSettings)

-- | Bits per sample.
wavSettings_bitDepth :: Lens.Lens' WavSettings (Core.Maybe Core.Double)
wavSettings_bitDepth = Lens.lens (\WavSettings' {bitDepth} -> bitDepth) (\s@WavSettings' {} a -> s {bitDepth = a} :: WavSettings)

-- | Sample rate in Hz.
wavSettings_sampleRate :: Lens.Lens' WavSettings (Core.Maybe Core.Double)
wavSettings_sampleRate = Lens.lens (\WavSettings' {sampleRate} -> sampleRate) (\s@WavSettings' {} a -> s {sampleRate = a} :: WavSettings)

instance Core.FromJSON WavSettings where
  parseJSON =
    Core.withObject
      "WavSettings"
      ( \x ->
          WavSettings'
            Core.<$> (x Core..:? "codingMode")
            Core.<*> (x Core..:? "bitDepth")
            Core.<*> (x Core..:? "sampleRate")
      )

instance Core.Hashable WavSettings

instance Core.NFData WavSettings

instance Core.ToJSON WavSettings where
  toJSON WavSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("codingMode" Core..=) Core.<$> codingMode,
            ("bitDepth" Core..=) Core.<$> bitDepth,
            ("sampleRate" Core..=) Core.<$> sampleRate
          ]
      )
