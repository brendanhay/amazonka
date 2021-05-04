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
-- Module      : Network.AWS.MediaLive.Types.WavSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.WavSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.WavCodingMode
import qualified Network.AWS.Prelude as Prelude

-- | Wav Settings
--
-- /See:/ 'newWavSettings' smart constructor.
data WavSettings = WavSettings'
  { -- | The audio coding mode for the WAV audio. The mode determines the number
    -- of channels in the audio.
    codingMode :: Prelude.Maybe WavCodingMode,
    -- | Bits per sample.
    bitDepth :: Prelude.Maybe Prelude.Double,
    -- | Sample rate in Hz.
    sampleRate :: Prelude.Maybe Prelude.Double
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
    { codingMode = Prelude.Nothing,
      bitDepth = Prelude.Nothing,
      sampleRate = Prelude.Nothing
    }

-- | The audio coding mode for the WAV audio. The mode determines the number
-- of channels in the audio.
wavSettings_codingMode :: Lens.Lens' WavSettings (Prelude.Maybe WavCodingMode)
wavSettings_codingMode = Lens.lens (\WavSettings' {codingMode} -> codingMode) (\s@WavSettings' {} a -> s {codingMode = a} :: WavSettings)

-- | Bits per sample.
wavSettings_bitDepth :: Lens.Lens' WavSettings (Prelude.Maybe Prelude.Double)
wavSettings_bitDepth = Lens.lens (\WavSettings' {bitDepth} -> bitDepth) (\s@WavSettings' {} a -> s {bitDepth = a} :: WavSettings)

-- | Sample rate in Hz.
wavSettings_sampleRate :: Lens.Lens' WavSettings (Prelude.Maybe Prelude.Double)
wavSettings_sampleRate = Lens.lens (\WavSettings' {sampleRate} -> sampleRate) (\s@WavSettings' {} a -> s {sampleRate = a} :: WavSettings)

instance Prelude.FromJSON WavSettings where
  parseJSON =
    Prelude.withObject
      "WavSettings"
      ( \x ->
          WavSettings'
            Prelude.<$> (x Prelude..:? "codingMode")
            Prelude.<*> (x Prelude..:? "bitDepth")
            Prelude.<*> (x Prelude..:? "sampleRate")
      )

instance Prelude.Hashable WavSettings

instance Prelude.NFData WavSettings

instance Prelude.ToJSON WavSettings where
  toJSON WavSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("codingMode" Prelude..=) Prelude.<$> codingMode,
            ("bitDepth" Prelude..=) Prelude.<$> bitDepth,
            ("sampleRate" Prelude..=) Prelude.<$> sampleRate
          ]
      )
