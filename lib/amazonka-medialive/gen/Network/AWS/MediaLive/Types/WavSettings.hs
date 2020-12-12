{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.WavSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.WavSettings
  ( WavSettings (..),

    -- * Smart constructor
    mkWavSettings,

    -- * Lenses
    wsBitDepth,
    wsCodingMode,
    wsSampleRate,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.WavCodingMode
import qualified Network.AWS.Prelude as Lude

-- | Wav Settings
--
-- /See:/ 'mkWavSettings' smart constructor.
data WavSettings = WavSettings'
  { bitDepth :: Lude.Maybe Lude.Double,
    codingMode :: Lude.Maybe WavCodingMode,
    sampleRate :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WavSettings' with the minimum fields required to make a request.
--
-- * 'bitDepth' - Bits per sample.
-- * 'codingMode' - The audio coding mode for the WAV audio. The mode determines the number of channels in the audio.
-- * 'sampleRate' - Sample rate in Hz.
mkWavSettings ::
  WavSettings
mkWavSettings =
  WavSettings'
    { bitDepth = Lude.Nothing,
      codingMode = Lude.Nothing,
      sampleRate = Lude.Nothing
    }

-- | Bits per sample.
--
-- /Note:/ Consider using 'bitDepth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wsBitDepth :: Lens.Lens' WavSettings (Lude.Maybe Lude.Double)
wsBitDepth = Lens.lens (bitDepth :: WavSettings -> Lude.Maybe Lude.Double) (\s a -> s {bitDepth = a} :: WavSettings)
{-# DEPRECATED wsBitDepth "Use generic-lens or generic-optics with 'bitDepth' instead." #-}

-- | The audio coding mode for the WAV audio. The mode determines the number of channels in the audio.
--
-- /Note:/ Consider using 'codingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wsCodingMode :: Lens.Lens' WavSettings (Lude.Maybe WavCodingMode)
wsCodingMode = Lens.lens (codingMode :: WavSettings -> Lude.Maybe WavCodingMode) (\s a -> s {codingMode = a} :: WavSettings)
{-# DEPRECATED wsCodingMode "Use generic-lens or generic-optics with 'codingMode' instead." #-}

-- | Sample rate in Hz.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wsSampleRate :: Lens.Lens' WavSettings (Lude.Maybe Lude.Double)
wsSampleRate = Lens.lens (sampleRate :: WavSettings -> Lude.Maybe Lude.Double) (\s a -> s {sampleRate = a} :: WavSettings)
{-# DEPRECATED wsSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

instance Lude.FromJSON WavSettings where
  parseJSON =
    Lude.withObject
      "WavSettings"
      ( \x ->
          WavSettings'
            Lude.<$> (x Lude..:? "bitDepth")
            Lude.<*> (x Lude..:? "codingMode")
            Lude.<*> (x Lude..:? "sampleRate")
      )

instance Lude.ToJSON WavSettings where
  toJSON WavSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("bitDepth" Lude..=) Lude.<$> bitDepth,
            ("codingMode" Lude..=) Lude.<$> codingMode,
            ("sampleRate" Lude..=) Lude.<$> sampleRate
          ]
      )
