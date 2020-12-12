{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.WavSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.WavSettings
  ( WavSettings (..),

    -- * Smart constructor
    mkWavSettings,

    -- * Lenses
    wsBitDepth,
    wsChannels,
    wsFormat,
    wsSampleRate,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.WavFormat
import qualified Network.AWS.Prelude as Lude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value WAV.
--
-- /See:/ 'mkWavSettings' smart constructor.
data WavSettings = WavSettings'
  { bitDepth ::
      Lude.Maybe Lude.Natural,
    channels :: Lude.Maybe Lude.Natural,
    format :: Lude.Maybe WavFormat,
    sampleRate :: Lude.Maybe Lude.Natural
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
-- * 'bitDepth' - Specify Bit depth (BitDepth), in bits per sample, to choose the encoding quality for this audio track.
-- * 'channels' - Specify the number of channels in this output audio track. Valid values are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up to 64.
-- * 'format' - The service defaults to using RIFF for WAV outputs. If your output audio is likely to exceed 4 GB in file size, or if you otherwise need the extended support of the RF64 format, set your output WAV file format to RF64.
-- * 'sampleRate' - Sample rate in Hz.
mkWavSettings ::
  WavSettings
mkWavSettings =
  WavSettings'
    { bitDepth = Lude.Nothing,
      channels = Lude.Nothing,
      format = Lude.Nothing,
      sampleRate = Lude.Nothing
    }

-- | Specify Bit depth (BitDepth), in bits per sample, to choose the encoding quality for this audio track.
--
-- /Note:/ Consider using 'bitDepth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wsBitDepth :: Lens.Lens' WavSettings (Lude.Maybe Lude.Natural)
wsBitDepth = Lens.lens (bitDepth :: WavSettings -> Lude.Maybe Lude.Natural) (\s a -> s {bitDepth = a} :: WavSettings)
{-# DEPRECATED wsBitDepth "Use generic-lens or generic-optics with 'bitDepth' instead." #-}

-- | Specify the number of channels in this output audio track. Valid values are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up to 64.
--
-- /Note:/ Consider using 'channels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wsChannels :: Lens.Lens' WavSettings (Lude.Maybe Lude.Natural)
wsChannels = Lens.lens (channels :: WavSettings -> Lude.Maybe Lude.Natural) (\s a -> s {channels = a} :: WavSettings)
{-# DEPRECATED wsChannels "Use generic-lens or generic-optics with 'channels' instead." #-}

-- | The service defaults to using RIFF for WAV outputs. If your output audio is likely to exceed 4 GB in file size, or if you otherwise need the extended support of the RF64 format, set your output WAV file format to RF64.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wsFormat :: Lens.Lens' WavSettings (Lude.Maybe WavFormat)
wsFormat = Lens.lens (format :: WavSettings -> Lude.Maybe WavFormat) (\s a -> s {format = a} :: WavSettings)
{-# DEPRECATED wsFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | Sample rate in Hz.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wsSampleRate :: Lens.Lens' WavSettings (Lude.Maybe Lude.Natural)
wsSampleRate = Lens.lens (sampleRate :: WavSettings -> Lude.Maybe Lude.Natural) (\s a -> s {sampleRate = a} :: WavSettings)
{-# DEPRECATED wsSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

instance Lude.FromJSON WavSettings where
  parseJSON =
    Lude.withObject
      "WavSettings"
      ( \x ->
          WavSettings'
            Lude.<$> (x Lude..:? "bitDepth")
            Lude.<*> (x Lude..:? "channels")
            Lude.<*> (x Lude..:? "format")
            Lude.<*> (x Lude..:? "sampleRate")
      )

instance Lude.ToJSON WavSettings where
  toJSON WavSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("bitDepth" Lude..=) Lude.<$> bitDepth,
            ("channels" Lude..=) Lude.<$> channels,
            ("format" Lude..=) Lude.<$> format,
            ("sampleRate" Lude..=) Lude.<$> sampleRate
          ]
      )
