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
import qualified Network.AWS.MediaConvert.Types.WavFormat as Types
import qualified Network.AWS.Prelude as Core

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value WAV.
--
-- /See:/ 'mkWavSettings' smart constructor.
data WavSettings = WavSettings'
  { -- | Specify Bit depth (BitDepth), in bits per sample, to choose the encoding quality for this audio track.
    bitDepth :: Core.Maybe Core.Natural,
    -- | Specify the number of channels in this output audio track. Valid values are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up to 64.
    channels :: Core.Maybe Core.Natural,
    -- | The service defaults to using RIFF for WAV outputs. If your output audio is likely to exceed 4 GB in file size, or if you otherwise need the extended support of the RF64 format, set your output WAV file format to RF64.
    format :: Core.Maybe Types.WavFormat,
    -- | Sample rate in Hz.
    sampleRate :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WavSettings' value with any optional fields omitted.
mkWavSettings ::
  WavSettings
mkWavSettings =
  WavSettings'
    { bitDepth = Core.Nothing,
      channels = Core.Nothing,
      format = Core.Nothing,
      sampleRate = Core.Nothing
    }

-- | Specify Bit depth (BitDepth), in bits per sample, to choose the encoding quality for this audio track.
--
-- /Note:/ Consider using 'bitDepth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wsBitDepth :: Lens.Lens' WavSettings (Core.Maybe Core.Natural)
wsBitDepth = Lens.field @"bitDepth"
{-# DEPRECATED wsBitDepth "Use generic-lens or generic-optics with 'bitDepth' instead." #-}

-- | Specify the number of channels in this output audio track. Valid values are 1 and even numbers up to 64. For example, 1, 2, 4, 6, and so on, up to 64.
--
-- /Note:/ Consider using 'channels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wsChannels :: Lens.Lens' WavSettings (Core.Maybe Core.Natural)
wsChannels = Lens.field @"channels"
{-# DEPRECATED wsChannels "Use generic-lens or generic-optics with 'channels' instead." #-}

-- | The service defaults to using RIFF for WAV outputs. If your output audio is likely to exceed 4 GB in file size, or if you otherwise need the extended support of the RF64 format, set your output WAV file format to RF64.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wsFormat :: Lens.Lens' WavSettings (Core.Maybe Types.WavFormat)
wsFormat = Lens.field @"format"
{-# DEPRECATED wsFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | Sample rate in Hz.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wsSampleRate :: Lens.Lens' WavSettings (Core.Maybe Core.Natural)
wsSampleRate = Lens.field @"sampleRate"
{-# DEPRECATED wsSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

instance Core.FromJSON WavSettings where
  toJSON WavSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("bitDepth" Core..=) Core.<$> bitDepth,
            ("channels" Core..=) Core.<$> channels,
            ("format" Core..=) Core.<$> format,
            ("sampleRate" Core..=) Core.<$> sampleRate
          ]
      )

instance Core.FromJSON WavSettings where
  parseJSON =
    Core.withObject "WavSettings" Core.$
      \x ->
        WavSettings'
          Core.<$> (x Core..:? "bitDepth")
          Core.<*> (x Core..:? "channels")
          Core.<*> (x Core..:? "format")
          Core.<*> (x Core..:? "sampleRate")
