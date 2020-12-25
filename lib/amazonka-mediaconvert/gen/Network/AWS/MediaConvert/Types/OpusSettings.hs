{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OpusSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OpusSettings
  ( OpusSettings (..),

    -- * Smart constructor
    mkOpusSettings,

    -- * Lenses
    osBitrate,
    osChannels,
    osSampleRate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to the value OPUS.
--
-- /See:/ 'mkOpusSettings' smart constructor.
data OpusSettings = OpusSettings'
  { -- | Optional. Specify the average bitrate in bits per second. Valid values are multiples of 8000, from 32000 through 192000. The default value is 96000, which we recommend for quality and bandwidth.
    bitrate :: Core.Maybe Core.Natural,
    -- | Specify the number of channels in this output audio track. Choosing Mono on the console gives you 1 output channel; choosing Stereo gives you 2. In the API, valid values are 1 and 2.
    channels :: Core.Maybe Core.Natural,
    -- | Optional. Sample rate in hz. Valid values are 16000, 24000, and 48000. The default value is 48000.
    sampleRate :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OpusSettings' value with any optional fields omitted.
mkOpusSettings ::
  OpusSettings
mkOpusSettings =
  OpusSettings'
    { bitrate = Core.Nothing,
      channels = Core.Nothing,
      sampleRate = Core.Nothing
    }

-- | Optional. Specify the average bitrate in bits per second. Valid values are multiples of 8000, from 32000 through 192000. The default value is 96000, which we recommend for quality and bandwidth.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osBitrate :: Lens.Lens' OpusSettings (Core.Maybe Core.Natural)
osBitrate = Lens.field @"bitrate"
{-# DEPRECATED osBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | Specify the number of channels in this output audio track. Choosing Mono on the console gives you 1 output channel; choosing Stereo gives you 2. In the API, valid values are 1 and 2.
--
-- /Note:/ Consider using 'channels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osChannels :: Lens.Lens' OpusSettings (Core.Maybe Core.Natural)
osChannels = Lens.field @"channels"
{-# DEPRECATED osChannels "Use generic-lens or generic-optics with 'channels' instead." #-}

-- | Optional. Sample rate in hz. Valid values are 16000, 24000, and 48000. The default value is 48000.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osSampleRate :: Lens.Lens' OpusSettings (Core.Maybe Core.Natural)
osSampleRate = Lens.field @"sampleRate"
{-# DEPRECATED osSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

instance Core.FromJSON OpusSettings where
  toJSON OpusSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("bitrate" Core..=) Core.<$> bitrate,
            ("channels" Core..=) Core.<$> channels,
            ("sampleRate" Core..=) Core.<$> sampleRate
          ]
      )

instance Core.FromJSON OpusSettings where
  parseJSON =
    Core.withObject "OpusSettings" Core.$
      \x ->
        OpusSettings'
          Core.<$> (x Core..:? "bitrate")
          Core.<*> (x Core..:? "channels")
          Core.<*> (x Core..:? "sampleRate")
