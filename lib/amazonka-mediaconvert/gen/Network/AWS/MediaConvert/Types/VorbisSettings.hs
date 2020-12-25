{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.VorbisSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.VorbisSettings
  ( VorbisSettings (..),

    -- * Smart constructor
    mkVorbisSettings,

    -- * Lenses
    vsChannels,
    vsSampleRate,
    vsVbrQuality,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to the value Vorbis.
--
-- /See:/ 'mkVorbisSettings' smart constructor.
data VorbisSettings = VorbisSettings'
  { -- | Optional. Specify the number of channels in this output audio track. Choosing Mono on the console gives you 1 output channel; choosing Stereo gives you 2. In the API, valid values are 1 and 2. The default value is 2.
    channels :: Core.Maybe Core.Natural,
    -- | Optional. Specify the audio sample rate in Hz. Valid values are 22050, 32000, 44100, and 48000. The default value is 48000.
    sampleRate :: Core.Maybe Core.Natural,
    -- | Optional. Specify the variable audio quality of this Vorbis output from -1 (lowest quality, ~45 kbit/s) to 10 (highest quality, ~500 kbit/s). The default value is 4 (~128 kbit/s). Values 5 and 6 are approximately 160 and 192 kbit/s, respectively.
    vbrQuality :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VorbisSettings' value with any optional fields omitted.
mkVorbisSettings ::
  VorbisSettings
mkVorbisSettings =
  VorbisSettings'
    { channels = Core.Nothing,
      sampleRate = Core.Nothing,
      vbrQuality = Core.Nothing
    }

-- | Optional. Specify the number of channels in this output audio track. Choosing Mono on the console gives you 1 output channel; choosing Stereo gives you 2. In the API, valid values are 1 and 2. The default value is 2.
--
-- /Note:/ Consider using 'channels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsChannels :: Lens.Lens' VorbisSettings (Core.Maybe Core.Natural)
vsChannels = Lens.field @"channels"
{-# DEPRECATED vsChannels "Use generic-lens or generic-optics with 'channels' instead." #-}

-- | Optional. Specify the audio sample rate in Hz. Valid values are 22050, 32000, 44100, and 48000. The default value is 48000.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsSampleRate :: Lens.Lens' VorbisSettings (Core.Maybe Core.Natural)
vsSampleRate = Lens.field @"sampleRate"
{-# DEPRECATED vsSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

-- | Optional. Specify the variable audio quality of this Vorbis output from -1 (lowest quality, ~45 kbit/s) to 10 (highest quality, ~500 kbit/s). The default value is 4 (~128 kbit/s). Values 5 and 6 are approximately 160 and 192 kbit/s, respectively.
--
-- /Note:/ Consider using 'vbrQuality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsVbrQuality :: Lens.Lens' VorbisSettings (Core.Maybe Core.Int)
vsVbrQuality = Lens.field @"vbrQuality"
{-# DEPRECATED vsVbrQuality "Use generic-lens or generic-optics with 'vbrQuality' instead." #-}

instance Core.FromJSON VorbisSettings where
  toJSON VorbisSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("channels" Core..=) Core.<$> channels,
            ("sampleRate" Core..=) Core.<$> sampleRate,
            ("vbrQuality" Core..=) Core.<$> vbrQuality
          ]
      )

instance Core.FromJSON VorbisSettings where
  parseJSON =
    Core.withObject "VorbisSettings" Core.$
      \x ->
        VorbisSettings'
          Core.<$> (x Core..:? "channels")
          Core.<*> (x Core..:? "sampleRate")
          Core.<*> (x Core..:? "vbrQuality")
