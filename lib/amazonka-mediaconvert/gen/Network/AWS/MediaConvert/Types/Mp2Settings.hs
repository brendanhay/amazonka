{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mp2Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mp2Settings
  ( Mp2Settings (..),

    -- * Smart constructor
    mkMp2Settings,

    -- * Lenses
    msBitrate,
    msChannels,
    msSampleRate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value MP2.
--
-- /See:/ 'mkMp2Settings' smart constructor.
data Mp2Settings = Mp2Settings'
  { -- | Specify the average bitrate in bits per second.
    bitrate :: Core.Maybe Core.Natural,
    -- | Set Channels to specify the number of channels in this output audio track. Choosing Mono in the console will give you 1 output channel; choosing Stereo will give you 2. In the API, valid values are 1 and 2.
    channels :: Core.Maybe Core.Natural,
    -- | Sample rate in hz.
    sampleRate :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Mp2Settings' value with any optional fields omitted.
mkMp2Settings ::
  Mp2Settings
mkMp2Settings =
  Mp2Settings'
    { bitrate = Core.Nothing,
      channels = Core.Nothing,
      sampleRate = Core.Nothing
    }

-- | Specify the average bitrate in bits per second.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msBitrate :: Lens.Lens' Mp2Settings (Core.Maybe Core.Natural)
msBitrate = Lens.field @"bitrate"
{-# DEPRECATED msBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | Set Channels to specify the number of channels in this output audio track. Choosing Mono in the console will give you 1 output channel; choosing Stereo will give you 2. In the API, valid values are 1 and 2.
--
-- /Note:/ Consider using 'channels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msChannels :: Lens.Lens' Mp2Settings (Core.Maybe Core.Natural)
msChannels = Lens.field @"channels"
{-# DEPRECATED msChannels "Use generic-lens or generic-optics with 'channels' instead." #-}

-- | Sample rate in hz.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msSampleRate :: Lens.Lens' Mp2Settings (Core.Maybe Core.Natural)
msSampleRate = Lens.field @"sampleRate"
{-# DEPRECATED msSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

instance Core.FromJSON Mp2Settings where
  toJSON Mp2Settings {..} =
    Core.object
      ( Core.catMaybes
          [ ("bitrate" Core..=) Core.<$> bitrate,
            ("channels" Core..=) Core.<$> channels,
            ("sampleRate" Core..=) Core.<$> sampleRate
          ]
      )

instance Core.FromJSON Mp2Settings where
  parseJSON =
    Core.withObject "Mp2Settings" Core.$
      \x ->
        Mp2Settings'
          Core.<$> (x Core..:? "bitrate")
          Core.<*> (x Core..:? "channels")
          Core.<*> (x Core..:? "sampleRate")
