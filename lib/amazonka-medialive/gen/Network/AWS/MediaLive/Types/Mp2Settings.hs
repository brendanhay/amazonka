{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mp2Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mp2Settings
  ( Mp2Settings (..),

    -- * Smart constructor
    mkMp2Settings,

    -- * Lenses
    msBitrate,
    msCodingMode,
    msSampleRate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.Mp2CodingMode as Types
import qualified Network.AWS.Prelude as Core

-- | Mp2 Settings
--
-- /See:/ 'mkMp2Settings' smart constructor.
data Mp2Settings = Mp2Settings'
  { -- | Average bitrate in bits/second.
    bitrate :: Core.Maybe Core.Double,
    -- | The MPEG2 Audio coding mode.  Valid values are codingMode10 (for mono) or codingMode20 (for stereo).
    codingMode :: Core.Maybe Types.Mp2CodingMode,
    -- | Sample rate in Hz.
    sampleRate :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Mp2Settings' value with any optional fields omitted.
mkMp2Settings ::
  Mp2Settings
mkMp2Settings =
  Mp2Settings'
    { bitrate = Core.Nothing,
      codingMode = Core.Nothing,
      sampleRate = Core.Nothing
    }

-- | Average bitrate in bits/second.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msBitrate :: Lens.Lens' Mp2Settings (Core.Maybe Core.Double)
msBitrate = Lens.field @"bitrate"
{-# DEPRECATED msBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | The MPEG2 Audio coding mode.  Valid values are codingMode10 (for mono) or codingMode20 (for stereo).
--
-- /Note:/ Consider using 'codingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msCodingMode :: Lens.Lens' Mp2Settings (Core.Maybe Types.Mp2CodingMode)
msCodingMode = Lens.field @"codingMode"
{-# DEPRECATED msCodingMode "Use generic-lens or generic-optics with 'codingMode' instead." #-}

-- | Sample rate in Hz.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msSampleRate :: Lens.Lens' Mp2Settings (Core.Maybe Core.Double)
msSampleRate = Lens.field @"sampleRate"
{-# DEPRECATED msSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

instance Core.FromJSON Mp2Settings where
  toJSON Mp2Settings {..} =
    Core.object
      ( Core.catMaybes
          [ ("bitrate" Core..=) Core.<$> bitrate,
            ("codingMode" Core..=) Core.<$> codingMode,
            ("sampleRate" Core..=) Core.<$> sampleRate
          ]
      )

instance Core.FromJSON Mp2Settings where
  parseJSON =
    Core.withObject "Mp2Settings" Core.$
      \x ->
        Mp2Settings'
          Core.<$> (x Core..:? "bitrate")
          Core.<*> (x Core..:? "codingMode")
          Core.<*> (x Core..:? "sampleRate")
