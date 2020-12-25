{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CaptionSourceFramerate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CaptionSourceFramerate
  ( CaptionSourceFramerate (..),

    -- * Smart constructor
    mkCaptionSourceFramerate,

    -- * Lenses
    csfFramerateDenominator,
    csfFramerateNumerator,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Ignore this setting unless your input captions format is SCC. To have the service compensate for differing frame rates between your input captions and input video, specify the frame rate of the captions file. Specify this value as a fraction, using the settings Framerate numerator (framerateNumerator) and Framerate denominator (framerateDenominator). For example, you might specify 24 / 1 for 24 fps, 25 / 1 for 25 fps, 24000 / 1001 for 23.976 fps, or 30000 / 1001 for 29.97 fps.
--
-- /See:/ 'mkCaptionSourceFramerate' smart constructor.
data CaptionSourceFramerate = CaptionSourceFramerate'
  { -- | Specify the denominator of the fraction that represents the frame rate for the setting Caption source frame rate (CaptionSourceFramerate). Use this setting along with the setting Framerate numerator (framerateNumerator).
    framerateDenominator :: Core.Maybe Core.Natural,
    -- | Specify the numerator of the fraction that represents the frame rate for the setting Caption source frame rate (CaptionSourceFramerate). Use this setting along with the setting Framerate denominator (framerateDenominator).
    framerateNumerator :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CaptionSourceFramerate' value with any optional fields omitted.
mkCaptionSourceFramerate ::
  CaptionSourceFramerate
mkCaptionSourceFramerate =
  CaptionSourceFramerate'
    { framerateDenominator = Core.Nothing,
      framerateNumerator = Core.Nothing
    }

-- | Specify the denominator of the fraction that represents the frame rate for the setting Caption source frame rate (CaptionSourceFramerate). Use this setting along with the setting Framerate numerator (framerateNumerator).
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfFramerateDenominator :: Lens.Lens' CaptionSourceFramerate (Core.Maybe Core.Natural)
csfFramerateDenominator = Lens.field @"framerateDenominator"
{-# DEPRECATED csfFramerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead." #-}

-- | Specify the numerator of the fraction that represents the frame rate for the setting Caption source frame rate (CaptionSourceFramerate). Use this setting along with the setting Framerate denominator (framerateDenominator).
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfFramerateNumerator :: Lens.Lens' CaptionSourceFramerate (Core.Maybe Core.Natural)
csfFramerateNumerator = Lens.field @"framerateNumerator"
{-# DEPRECATED csfFramerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead." #-}

instance Core.FromJSON CaptionSourceFramerate where
  toJSON CaptionSourceFramerate {..} =
    Core.object
      ( Core.catMaybes
          [ ("framerateDenominator" Core..=) Core.<$> framerateDenominator,
            ("framerateNumerator" Core..=) Core.<$> framerateNumerator
          ]
      )

instance Core.FromJSON CaptionSourceFramerate where
  parseJSON =
    Core.withObject "CaptionSourceFramerate" Core.$
      \x ->
        CaptionSourceFramerate'
          Core.<$> (x Core..:? "framerateDenominator")
          Core.<*> (x Core..:? "framerateNumerator")
