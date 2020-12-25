{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TemporalFilterSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TemporalFilterSettings
  ( TemporalFilterSettings (..),

    -- * Smart constructor
    mkTemporalFilterSettings,

    -- * Lenses
    tfsPostFilterSharpening,
    tfsStrength,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.TemporalFilterPostFilterSharpening as Types
import qualified Network.AWS.MediaLive.Types.TemporalFilterStrength as Types
import qualified Network.AWS.Prelude as Core

-- | Temporal Filter Settings
--
-- /See:/ 'mkTemporalFilterSettings' smart constructor.
data TemporalFilterSettings = TemporalFilterSettings'
  { -- | If you enable this filter, the results are the following:
    --
    -- - If the source content is noisy (it contains excessive digital artifacts), the filter cleans up the source.
    -- - If the source content is already clean, the filter tends to decrease the bitrate, especially when the rate control mode is QVBR.
    postFilterSharpening :: Core.Maybe Types.TemporalFilterPostFilterSharpening,
    -- | Choose a filter strength. We recommend a strength of 1 or 2. A higher strength might take out good information, resulting in an image that is overly soft.
    strength :: Core.Maybe Types.TemporalFilterStrength
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TemporalFilterSettings' value with any optional fields omitted.
mkTemporalFilterSettings ::
  TemporalFilterSettings
mkTemporalFilterSettings =
  TemporalFilterSettings'
    { postFilterSharpening = Core.Nothing,
      strength = Core.Nothing
    }

-- | If you enable this filter, the results are the following:
--
-- - If the source content is noisy (it contains excessive digital artifacts), the filter cleans up the source.
-- - If the source content is already clean, the filter tends to decrease the bitrate, especially when the rate control mode is QVBR.
--
-- /Note:/ Consider using 'postFilterSharpening' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfsPostFilterSharpening :: Lens.Lens' TemporalFilterSettings (Core.Maybe Types.TemporalFilterPostFilterSharpening)
tfsPostFilterSharpening = Lens.field @"postFilterSharpening"
{-# DEPRECATED tfsPostFilterSharpening "Use generic-lens or generic-optics with 'postFilterSharpening' instead." #-}

-- | Choose a filter strength. We recommend a strength of 1 or 2. A higher strength might take out good information, resulting in an image that is overly soft.
--
-- /Note:/ Consider using 'strength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfsStrength :: Lens.Lens' TemporalFilterSettings (Core.Maybe Types.TemporalFilterStrength)
tfsStrength = Lens.field @"strength"
{-# DEPRECATED tfsStrength "Use generic-lens or generic-optics with 'strength' instead." #-}

instance Core.FromJSON TemporalFilterSettings where
  toJSON TemporalFilterSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("postFilterSharpening" Core..=) Core.<$> postFilterSharpening,
            ("strength" Core..=) Core.<$> strength
          ]
      )

instance Core.FromJSON TemporalFilterSettings where
  parseJSON =
    Core.withObject "TemporalFilterSettings" Core.$
      \x ->
        TemporalFilterSettings'
          Core.<$> (x Core..:? "postFilterSharpening")
          Core.<*> (x Core..:? "strength")
