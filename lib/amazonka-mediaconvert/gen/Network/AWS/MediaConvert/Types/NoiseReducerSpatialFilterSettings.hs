{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NoiseReducerSpatialFilterSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NoiseReducerSpatialFilterSettings
  ( NoiseReducerSpatialFilterSettings (..),

    -- * Smart constructor
    mkNoiseReducerSpatialFilterSettings,

    -- * Lenses
    nrsfsPostFilterSharpenStrength,
    nrsfsSpeed,
    nrsfsStrength,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Noise reducer filter settings for spatial filter.
--
-- /See:/ 'mkNoiseReducerSpatialFilterSettings' smart constructor.
data NoiseReducerSpatialFilterSettings = NoiseReducerSpatialFilterSettings'
  { -- | Specify strength of post noise reduction sharpening filter, with 0 disabling the filter and 3 enabling it at maximum strength.
    postFilterSharpenStrength :: Core.Maybe Core.Natural,
    -- | The speed of the filter, from -2 (lower speed) to 3 (higher speed), with 0 being the nominal value.
    speed :: Core.Maybe Core.Int,
    -- | Relative strength of noise reducing filter. Higher values produce stronger filtering.
    strength :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NoiseReducerSpatialFilterSettings' value with any optional fields omitted.
mkNoiseReducerSpatialFilterSettings ::
  NoiseReducerSpatialFilterSettings
mkNoiseReducerSpatialFilterSettings =
  NoiseReducerSpatialFilterSettings'
    { postFilterSharpenStrength =
        Core.Nothing,
      speed = Core.Nothing,
      strength = Core.Nothing
    }

-- | Specify strength of post noise reduction sharpening filter, with 0 disabling the filter and 3 enabling it at maximum strength.
--
-- /Note:/ Consider using 'postFilterSharpenStrength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrsfsPostFilterSharpenStrength :: Lens.Lens' NoiseReducerSpatialFilterSettings (Core.Maybe Core.Natural)
nrsfsPostFilterSharpenStrength = Lens.field @"postFilterSharpenStrength"
{-# DEPRECATED nrsfsPostFilterSharpenStrength "Use generic-lens or generic-optics with 'postFilterSharpenStrength' instead." #-}

-- | The speed of the filter, from -2 (lower speed) to 3 (higher speed), with 0 being the nominal value.
--
-- /Note:/ Consider using 'speed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrsfsSpeed :: Lens.Lens' NoiseReducerSpatialFilterSettings (Core.Maybe Core.Int)
nrsfsSpeed = Lens.field @"speed"
{-# DEPRECATED nrsfsSpeed "Use generic-lens or generic-optics with 'speed' instead." #-}

-- | Relative strength of noise reducing filter. Higher values produce stronger filtering.
--
-- /Note:/ Consider using 'strength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrsfsStrength :: Lens.Lens' NoiseReducerSpatialFilterSettings (Core.Maybe Core.Natural)
nrsfsStrength = Lens.field @"strength"
{-# DEPRECATED nrsfsStrength "Use generic-lens or generic-optics with 'strength' instead." #-}

instance Core.FromJSON NoiseReducerSpatialFilterSettings where
  toJSON NoiseReducerSpatialFilterSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("postFilterSharpenStrength" Core..=)
              Core.<$> postFilterSharpenStrength,
            ("speed" Core..=) Core.<$> speed,
            ("strength" Core..=) Core.<$> strength
          ]
      )

instance Core.FromJSON NoiseReducerSpatialFilterSettings where
  parseJSON =
    Core.withObject "NoiseReducerSpatialFilterSettings" Core.$
      \x ->
        NoiseReducerSpatialFilterSettings'
          Core.<$> (x Core..:? "postFilterSharpenStrength")
          Core.<*> (x Core..:? "speed")
          Core.<*> (x Core..:? "strength")
