{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NoiseReducerFilterSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NoiseReducerFilterSettings
  ( NoiseReducerFilterSettings (..),

    -- * Smart constructor
    mkNoiseReducerFilterSettings,

    -- * Lenses
    nrfsStrength,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings for a noise reducer filter
--
-- /See:/ 'mkNoiseReducerFilterSettings' smart constructor.
newtype NoiseReducerFilterSettings = NoiseReducerFilterSettings'
  { -- | Relative strength of noise reducing filter. Higher values produce stronger filtering.
    strength :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'NoiseReducerFilterSettings' value with any optional fields omitted.
mkNoiseReducerFilterSettings ::
  NoiseReducerFilterSettings
mkNoiseReducerFilterSettings =
  NoiseReducerFilterSettings' {strength = Core.Nothing}

-- | Relative strength of noise reducing filter. Higher values produce stronger filtering.
--
-- /Note:/ Consider using 'strength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrfsStrength :: Lens.Lens' NoiseReducerFilterSettings (Core.Maybe Core.Natural)
nrfsStrength = Lens.field @"strength"
{-# DEPRECATED nrfsStrength "Use generic-lens or generic-optics with 'strength' instead." #-}

instance Core.FromJSON NoiseReducerFilterSettings where
  toJSON NoiseReducerFilterSettings {..} =
    Core.object
      (Core.catMaybes [("strength" Core..=) Core.<$> strength])

instance Core.FromJSON NoiseReducerFilterSettings where
  parseJSON =
    Core.withObject "NoiseReducerFilterSettings" Core.$
      \x -> NoiseReducerFilterSettings' Core.<$> (x Core..:? "strength")
