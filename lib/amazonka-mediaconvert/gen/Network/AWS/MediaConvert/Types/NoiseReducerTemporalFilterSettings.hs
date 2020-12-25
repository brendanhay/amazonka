{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NoiseReducerTemporalFilterSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NoiseReducerTemporalFilterSettings
  ( NoiseReducerTemporalFilterSettings (..),

    -- * Smart constructor
    mkNoiseReducerTemporalFilterSettings,

    -- * Lenses
    nrtfsAggressiveMode,
    nrtfsPostTemporalSharpening,
    nrtfsSpeed,
    nrtfsStrength,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.NoiseFilterPostTemporalSharpening as Types
import qualified Network.AWS.Prelude as Core

-- | Noise reducer filter settings for temporal filter.
--
-- /See:/ 'mkNoiseReducerTemporalFilterSettings' smart constructor.
data NoiseReducerTemporalFilterSettings = NoiseReducerTemporalFilterSettings'
  { -- | Use Aggressive mode for content that has complex motion. Higher values produce stronger temporal filtering. This filters highly complex scenes more aggressively and creates better VQ for low bitrate outputs.
    aggressiveMode :: Core.Maybe Core.Natural,
    -- | Optional. When you set Noise reducer (noiseReducer) to Temporal (TEMPORAL), you can use this setting to apply sharpening. The default behavior, Auto (AUTO), allows the transcoder to determine whether to apply filtering, depending on input type and quality. When you set Noise reducer to Temporal, your output bandwidth is reduced. When Post temporal sharpening is also enabled, that bandwidth reduction is smaller.
    postTemporalSharpening :: Core.Maybe Types.NoiseFilterPostTemporalSharpening,
    -- | The speed of the filter (higher number is faster). Low setting reduces bit rate at the cost of transcode time, high setting improves transcode time at the cost of bit rate.
    speed :: Core.Maybe Core.Int,
    -- | Specify the strength of the noise reducing filter on this output. Higher values produce stronger filtering. We recommend the following value ranges, depending on the result that you want: * 0-2 for complexity reduction with minimal sharpness loss * 2-8 for complexity reduction with image preservation * 8-16 for a high level of complexity reduction
    strength :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NoiseReducerTemporalFilterSettings' value with any optional fields omitted.
mkNoiseReducerTemporalFilterSettings ::
  NoiseReducerTemporalFilterSettings
mkNoiseReducerTemporalFilterSettings =
  NoiseReducerTemporalFilterSettings'
    { aggressiveMode =
        Core.Nothing,
      postTemporalSharpening = Core.Nothing,
      speed = Core.Nothing,
      strength = Core.Nothing
    }

-- | Use Aggressive mode for content that has complex motion. Higher values produce stronger temporal filtering. This filters highly complex scenes more aggressively and creates better VQ for low bitrate outputs.
--
-- /Note:/ Consider using 'aggressiveMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrtfsAggressiveMode :: Lens.Lens' NoiseReducerTemporalFilterSettings (Core.Maybe Core.Natural)
nrtfsAggressiveMode = Lens.field @"aggressiveMode"
{-# DEPRECATED nrtfsAggressiveMode "Use generic-lens or generic-optics with 'aggressiveMode' instead." #-}

-- | Optional. When you set Noise reducer (noiseReducer) to Temporal (TEMPORAL), you can use this setting to apply sharpening. The default behavior, Auto (AUTO), allows the transcoder to determine whether to apply filtering, depending on input type and quality. When you set Noise reducer to Temporal, your output bandwidth is reduced. When Post temporal sharpening is also enabled, that bandwidth reduction is smaller.
--
-- /Note:/ Consider using 'postTemporalSharpening' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrtfsPostTemporalSharpening :: Lens.Lens' NoiseReducerTemporalFilterSettings (Core.Maybe Types.NoiseFilterPostTemporalSharpening)
nrtfsPostTemporalSharpening = Lens.field @"postTemporalSharpening"
{-# DEPRECATED nrtfsPostTemporalSharpening "Use generic-lens or generic-optics with 'postTemporalSharpening' instead." #-}

-- | The speed of the filter (higher number is faster). Low setting reduces bit rate at the cost of transcode time, high setting improves transcode time at the cost of bit rate.
--
-- /Note:/ Consider using 'speed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrtfsSpeed :: Lens.Lens' NoiseReducerTemporalFilterSettings (Core.Maybe Core.Int)
nrtfsSpeed = Lens.field @"speed"
{-# DEPRECATED nrtfsSpeed "Use generic-lens or generic-optics with 'speed' instead." #-}

-- | Specify the strength of the noise reducing filter on this output. Higher values produce stronger filtering. We recommend the following value ranges, depending on the result that you want: * 0-2 for complexity reduction with minimal sharpness loss * 2-8 for complexity reduction with image preservation * 8-16 for a high level of complexity reduction
--
-- /Note:/ Consider using 'strength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrtfsStrength :: Lens.Lens' NoiseReducerTemporalFilterSettings (Core.Maybe Core.Natural)
nrtfsStrength = Lens.field @"strength"
{-# DEPRECATED nrtfsStrength "Use generic-lens or generic-optics with 'strength' instead." #-}

instance Core.FromJSON NoiseReducerTemporalFilterSettings where
  toJSON NoiseReducerTemporalFilterSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("aggressiveMode" Core..=) Core.<$> aggressiveMode,
            ("postTemporalSharpening" Core..=) Core.<$> postTemporalSharpening,
            ("speed" Core..=) Core.<$> speed,
            ("strength" Core..=) Core.<$> strength
          ]
      )

instance Core.FromJSON NoiseReducerTemporalFilterSettings where
  parseJSON =
    Core.withObject "NoiseReducerTemporalFilterSettings" Core.$
      \x ->
        NoiseReducerTemporalFilterSettings'
          Core.<$> (x Core..:? "aggressiveMode")
          Core.<*> (x Core..:? "postTemporalSharpening")
          Core.<*> (x Core..:? "speed")
          Core.<*> (x Core..:? "strength")
