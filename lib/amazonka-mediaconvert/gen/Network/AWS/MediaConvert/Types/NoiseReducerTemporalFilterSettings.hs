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
    nrtfsPostTemporalSharpening,
    nrtfsAggressiveMode,
    nrtfsStrength,
    nrtfsSpeed,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.NoiseFilterPostTemporalSharpening
import qualified Network.AWS.Prelude as Lude

-- | Noise reducer filter settings for temporal filter.
--
-- /See:/ 'mkNoiseReducerTemporalFilterSettings' smart constructor.
data NoiseReducerTemporalFilterSettings = NoiseReducerTemporalFilterSettings'
  { -- | Optional. When you set Noise reducer (noiseReducer) to Temporal (TEMPORAL), you can use this setting to apply sharpening. The default behavior, Auto (AUTO), allows the transcoder to determine whether to apply filtering, depending on input type and quality. When you set Noise reducer to Temporal, your output bandwidth is reduced. When Post temporal sharpening is also enabled, that bandwidth reduction is smaller.
    postTemporalSharpening :: Lude.Maybe NoiseFilterPostTemporalSharpening,
    -- | Use Aggressive mode for content that has complex motion. Higher values produce stronger temporal filtering. This filters highly complex scenes more aggressively and creates better VQ for low bitrate outputs.
    aggressiveMode :: Lude.Maybe Lude.Natural,
    -- | Specify the strength of the noise reducing filter on this output. Higher values produce stronger filtering. We recommend the following value ranges, depending on the result that you want: * 0-2 for complexity reduction with minimal sharpness loss * 2-8 for complexity reduction with image preservation * 8-16 for a high level of complexity reduction
    strength :: Lude.Maybe Lude.Natural,
    -- | The speed of the filter (higher number is faster). Low setting reduces bit rate at the cost of transcode time, high setting improves transcode time at the cost of bit rate.
    speed :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NoiseReducerTemporalFilterSettings' with the minimum fields required to make a request.
--
-- * 'postTemporalSharpening' - Optional. When you set Noise reducer (noiseReducer) to Temporal (TEMPORAL), you can use this setting to apply sharpening. The default behavior, Auto (AUTO), allows the transcoder to determine whether to apply filtering, depending on input type and quality. When you set Noise reducer to Temporal, your output bandwidth is reduced. When Post temporal sharpening is also enabled, that bandwidth reduction is smaller.
-- * 'aggressiveMode' - Use Aggressive mode for content that has complex motion. Higher values produce stronger temporal filtering. This filters highly complex scenes more aggressively and creates better VQ for low bitrate outputs.
-- * 'strength' - Specify the strength of the noise reducing filter on this output. Higher values produce stronger filtering. We recommend the following value ranges, depending on the result that you want: * 0-2 for complexity reduction with minimal sharpness loss * 2-8 for complexity reduction with image preservation * 8-16 for a high level of complexity reduction
-- * 'speed' - The speed of the filter (higher number is faster). Low setting reduces bit rate at the cost of transcode time, high setting improves transcode time at the cost of bit rate.
mkNoiseReducerTemporalFilterSettings ::
  NoiseReducerTemporalFilterSettings
mkNoiseReducerTemporalFilterSettings =
  NoiseReducerTemporalFilterSettings'
    { postTemporalSharpening =
        Lude.Nothing,
      aggressiveMode = Lude.Nothing,
      strength = Lude.Nothing,
      speed = Lude.Nothing
    }

-- | Optional. When you set Noise reducer (noiseReducer) to Temporal (TEMPORAL), you can use this setting to apply sharpening. The default behavior, Auto (AUTO), allows the transcoder to determine whether to apply filtering, depending on input type and quality. When you set Noise reducer to Temporal, your output bandwidth is reduced. When Post temporal sharpening is also enabled, that bandwidth reduction is smaller.
--
-- /Note:/ Consider using 'postTemporalSharpening' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrtfsPostTemporalSharpening :: Lens.Lens' NoiseReducerTemporalFilterSettings (Lude.Maybe NoiseFilterPostTemporalSharpening)
nrtfsPostTemporalSharpening = Lens.lens (postTemporalSharpening :: NoiseReducerTemporalFilterSettings -> Lude.Maybe NoiseFilterPostTemporalSharpening) (\s a -> s {postTemporalSharpening = a} :: NoiseReducerTemporalFilterSettings)
{-# DEPRECATED nrtfsPostTemporalSharpening "Use generic-lens or generic-optics with 'postTemporalSharpening' instead." #-}

-- | Use Aggressive mode for content that has complex motion. Higher values produce stronger temporal filtering. This filters highly complex scenes more aggressively and creates better VQ for low bitrate outputs.
--
-- /Note:/ Consider using 'aggressiveMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrtfsAggressiveMode :: Lens.Lens' NoiseReducerTemporalFilterSettings (Lude.Maybe Lude.Natural)
nrtfsAggressiveMode = Lens.lens (aggressiveMode :: NoiseReducerTemporalFilterSettings -> Lude.Maybe Lude.Natural) (\s a -> s {aggressiveMode = a} :: NoiseReducerTemporalFilterSettings)
{-# DEPRECATED nrtfsAggressiveMode "Use generic-lens or generic-optics with 'aggressiveMode' instead." #-}

-- | Specify the strength of the noise reducing filter on this output. Higher values produce stronger filtering. We recommend the following value ranges, depending on the result that you want: * 0-2 for complexity reduction with minimal sharpness loss * 2-8 for complexity reduction with image preservation * 8-16 for a high level of complexity reduction
--
-- /Note:/ Consider using 'strength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrtfsStrength :: Lens.Lens' NoiseReducerTemporalFilterSettings (Lude.Maybe Lude.Natural)
nrtfsStrength = Lens.lens (strength :: NoiseReducerTemporalFilterSettings -> Lude.Maybe Lude.Natural) (\s a -> s {strength = a} :: NoiseReducerTemporalFilterSettings)
{-# DEPRECATED nrtfsStrength "Use generic-lens or generic-optics with 'strength' instead." #-}

-- | The speed of the filter (higher number is faster). Low setting reduces bit rate at the cost of transcode time, high setting improves transcode time at the cost of bit rate.
--
-- /Note:/ Consider using 'speed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrtfsSpeed :: Lens.Lens' NoiseReducerTemporalFilterSettings (Lude.Maybe Lude.Int)
nrtfsSpeed = Lens.lens (speed :: NoiseReducerTemporalFilterSettings -> Lude.Maybe Lude.Int) (\s a -> s {speed = a} :: NoiseReducerTemporalFilterSettings)
{-# DEPRECATED nrtfsSpeed "Use generic-lens or generic-optics with 'speed' instead." #-}

instance Lude.FromJSON NoiseReducerTemporalFilterSettings where
  parseJSON =
    Lude.withObject
      "NoiseReducerTemporalFilterSettings"
      ( \x ->
          NoiseReducerTemporalFilterSettings'
            Lude.<$> (x Lude..:? "postTemporalSharpening")
            Lude.<*> (x Lude..:? "aggressiveMode")
            Lude.<*> (x Lude..:? "strength")
            Lude.<*> (x Lude..:? "speed")
      )

instance Lude.ToJSON NoiseReducerTemporalFilterSettings where
  toJSON NoiseReducerTemporalFilterSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("postTemporalSharpening" Lude..=)
              Lude.<$> postTemporalSharpening,
            ("aggressiveMode" Lude..=) Lude.<$> aggressiveMode,
            ("strength" Lude..=) Lude.<$> strength,
            ("speed" Lude..=) Lude.<$> speed
          ]
      )
