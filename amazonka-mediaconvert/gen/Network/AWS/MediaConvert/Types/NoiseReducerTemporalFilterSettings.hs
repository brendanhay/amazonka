{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NoiseReducerTemporalFilterSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NoiseReducerTemporalFilterSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.NoiseFilterPostTemporalSharpening
import qualified Network.AWS.Prelude as Prelude

-- | Noise reducer filter settings for temporal filter.
--
-- /See:/ 'newNoiseReducerTemporalFilterSettings' smart constructor.
data NoiseReducerTemporalFilterSettings = NoiseReducerTemporalFilterSettings'
  { -- | Optional. When you set Noise reducer (noiseReducer) to Temporal
    -- (TEMPORAL), you can use this setting to apply sharpening. The default
    -- behavior, Auto (AUTO), allows the transcoder to determine whether to
    -- apply filtering, depending on input type and quality. When you set Noise
    -- reducer to Temporal, your output bandwidth is reduced. When Post
    -- temporal sharpening is also enabled, that bandwidth reduction is
    -- smaller.
    postTemporalSharpening :: Prelude.Maybe NoiseFilterPostTemporalSharpening,
    -- | The speed of the filter (higher number is faster). Low setting reduces
    -- bit rate at the cost of transcode time, high setting improves transcode
    -- time at the cost of bit rate.
    speed :: Prelude.Maybe Prelude.Int,
    -- | Use Aggressive mode for content that has complex motion. Higher values
    -- produce stronger temporal filtering. This filters highly complex scenes
    -- more aggressively and creates better VQ for low bitrate outputs.
    aggressiveMode :: Prelude.Maybe Prelude.Natural,
    -- | Specify the strength of the noise reducing filter on this output. Higher
    -- values produce stronger filtering. We recommend the following value
    -- ranges, depending on the result that you want: * 0-2 for complexity
    -- reduction with minimal sharpness loss * 2-8 for complexity reduction
    -- with image preservation * 8-16 for a high level of complexity reduction
    strength :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NoiseReducerTemporalFilterSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'postTemporalSharpening', 'noiseReducerTemporalFilterSettings_postTemporalSharpening' - Optional. When you set Noise reducer (noiseReducer) to Temporal
-- (TEMPORAL), you can use this setting to apply sharpening. The default
-- behavior, Auto (AUTO), allows the transcoder to determine whether to
-- apply filtering, depending on input type and quality. When you set Noise
-- reducer to Temporal, your output bandwidth is reduced. When Post
-- temporal sharpening is also enabled, that bandwidth reduction is
-- smaller.
--
-- 'speed', 'noiseReducerTemporalFilterSettings_speed' - The speed of the filter (higher number is faster). Low setting reduces
-- bit rate at the cost of transcode time, high setting improves transcode
-- time at the cost of bit rate.
--
-- 'aggressiveMode', 'noiseReducerTemporalFilterSettings_aggressiveMode' - Use Aggressive mode for content that has complex motion. Higher values
-- produce stronger temporal filtering. This filters highly complex scenes
-- more aggressively and creates better VQ for low bitrate outputs.
--
-- 'strength', 'noiseReducerTemporalFilterSettings_strength' - Specify the strength of the noise reducing filter on this output. Higher
-- values produce stronger filtering. We recommend the following value
-- ranges, depending on the result that you want: * 0-2 for complexity
-- reduction with minimal sharpness loss * 2-8 for complexity reduction
-- with image preservation * 8-16 for a high level of complexity reduction
newNoiseReducerTemporalFilterSettings ::
  NoiseReducerTemporalFilterSettings
newNoiseReducerTemporalFilterSettings =
  NoiseReducerTemporalFilterSettings'
    { postTemporalSharpening =
        Prelude.Nothing,
      speed = Prelude.Nothing,
      aggressiveMode = Prelude.Nothing,
      strength = Prelude.Nothing
    }

-- | Optional. When you set Noise reducer (noiseReducer) to Temporal
-- (TEMPORAL), you can use this setting to apply sharpening. The default
-- behavior, Auto (AUTO), allows the transcoder to determine whether to
-- apply filtering, depending on input type and quality. When you set Noise
-- reducer to Temporal, your output bandwidth is reduced. When Post
-- temporal sharpening is also enabled, that bandwidth reduction is
-- smaller.
noiseReducerTemporalFilterSettings_postTemporalSharpening :: Lens.Lens' NoiseReducerTemporalFilterSettings (Prelude.Maybe NoiseFilterPostTemporalSharpening)
noiseReducerTemporalFilterSettings_postTemporalSharpening = Lens.lens (\NoiseReducerTemporalFilterSettings' {postTemporalSharpening} -> postTemporalSharpening) (\s@NoiseReducerTemporalFilterSettings' {} a -> s {postTemporalSharpening = a} :: NoiseReducerTemporalFilterSettings)

-- | The speed of the filter (higher number is faster). Low setting reduces
-- bit rate at the cost of transcode time, high setting improves transcode
-- time at the cost of bit rate.
noiseReducerTemporalFilterSettings_speed :: Lens.Lens' NoiseReducerTemporalFilterSettings (Prelude.Maybe Prelude.Int)
noiseReducerTemporalFilterSettings_speed = Lens.lens (\NoiseReducerTemporalFilterSettings' {speed} -> speed) (\s@NoiseReducerTemporalFilterSettings' {} a -> s {speed = a} :: NoiseReducerTemporalFilterSettings)

-- | Use Aggressive mode for content that has complex motion. Higher values
-- produce stronger temporal filtering. This filters highly complex scenes
-- more aggressively and creates better VQ for low bitrate outputs.
noiseReducerTemporalFilterSettings_aggressiveMode :: Lens.Lens' NoiseReducerTemporalFilterSettings (Prelude.Maybe Prelude.Natural)
noiseReducerTemporalFilterSettings_aggressiveMode = Lens.lens (\NoiseReducerTemporalFilterSettings' {aggressiveMode} -> aggressiveMode) (\s@NoiseReducerTemporalFilterSettings' {} a -> s {aggressiveMode = a} :: NoiseReducerTemporalFilterSettings)

-- | Specify the strength of the noise reducing filter on this output. Higher
-- values produce stronger filtering. We recommend the following value
-- ranges, depending on the result that you want: * 0-2 for complexity
-- reduction with minimal sharpness loss * 2-8 for complexity reduction
-- with image preservation * 8-16 for a high level of complexity reduction
noiseReducerTemporalFilterSettings_strength :: Lens.Lens' NoiseReducerTemporalFilterSettings (Prelude.Maybe Prelude.Natural)
noiseReducerTemporalFilterSettings_strength = Lens.lens (\NoiseReducerTemporalFilterSettings' {strength} -> strength) (\s@NoiseReducerTemporalFilterSettings' {} a -> s {strength = a} :: NoiseReducerTemporalFilterSettings)

instance
  Prelude.FromJSON
    NoiseReducerTemporalFilterSettings
  where
  parseJSON =
    Prelude.withObject
      "NoiseReducerTemporalFilterSettings"
      ( \x ->
          NoiseReducerTemporalFilterSettings'
            Prelude.<$> (x Prelude..:? "postTemporalSharpening")
            Prelude.<*> (x Prelude..:? "speed")
            Prelude.<*> (x Prelude..:? "aggressiveMode")
            Prelude.<*> (x Prelude..:? "strength")
      )

instance
  Prelude.Hashable
    NoiseReducerTemporalFilterSettings

instance
  Prelude.NFData
    NoiseReducerTemporalFilterSettings

instance
  Prelude.ToJSON
    NoiseReducerTemporalFilterSettings
  where
  toJSON NoiseReducerTemporalFilterSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("postTemporalSharpening" Prelude..=)
              Prelude.<$> postTemporalSharpening,
            ("speed" Prelude..=) Prelude.<$> speed,
            ("aggressiveMode" Prelude..=)
              Prelude.<$> aggressiveMode,
            ("strength" Prelude..=) Prelude.<$> strength
          ]
      )
