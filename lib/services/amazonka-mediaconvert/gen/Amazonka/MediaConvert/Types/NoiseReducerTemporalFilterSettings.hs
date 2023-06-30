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
-- Module      : Amazonka.MediaConvert.Types.NoiseReducerTemporalFilterSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.NoiseReducerTemporalFilterSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.NoiseFilterPostTemporalSharpening
import Amazonka.MediaConvert.Types.NoiseFilterPostTemporalSharpeningStrength
import qualified Amazonka.Prelude as Prelude

-- | Noise reducer filter settings for temporal filter.
--
-- /See:/ 'newNoiseReducerTemporalFilterSettings' smart constructor.
data NoiseReducerTemporalFilterSettings = NoiseReducerTemporalFilterSettings'
  { -- | Use Aggressive mode for content that has complex motion. Higher values
    -- produce stronger temporal filtering. This filters highly complex scenes
    -- more aggressively and creates better VQ for low bitrate outputs.
    aggressiveMode :: Prelude.Maybe Prelude.Natural,
    -- | When you set Noise reducer (noiseReducer) to Temporal (TEMPORAL), the
    -- bandwidth and sharpness of your output is reduced. You can optionally
    -- use Post temporal sharpening (postTemporalSharpening) to apply
    -- sharpening to the edges of your output. Note that Post temporal
    -- sharpening will also make the bandwidth reduction from the Noise reducer
    -- smaller. The default behavior, Auto (AUTO), allows the transcoder to
    -- determine whether to apply sharpening, depending on your input type and
    -- quality. When you set Post temporal sharpening to Enabled (ENABLED),
    -- specify how much sharpening is applied using Post temporal sharpening
    -- strength (postTemporalSharpeningStrength). Set Post temporal sharpening
    -- to Disabled (DISABLED) to not apply sharpening.
    postTemporalSharpening :: Prelude.Maybe NoiseFilterPostTemporalSharpening,
    -- | Use Post temporal sharpening strength (postTemporalSharpeningStrength)
    -- to define the amount of sharpening the transcoder applies to your
    -- output. Set Post temporal sharpening strength to Low (LOW), Medium
    -- (MEDIUM), or High (HIGH) to indicate the amount of sharpening.
    postTemporalSharpeningStrength :: Prelude.Maybe NoiseFilterPostTemporalSharpeningStrength,
    -- | The speed of the filter (higher number is faster). Low setting reduces
    -- bit rate at the cost of transcode time, high setting improves transcode
    -- time at the cost of bit rate.
    speed :: Prelude.Maybe Prelude.Int,
    -- | Specify the strength of the noise reducing filter on this output. Higher
    -- values produce stronger filtering. We recommend the following value
    -- ranges, depending on the result that you want: * 0-2 for complexity
    -- reduction with minimal sharpness loss * 2-8 for complexity reduction
    -- with image preservation * 8-16 for a high level of complexity reduction
    strength :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NoiseReducerTemporalFilterSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggressiveMode', 'noiseReducerTemporalFilterSettings_aggressiveMode' - Use Aggressive mode for content that has complex motion. Higher values
-- produce stronger temporal filtering. This filters highly complex scenes
-- more aggressively and creates better VQ for low bitrate outputs.
--
-- 'postTemporalSharpening', 'noiseReducerTemporalFilterSettings_postTemporalSharpening' - When you set Noise reducer (noiseReducer) to Temporal (TEMPORAL), the
-- bandwidth and sharpness of your output is reduced. You can optionally
-- use Post temporal sharpening (postTemporalSharpening) to apply
-- sharpening to the edges of your output. Note that Post temporal
-- sharpening will also make the bandwidth reduction from the Noise reducer
-- smaller. The default behavior, Auto (AUTO), allows the transcoder to
-- determine whether to apply sharpening, depending on your input type and
-- quality. When you set Post temporal sharpening to Enabled (ENABLED),
-- specify how much sharpening is applied using Post temporal sharpening
-- strength (postTemporalSharpeningStrength). Set Post temporal sharpening
-- to Disabled (DISABLED) to not apply sharpening.
--
-- 'postTemporalSharpeningStrength', 'noiseReducerTemporalFilterSettings_postTemporalSharpeningStrength' - Use Post temporal sharpening strength (postTemporalSharpeningStrength)
-- to define the amount of sharpening the transcoder applies to your
-- output. Set Post temporal sharpening strength to Low (LOW), Medium
-- (MEDIUM), or High (HIGH) to indicate the amount of sharpening.
--
-- 'speed', 'noiseReducerTemporalFilterSettings_speed' - The speed of the filter (higher number is faster). Low setting reduces
-- bit rate at the cost of transcode time, high setting improves transcode
-- time at the cost of bit rate.
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
    { aggressiveMode =
        Prelude.Nothing,
      postTemporalSharpening =
        Prelude.Nothing,
      postTemporalSharpeningStrength =
        Prelude.Nothing,
      speed = Prelude.Nothing,
      strength = Prelude.Nothing
    }

-- | Use Aggressive mode for content that has complex motion. Higher values
-- produce stronger temporal filtering. This filters highly complex scenes
-- more aggressively and creates better VQ for low bitrate outputs.
noiseReducerTemporalFilterSettings_aggressiveMode :: Lens.Lens' NoiseReducerTemporalFilterSettings (Prelude.Maybe Prelude.Natural)
noiseReducerTemporalFilterSettings_aggressiveMode = Lens.lens (\NoiseReducerTemporalFilterSettings' {aggressiveMode} -> aggressiveMode) (\s@NoiseReducerTemporalFilterSettings' {} a -> s {aggressiveMode = a} :: NoiseReducerTemporalFilterSettings)

-- | When you set Noise reducer (noiseReducer) to Temporal (TEMPORAL), the
-- bandwidth and sharpness of your output is reduced. You can optionally
-- use Post temporal sharpening (postTemporalSharpening) to apply
-- sharpening to the edges of your output. Note that Post temporal
-- sharpening will also make the bandwidth reduction from the Noise reducer
-- smaller. The default behavior, Auto (AUTO), allows the transcoder to
-- determine whether to apply sharpening, depending on your input type and
-- quality. When you set Post temporal sharpening to Enabled (ENABLED),
-- specify how much sharpening is applied using Post temporal sharpening
-- strength (postTemporalSharpeningStrength). Set Post temporal sharpening
-- to Disabled (DISABLED) to not apply sharpening.
noiseReducerTemporalFilterSettings_postTemporalSharpening :: Lens.Lens' NoiseReducerTemporalFilterSettings (Prelude.Maybe NoiseFilterPostTemporalSharpening)
noiseReducerTemporalFilterSettings_postTemporalSharpening = Lens.lens (\NoiseReducerTemporalFilterSettings' {postTemporalSharpening} -> postTemporalSharpening) (\s@NoiseReducerTemporalFilterSettings' {} a -> s {postTemporalSharpening = a} :: NoiseReducerTemporalFilterSettings)

-- | Use Post temporal sharpening strength (postTemporalSharpeningStrength)
-- to define the amount of sharpening the transcoder applies to your
-- output. Set Post temporal sharpening strength to Low (LOW), Medium
-- (MEDIUM), or High (HIGH) to indicate the amount of sharpening.
noiseReducerTemporalFilterSettings_postTemporalSharpeningStrength :: Lens.Lens' NoiseReducerTemporalFilterSettings (Prelude.Maybe NoiseFilterPostTemporalSharpeningStrength)
noiseReducerTemporalFilterSettings_postTemporalSharpeningStrength = Lens.lens (\NoiseReducerTemporalFilterSettings' {postTemporalSharpeningStrength} -> postTemporalSharpeningStrength) (\s@NoiseReducerTemporalFilterSettings' {} a -> s {postTemporalSharpeningStrength = a} :: NoiseReducerTemporalFilterSettings)

-- | The speed of the filter (higher number is faster). Low setting reduces
-- bit rate at the cost of transcode time, high setting improves transcode
-- time at the cost of bit rate.
noiseReducerTemporalFilterSettings_speed :: Lens.Lens' NoiseReducerTemporalFilterSettings (Prelude.Maybe Prelude.Int)
noiseReducerTemporalFilterSettings_speed = Lens.lens (\NoiseReducerTemporalFilterSettings' {speed} -> speed) (\s@NoiseReducerTemporalFilterSettings' {} a -> s {speed = a} :: NoiseReducerTemporalFilterSettings)

-- | Specify the strength of the noise reducing filter on this output. Higher
-- values produce stronger filtering. We recommend the following value
-- ranges, depending on the result that you want: * 0-2 for complexity
-- reduction with minimal sharpness loss * 2-8 for complexity reduction
-- with image preservation * 8-16 for a high level of complexity reduction
noiseReducerTemporalFilterSettings_strength :: Lens.Lens' NoiseReducerTemporalFilterSettings (Prelude.Maybe Prelude.Natural)
noiseReducerTemporalFilterSettings_strength = Lens.lens (\NoiseReducerTemporalFilterSettings' {strength} -> strength) (\s@NoiseReducerTemporalFilterSettings' {} a -> s {strength = a} :: NoiseReducerTemporalFilterSettings)

instance
  Data.FromJSON
    NoiseReducerTemporalFilterSettings
  where
  parseJSON =
    Data.withObject
      "NoiseReducerTemporalFilterSettings"
      ( \x ->
          NoiseReducerTemporalFilterSettings'
            Prelude.<$> (x Data..:? "aggressiveMode")
            Prelude.<*> (x Data..:? "postTemporalSharpening")
            Prelude.<*> (x Data..:? "postTemporalSharpeningStrength")
            Prelude.<*> (x Data..:? "speed")
            Prelude.<*> (x Data..:? "strength")
      )

instance
  Prelude.Hashable
    NoiseReducerTemporalFilterSettings
  where
  hashWithSalt
    _salt
    NoiseReducerTemporalFilterSettings' {..} =
      _salt
        `Prelude.hashWithSalt` aggressiveMode
        `Prelude.hashWithSalt` postTemporalSharpening
        `Prelude.hashWithSalt` postTemporalSharpeningStrength
        `Prelude.hashWithSalt` speed
        `Prelude.hashWithSalt` strength

instance
  Prelude.NFData
    NoiseReducerTemporalFilterSettings
  where
  rnf NoiseReducerTemporalFilterSettings' {..} =
    Prelude.rnf aggressiveMode
      `Prelude.seq` Prelude.rnf postTemporalSharpening
      `Prelude.seq` Prelude.rnf postTemporalSharpeningStrength
      `Prelude.seq` Prelude.rnf speed
      `Prelude.seq` Prelude.rnf strength

instance
  Data.ToJSON
    NoiseReducerTemporalFilterSettings
  where
  toJSON NoiseReducerTemporalFilterSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("aggressiveMode" Data..=)
              Prelude.<$> aggressiveMode,
            ("postTemporalSharpening" Data..=)
              Prelude.<$> postTemporalSharpening,
            ("postTemporalSharpeningStrength" Data..=)
              Prelude.<$> postTemporalSharpeningStrength,
            ("speed" Data..=) Prelude.<$> speed,
            ("strength" Data..=) Prelude.<$> strength
          ]
      )
