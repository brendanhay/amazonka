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
-- Module      : Network.AWS.MediaConvert.Types.NoiseReducerSpatialFilterSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NoiseReducerSpatialFilterSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Noise reducer filter settings for spatial filter.
--
-- /See:/ 'newNoiseReducerSpatialFilterSettings' smart constructor.
data NoiseReducerSpatialFilterSettings = NoiseReducerSpatialFilterSettings'
  { -- | The speed of the filter, from -2 (lower speed) to 3 (higher speed), with
    -- 0 being the nominal value.
    speed :: Core.Maybe Core.Int,
    -- | Specify strength of post noise reduction sharpening filter, with 0
    -- disabling the filter and 3 enabling it at maximum strength.
    postFilterSharpenStrength :: Core.Maybe Core.Natural,
    -- | Relative strength of noise reducing filter. Higher values produce
    -- stronger filtering.
    strength :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NoiseReducerSpatialFilterSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'speed', 'noiseReducerSpatialFilterSettings_speed' - The speed of the filter, from -2 (lower speed) to 3 (higher speed), with
-- 0 being the nominal value.
--
-- 'postFilterSharpenStrength', 'noiseReducerSpatialFilterSettings_postFilterSharpenStrength' - Specify strength of post noise reduction sharpening filter, with 0
-- disabling the filter and 3 enabling it at maximum strength.
--
-- 'strength', 'noiseReducerSpatialFilterSettings_strength' - Relative strength of noise reducing filter. Higher values produce
-- stronger filtering.
newNoiseReducerSpatialFilterSettings ::
  NoiseReducerSpatialFilterSettings
newNoiseReducerSpatialFilterSettings =
  NoiseReducerSpatialFilterSettings'
    { speed =
        Core.Nothing,
      postFilterSharpenStrength = Core.Nothing,
      strength = Core.Nothing
    }

-- | The speed of the filter, from -2 (lower speed) to 3 (higher speed), with
-- 0 being the nominal value.
noiseReducerSpatialFilterSettings_speed :: Lens.Lens' NoiseReducerSpatialFilterSettings (Core.Maybe Core.Int)
noiseReducerSpatialFilterSettings_speed = Lens.lens (\NoiseReducerSpatialFilterSettings' {speed} -> speed) (\s@NoiseReducerSpatialFilterSettings' {} a -> s {speed = a} :: NoiseReducerSpatialFilterSettings)

-- | Specify strength of post noise reduction sharpening filter, with 0
-- disabling the filter and 3 enabling it at maximum strength.
noiseReducerSpatialFilterSettings_postFilterSharpenStrength :: Lens.Lens' NoiseReducerSpatialFilterSettings (Core.Maybe Core.Natural)
noiseReducerSpatialFilterSettings_postFilterSharpenStrength = Lens.lens (\NoiseReducerSpatialFilterSettings' {postFilterSharpenStrength} -> postFilterSharpenStrength) (\s@NoiseReducerSpatialFilterSettings' {} a -> s {postFilterSharpenStrength = a} :: NoiseReducerSpatialFilterSettings)

-- | Relative strength of noise reducing filter. Higher values produce
-- stronger filtering.
noiseReducerSpatialFilterSettings_strength :: Lens.Lens' NoiseReducerSpatialFilterSettings (Core.Maybe Core.Natural)
noiseReducerSpatialFilterSettings_strength = Lens.lens (\NoiseReducerSpatialFilterSettings' {strength} -> strength) (\s@NoiseReducerSpatialFilterSettings' {} a -> s {strength = a} :: NoiseReducerSpatialFilterSettings)

instance
  Core.FromJSON
    NoiseReducerSpatialFilterSettings
  where
  parseJSON =
    Core.withObject
      "NoiseReducerSpatialFilterSettings"
      ( \x ->
          NoiseReducerSpatialFilterSettings'
            Core.<$> (x Core..:? "speed")
            Core.<*> (x Core..:? "postFilterSharpenStrength")
            Core.<*> (x Core..:? "strength")
      )

instance
  Core.Hashable
    NoiseReducerSpatialFilterSettings

instance
  Core.NFData
    NoiseReducerSpatialFilterSettings

instance
  Core.ToJSON
    NoiseReducerSpatialFilterSettings
  where
  toJSON NoiseReducerSpatialFilterSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("speed" Core..=) Core.<$> speed,
            ("postFilterSharpenStrength" Core..=)
              Core.<$> postFilterSharpenStrength,
            ("strength" Core..=) Core.<$> strength
          ]
      )
