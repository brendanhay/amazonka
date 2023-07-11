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
-- Module      : Amazonka.MediaConvert.Types.NoiseReducerSpatialFilterSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.NoiseReducerSpatialFilterSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Noise reducer filter settings for spatial filter.
--
-- /See:/ 'newNoiseReducerSpatialFilterSettings' smart constructor.
data NoiseReducerSpatialFilterSettings = NoiseReducerSpatialFilterSettings'
  { -- | Specify strength of post noise reduction sharpening filter, with 0
    -- disabling the filter and 3 enabling it at maximum strength.
    postFilterSharpenStrength :: Prelude.Maybe Prelude.Natural,
    -- | The speed of the filter, from -2 (lower speed) to 3 (higher speed), with
    -- 0 being the nominal value.
    speed :: Prelude.Maybe Prelude.Int,
    -- | Relative strength of noise reducing filter. Higher values produce
    -- stronger filtering.
    strength :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NoiseReducerSpatialFilterSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'postFilterSharpenStrength', 'noiseReducerSpatialFilterSettings_postFilterSharpenStrength' - Specify strength of post noise reduction sharpening filter, with 0
-- disabling the filter and 3 enabling it at maximum strength.
--
-- 'speed', 'noiseReducerSpatialFilterSettings_speed' - The speed of the filter, from -2 (lower speed) to 3 (higher speed), with
-- 0 being the nominal value.
--
-- 'strength', 'noiseReducerSpatialFilterSettings_strength' - Relative strength of noise reducing filter. Higher values produce
-- stronger filtering.
newNoiseReducerSpatialFilterSettings ::
  NoiseReducerSpatialFilterSettings
newNoiseReducerSpatialFilterSettings =
  NoiseReducerSpatialFilterSettings'
    { postFilterSharpenStrength =
        Prelude.Nothing,
      speed = Prelude.Nothing,
      strength = Prelude.Nothing
    }

-- | Specify strength of post noise reduction sharpening filter, with 0
-- disabling the filter and 3 enabling it at maximum strength.
noiseReducerSpatialFilterSettings_postFilterSharpenStrength :: Lens.Lens' NoiseReducerSpatialFilterSettings (Prelude.Maybe Prelude.Natural)
noiseReducerSpatialFilterSettings_postFilterSharpenStrength = Lens.lens (\NoiseReducerSpatialFilterSettings' {postFilterSharpenStrength} -> postFilterSharpenStrength) (\s@NoiseReducerSpatialFilterSettings' {} a -> s {postFilterSharpenStrength = a} :: NoiseReducerSpatialFilterSettings)

-- | The speed of the filter, from -2 (lower speed) to 3 (higher speed), with
-- 0 being the nominal value.
noiseReducerSpatialFilterSettings_speed :: Lens.Lens' NoiseReducerSpatialFilterSettings (Prelude.Maybe Prelude.Int)
noiseReducerSpatialFilterSettings_speed = Lens.lens (\NoiseReducerSpatialFilterSettings' {speed} -> speed) (\s@NoiseReducerSpatialFilterSettings' {} a -> s {speed = a} :: NoiseReducerSpatialFilterSettings)

-- | Relative strength of noise reducing filter. Higher values produce
-- stronger filtering.
noiseReducerSpatialFilterSettings_strength :: Lens.Lens' NoiseReducerSpatialFilterSettings (Prelude.Maybe Prelude.Natural)
noiseReducerSpatialFilterSettings_strength = Lens.lens (\NoiseReducerSpatialFilterSettings' {strength} -> strength) (\s@NoiseReducerSpatialFilterSettings' {} a -> s {strength = a} :: NoiseReducerSpatialFilterSettings)

instance
  Data.FromJSON
    NoiseReducerSpatialFilterSettings
  where
  parseJSON =
    Data.withObject
      "NoiseReducerSpatialFilterSettings"
      ( \x ->
          NoiseReducerSpatialFilterSettings'
            Prelude.<$> (x Data..:? "postFilterSharpenStrength")
            Prelude.<*> (x Data..:? "speed")
            Prelude.<*> (x Data..:? "strength")
      )

instance
  Prelude.Hashable
    NoiseReducerSpatialFilterSettings
  where
  hashWithSalt
    _salt
    NoiseReducerSpatialFilterSettings' {..} =
      _salt
        `Prelude.hashWithSalt` postFilterSharpenStrength
        `Prelude.hashWithSalt` speed
        `Prelude.hashWithSalt` strength

instance
  Prelude.NFData
    NoiseReducerSpatialFilterSettings
  where
  rnf NoiseReducerSpatialFilterSettings' {..} =
    Prelude.rnf postFilterSharpenStrength
      `Prelude.seq` Prelude.rnf speed
      `Prelude.seq` Prelude.rnf strength

instance
  Data.ToJSON
    NoiseReducerSpatialFilterSettings
  where
  toJSON NoiseReducerSpatialFilterSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("postFilterSharpenStrength" Data..=)
              Prelude.<$> postFilterSharpenStrength,
            ("speed" Data..=) Prelude.<$> speed,
            ("strength" Data..=) Prelude.<$> strength
          ]
      )
