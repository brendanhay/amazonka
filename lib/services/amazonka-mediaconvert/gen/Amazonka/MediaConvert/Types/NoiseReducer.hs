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
-- Module      : Amazonka.MediaConvert.Types.NoiseReducer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.NoiseReducer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConvert.Types.NoiseReducerFilter
import Amazonka.MediaConvert.Types.NoiseReducerFilterSettings
import Amazonka.MediaConvert.Types.NoiseReducerSpatialFilterSettings
import Amazonka.MediaConvert.Types.NoiseReducerTemporalFilterSettings
import qualified Amazonka.Prelude as Prelude

-- | Enable the Noise reducer (NoiseReducer) feature to remove noise from
-- your video output if necessary. Enable or disable this feature for each
-- output individually. This setting is disabled by default. When you
-- enable Noise reducer (NoiseReducer), you must also select a value for
-- Noise reducer filter (NoiseReducerFilter).
--
-- /See:/ 'newNoiseReducer' smart constructor.
data NoiseReducer = NoiseReducer'
  { -- | Noise reducer filter settings for temporal filter.
    temporalFilterSettings :: Prelude.Maybe NoiseReducerTemporalFilterSettings,
    -- | Noise reducer filter settings for spatial filter.
    spatialFilterSettings :: Prelude.Maybe NoiseReducerSpatialFilterSettings,
    -- | Settings for a noise reducer filter
    filterSettings :: Prelude.Maybe NoiseReducerFilterSettings,
    -- | Use Noise reducer filter (NoiseReducerFilter) to select one of the
    -- following spatial image filtering functions. To use this setting, you
    -- must also enable Noise reducer (NoiseReducer). * Bilateral preserves
    -- edges while reducing noise. * Mean (softest), Gaussian, Lanczos, and
    -- Sharpen (sharpest) do convolution filtering. * Conserve does min\/max
    -- noise reduction. * Spatial does frequency-domain filtering based on JND
    -- principles. * Temporal optimizes video quality for complex motion.
    filter' :: Prelude.Maybe NoiseReducerFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NoiseReducer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'temporalFilterSettings', 'noiseReducer_temporalFilterSettings' - Noise reducer filter settings for temporal filter.
--
-- 'spatialFilterSettings', 'noiseReducer_spatialFilterSettings' - Noise reducer filter settings for spatial filter.
--
-- 'filterSettings', 'noiseReducer_filterSettings' - Settings for a noise reducer filter
--
-- 'filter'', 'noiseReducer_filter' - Use Noise reducer filter (NoiseReducerFilter) to select one of the
-- following spatial image filtering functions. To use this setting, you
-- must also enable Noise reducer (NoiseReducer). * Bilateral preserves
-- edges while reducing noise. * Mean (softest), Gaussian, Lanczos, and
-- Sharpen (sharpest) do convolution filtering. * Conserve does min\/max
-- noise reduction. * Spatial does frequency-domain filtering based on JND
-- principles. * Temporal optimizes video quality for complex motion.
newNoiseReducer ::
  NoiseReducer
newNoiseReducer =
  NoiseReducer'
    { temporalFilterSettings =
        Prelude.Nothing,
      spatialFilterSettings = Prelude.Nothing,
      filterSettings = Prelude.Nothing,
      filter' = Prelude.Nothing
    }

-- | Noise reducer filter settings for temporal filter.
noiseReducer_temporalFilterSettings :: Lens.Lens' NoiseReducer (Prelude.Maybe NoiseReducerTemporalFilterSettings)
noiseReducer_temporalFilterSettings = Lens.lens (\NoiseReducer' {temporalFilterSettings} -> temporalFilterSettings) (\s@NoiseReducer' {} a -> s {temporalFilterSettings = a} :: NoiseReducer)

-- | Noise reducer filter settings for spatial filter.
noiseReducer_spatialFilterSettings :: Lens.Lens' NoiseReducer (Prelude.Maybe NoiseReducerSpatialFilterSettings)
noiseReducer_spatialFilterSettings = Lens.lens (\NoiseReducer' {spatialFilterSettings} -> spatialFilterSettings) (\s@NoiseReducer' {} a -> s {spatialFilterSettings = a} :: NoiseReducer)

-- | Settings for a noise reducer filter
noiseReducer_filterSettings :: Lens.Lens' NoiseReducer (Prelude.Maybe NoiseReducerFilterSettings)
noiseReducer_filterSettings = Lens.lens (\NoiseReducer' {filterSettings} -> filterSettings) (\s@NoiseReducer' {} a -> s {filterSettings = a} :: NoiseReducer)

-- | Use Noise reducer filter (NoiseReducerFilter) to select one of the
-- following spatial image filtering functions. To use this setting, you
-- must also enable Noise reducer (NoiseReducer). * Bilateral preserves
-- edges while reducing noise. * Mean (softest), Gaussian, Lanczos, and
-- Sharpen (sharpest) do convolution filtering. * Conserve does min\/max
-- noise reduction. * Spatial does frequency-domain filtering based on JND
-- principles. * Temporal optimizes video quality for complex motion.
noiseReducer_filter :: Lens.Lens' NoiseReducer (Prelude.Maybe NoiseReducerFilter)
noiseReducer_filter = Lens.lens (\NoiseReducer' {filter'} -> filter') (\s@NoiseReducer' {} a -> s {filter' = a} :: NoiseReducer)

instance Core.FromJSON NoiseReducer where
  parseJSON =
    Core.withObject
      "NoiseReducer"
      ( \x ->
          NoiseReducer'
            Prelude.<$> (x Core..:? "temporalFilterSettings")
            Prelude.<*> (x Core..:? "spatialFilterSettings")
            Prelude.<*> (x Core..:? "filterSettings")
            Prelude.<*> (x Core..:? "filter")
      )

instance Prelude.Hashable NoiseReducer where
  hashWithSalt _salt NoiseReducer' {..} =
    _salt `Prelude.hashWithSalt` temporalFilterSettings
      `Prelude.hashWithSalt` spatialFilterSettings
      `Prelude.hashWithSalt` filterSettings
      `Prelude.hashWithSalt` filter'

instance Prelude.NFData NoiseReducer where
  rnf NoiseReducer' {..} =
    Prelude.rnf temporalFilterSettings
      `Prelude.seq` Prelude.rnf spatialFilterSettings
      `Prelude.seq` Prelude.rnf filterSettings
      `Prelude.seq` Prelude.rnf filter'

instance Core.ToJSON NoiseReducer where
  toJSON NoiseReducer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("temporalFilterSettings" Core..=)
              Prelude.<$> temporalFilterSettings,
            ("spatialFilterSettings" Core..=)
              Prelude.<$> spatialFilterSettings,
            ("filterSettings" Core..=)
              Prelude.<$> filterSettings,
            ("filter" Core..=) Prelude.<$> filter'
          ]
      )
