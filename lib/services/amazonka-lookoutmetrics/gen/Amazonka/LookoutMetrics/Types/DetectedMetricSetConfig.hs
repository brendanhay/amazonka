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
-- Module      : Amazonka.LookoutMetrics.Types.DetectedMetricSetConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.DetectedMetricSetConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.DetectedField
import Amazonka.LookoutMetrics.Types.DetectedMetricSource
import qualified Amazonka.Prelude as Prelude

-- | An inferred dataset configuration.
--
-- /See:/ 'newDetectedMetricSetConfig' smart constructor.
data DetectedMetricSetConfig = DetectedMetricSetConfig'
  { -- | The dataset\'s interval.
    metricSetFrequency :: Prelude.Maybe DetectedField,
    -- | The dataset\'s data source.
    metricSource :: Prelude.Maybe DetectedMetricSource,
    -- | The dataset\'s offset.
    offset :: Prelude.Maybe DetectedField
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectedMetricSetConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricSetFrequency', 'detectedMetricSetConfig_metricSetFrequency' - The dataset\'s interval.
--
-- 'metricSource', 'detectedMetricSetConfig_metricSource' - The dataset\'s data source.
--
-- 'offset', 'detectedMetricSetConfig_offset' - The dataset\'s offset.
newDetectedMetricSetConfig ::
  DetectedMetricSetConfig
newDetectedMetricSetConfig =
  DetectedMetricSetConfig'
    { metricSetFrequency =
        Prelude.Nothing,
      metricSource = Prelude.Nothing,
      offset = Prelude.Nothing
    }

-- | The dataset\'s interval.
detectedMetricSetConfig_metricSetFrequency :: Lens.Lens' DetectedMetricSetConfig (Prelude.Maybe DetectedField)
detectedMetricSetConfig_metricSetFrequency = Lens.lens (\DetectedMetricSetConfig' {metricSetFrequency} -> metricSetFrequency) (\s@DetectedMetricSetConfig' {} a -> s {metricSetFrequency = a} :: DetectedMetricSetConfig)

-- | The dataset\'s data source.
detectedMetricSetConfig_metricSource :: Lens.Lens' DetectedMetricSetConfig (Prelude.Maybe DetectedMetricSource)
detectedMetricSetConfig_metricSource = Lens.lens (\DetectedMetricSetConfig' {metricSource} -> metricSource) (\s@DetectedMetricSetConfig' {} a -> s {metricSource = a} :: DetectedMetricSetConfig)

-- | The dataset\'s offset.
detectedMetricSetConfig_offset :: Lens.Lens' DetectedMetricSetConfig (Prelude.Maybe DetectedField)
detectedMetricSetConfig_offset = Lens.lens (\DetectedMetricSetConfig' {offset} -> offset) (\s@DetectedMetricSetConfig' {} a -> s {offset = a} :: DetectedMetricSetConfig)

instance Data.FromJSON DetectedMetricSetConfig where
  parseJSON =
    Data.withObject
      "DetectedMetricSetConfig"
      ( \x ->
          DetectedMetricSetConfig'
            Prelude.<$> (x Data..:? "MetricSetFrequency")
            Prelude.<*> (x Data..:? "MetricSource")
            Prelude.<*> (x Data..:? "Offset")
      )

instance Prelude.Hashable DetectedMetricSetConfig where
  hashWithSalt _salt DetectedMetricSetConfig' {..} =
    _salt
      `Prelude.hashWithSalt` metricSetFrequency
      `Prelude.hashWithSalt` metricSource
      `Prelude.hashWithSalt` offset

instance Prelude.NFData DetectedMetricSetConfig where
  rnf DetectedMetricSetConfig' {..} =
    Prelude.rnf metricSetFrequency
      `Prelude.seq` Prelude.rnf metricSource
      `Prelude.seq` Prelude.rnf offset
