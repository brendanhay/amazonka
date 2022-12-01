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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.DetectedMetricSetConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutMetrics.Types.DetectedField
import Amazonka.LookoutMetrics.Types.DetectedMetricSource
import qualified Amazonka.Prelude as Prelude

-- | An inferred dataset configuration.
--
-- /See:/ 'newDetectedMetricSetConfig' smart constructor.
data DetectedMetricSetConfig = DetectedMetricSetConfig'
  { -- | The dataset\'s offset.
    offset :: Prelude.Maybe DetectedField,
    -- | The dataset\'s data source.
    metricSource :: Prelude.Maybe DetectedMetricSource,
    -- | The dataset\'s interval.
    metricSetFrequency :: Prelude.Maybe DetectedField
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
-- 'offset', 'detectedMetricSetConfig_offset' - The dataset\'s offset.
--
-- 'metricSource', 'detectedMetricSetConfig_metricSource' - The dataset\'s data source.
--
-- 'metricSetFrequency', 'detectedMetricSetConfig_metricSetFrequency' - The dataset\'s interval.
newDetectedMetricSetConfig ::
  DetectedMetricSetConfig
newDetectedMetricSetConfig =
  DetectedMetricSetConfig'
    { offset = Prelude.Nothing,
      metricSource = Prelude.Nothing,
      metricSetFrequency = Prelude.Nothing
    }

-- | The dataset\'s offset.
detectedMetricSetConfig_offset :: Lens.Lens' DetectedMetricSetConfig (Prelude.Maybe DetectedField)
detectedMetricSetConfig_offset = Lens.lens (\DetectedMetricSetConfig' {offset} -> offset) (\s@DetectedMetricSetConfig' {} a -> s {offset = a} :: DetectedMetricSetConfig)

-- | The dataset\'s data source.
detectedMetricSetConfig_metricSource :: Lens.Lens' DetectedMetricSetConfig (Prelude.Maybe DetectedMetricSource)
detectedMetricSetConfig_metricSource = Lens.lens (\DetectedMetricSetConfig' {metricSource} -> metricSource) (\s@DetectedMetricSetConfig' {} a -> s {metricSource = a} :: DetectedMetricSetConfig)

-- | The dataset\'s interval.
detectedMetricSetConfig_metricSetFrequency :: Lens.Lens' DetectedMetricSetConfig (Prelude.Maybe DetectedField)
detectedMetricSetConfig_metricSetFrequency = Lens.lens (\DetectedMetricSetConfig' {metricSetFrequency} -> metricSetFrequency) (\s@DetectedMetricSetConfig' {} a -> s {metricSetFrequency = a} :: DetectedMetricSetConfig)

instance Core.FromJSON DetectedMetricSetConfig where
  parseJSON =
    Core.withObject
      "DetectedMetricSetConfig"
      ( \x ->
          DetectedMetricSetConfig'
            Prelude.<$> (x Core..:? "Offset")
            Prelude.<*> (x Core..:? "MetricSource")
            Prelude.<*> (x Core..:? "MetricSetFrequency")
      )

instance Prelude.Hashable DetectedMetricSetConfig where
  hashWithSalt _salt DetectedMetricSetConfig' {..} =
    _salt `Prelude.hashWithSalt` offset
      `Prelude.hashWithSalt` metricSource
      `Prelude.hashWithSalt` metricSetFrequency

instance Prelude.NFData DetectedMetricSetConfig where
  rnf DetectedMetricSetConfig' {..} =
    Prelude.rnf offset
      `Prelude.seq` Prelude.rnf metricSource
      `Prelude.seq` Prelude.rnf metricSetFrequency
