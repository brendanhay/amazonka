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
-- Module      : Amazonka.LookoutMetrics.Types.AnomalyDetectorDataQualityMetric
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.AnomalyDetectorDataQualityMetric where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutMetrics.Types.MetricSetDataQualityMetric
import qualified Amazonka.Prelude as Prelude

-- | Aggregated details about the data quality metrics collected for the
-- @AnomalyDetectorArn@ provided in the GetDataQualityMetrics object.
--
-- /See:/ 'newAnomalyDetectorDataQualityMetric' smart constructor.
data AnomalyDetectorDataQualityMetric = AnomalyDetectorDataQualityMetric'
  { -- | The start time for the data quality metrics collection.
    startTimestamp :: Prelude.Maybe Core.POSIX,
    -- | An array of @DataQualityMetricList@ objects. Each object in the array
    -- contains information about a data quality metric.
    metricSetDataQualityMetricList :: Prelude.Maybe [MetricSetDataQualityMetric]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnomalyDetectorDataQualityMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTimestamp', 'anomalyDetectorDataQualityMetric_startTimestamp' - The start time for the data quality metrics collection.
--
-- 'metricSetDataQualityMetricList', 'anomalyDetectorDataQualityMetric_metricSetDataQualityMetricList' - An array of @DataQualityMetricList@ objects. Each object in the array
-- contains information about a data quality metric.
newAnomalyDetectorDataQualityMetric ::
  AnomalyDetectorDataQualityMetric
newAnomalyDetectorDataQualityMetric =
  AnomalyDetectorDataQualityMetric'
    { startTimestamp =
        Prelude.Nothing,
      metricSetDataQualityMetricList =
        Prelude.Nothing
    }

-- | The start time for the data quality metrics collection.
anomalyDetectorDataQualityMetric_startTimestamp :: Lens.Lens' AnomalyDetectorDataQualityMetric (Prelude.Maybe Prelude.UTCTime)
anomalyDetectorDataQualityMetric_startTimestamp = Lens.lens (\AnomalyDetectorDataQualityMetric' {startTimestamp} -> startTimestamp) (\s@AnomalyDetectorDataQualityMetric' {} a -> s {startTimestamp = a} :: AnomalyDetectorDataQualityMetric) Prelude.. Lens.mapping Core._Time

-- | An array of @DataQualityMetricList@ objects. Each object in the array
-- contains information about a data quality metric.
anomalyDetectorDataQualityMetric_metricSetDataQualityMetricList :: Lens.Lens' AnomalyDetectorDataQualityMetric (Prelude.Maybe [MetricSetDataQualityMetric])
anomalyDetectorDataQualityMetric_metricSetDataQualityMetricList = Lens.lens (\AnomalyDetectorDataQualityMetric' {metricSetDataQualityMetricList} -> metricSetDataQualityMetricList) (\s@AnomalyDetectorDataQualityMetric' {} a -> s {metricSetDataQualityMetricList = a} :: AnomalyDetectorDataQualityMetric) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    AnomalyDetectorDataQualityMetric
  where
  parseJSON =
    Core.withObject
      "AnomalyDetectorDataQualityMetric"
      ( \x ->
          AnomalyDetectorDataQualityMetric'
            Prelude.<$> (x Core..:? "StartTimestamp")
            Prelude.<*> ( x Core..:? "MetricSetDataQualityMetricList"
                            Core..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    AnomalyDetectorDataQualityMetric
  where
  hashWithSalt
    _salt
    AnomalyDetectorDataQualityMetric' {..} =
      _salt `Prelude.hashWithSalt` startTimestamp
        `Prelude.hashWithSalt` metricSetDataQualityMetricList

instance
  Prelude.NFData
    AnomalyDetectorDataQualityMetric
  where
  rnf AnomalyDetectorDataQualityMetric' {..} =
    Prelude.rnf startTimestamp
      `Prelude.seq` Prelude.rnf metricSetDataQualityMetricList
