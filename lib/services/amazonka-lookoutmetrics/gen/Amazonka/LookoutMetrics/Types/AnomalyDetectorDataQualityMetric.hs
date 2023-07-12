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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.AnomalyDetectorDataQualityMetric where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.MetricSetDataQualityMetric
import qualified Amazonka.Prelude as Prelude

-- | Aggregated details about the data quality metrics collected for the
-- @AnomalyDetectorArn@ provided in the GetDataQualityMetrics object.
--
-- /See:/ 'newAnomalyDetectorDataQualityMetric' smart constructor.
data AnomalyDetectorDataQualityMetric = AnomalyDetectorDataQualityMetric'
  { -- | An array of @DataQualityMetricList@ objects. Each object in the array
    -- contains information about a data quality metric.
    metricSetDataQualityMetricList :: Prelude.Maybe [MetricSetDataQualityMetric],
    -- | The start time for the data quality metrics collection.
    startTimestamp :: Prelude.Maybe Data.POSIX
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
-- 'metricSetDataQualityMetricList', 'anomalyDetectorDataQualityMetric_metricSetDataQualityMetricList' - An array of @DataQualityMetricList@ objects. Each object in the array
-- contains information about a data quality metric.
--
-- 'startTimestamp', 'anomalyDetectorDataQualityMetric_startTimestamp' - The start time for the data quality metrics collection.
newAnomalyDetectorDataQualityMetric ::
  AnomalyDetectorDataQualityMetric
newAnomalyDetectorDataQualityMetric =
  AnomalyDetectorDataQualityMetric'
    { metricSetDataQualityMetricList =
        Prelude.Nothing,
      startTimestamp = Prelude.Nothing
    }

-- | An array of @DataQualityMetricList@ objects. Each object in the array
-- contains information about a data quality metric.
anomalyDetectorDataQualityMetric_metricSetDataQualityMetricList :: Lens.Lens' AnomalyDetectorDataQualityMetric (Prelude.Maybe [MetricSetDataQualityMetric])
anomalyDetectorDataQualityMetric_metricSetDataQualityMetricList = Lens.lens (\AnomalyDetectorDataQualityMetric' {metricSetDataQualityMetricList} -> metricSetDataQualityMetricList) (\s@AnomalyDetectorDataQualityMetric' {} a -> s {metricSetDataQualityMetricList = a} :: AnomalyDetectorDataQualityMetric) Prelude.. Lens.mapping Lens.coerced

-- | The start time for the data quality metrics collection.
anomalyDetectorDataQualityMetric_startTimestamp :: Lens.Lens' AnomalyDetectorDataQualityMetric (Prelude.Maybe Prelude.UTCTime)
anomalyDetectorDataQualityMetric_startTimestamp = Lens.lens (\AnomalyDetectorDataQualityMetric' {startTimestamp} -> startTimestamp) (\s@AnomalyDetectorDataQualityMetric' {} a -> s {startTimestamp = a} :: AnomalyDetectorDataQualityMetric) Prelude.. Lens.mapping Data._Time

instance
  Data.FromJSON
    AnomalyDetectorDataQualityMetric
  where
  parseJSON =
    Data.withObject
      "AnomalyDetectorDataQualityMetric"
      ( \x ->
          AnomalyDetectorDataQualityMetric'
            Prelude.<$> ( x
                            Data..:? "MetricSetDataQualityMetricList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "StartTimestamp")
      )

instance
  Prelude.Hashable
    AnomalyDetectorDataQualityMetric
  where
  hashWithSalt
    _salt
    AnomalyDetectorDataQualityMetric' {..} =
      _salt
        `Prelude.hashWithSalt` metricSetDataQualityMetricList
        `Prelude.hashWithSalt` startTimestamp

instance
  Prelude.NFData
    AnomalyDetectorDataQualityMetric
  where
  rnf AnomalyDetectorDataQualityMetric' {..} =
    Prelude.rnf metricSetDataQualityMetricList
      `Prelude.seq` Prelude.rnf startTimestamp
