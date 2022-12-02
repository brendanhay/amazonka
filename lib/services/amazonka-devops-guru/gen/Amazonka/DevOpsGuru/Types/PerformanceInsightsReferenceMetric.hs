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
-- Module      : Amazonka.DevOpsGuru.Types.PerformanceInsightsReferenceMetric
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.PerformanceInsightsReferenceMetric where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.PerformanceInsightsMetricQuery
import qualified Amazonka.Prelude as Prelude

-- | Information about a reference metric used to evaluate Performance
-- Insights.
--
-- /See:/ 'newPerformanceInsightsReferenceMetric' smart constructor.
data PerformanceInsightsReferenceMetric = PerformanceInsightsReferenceMetric'
  { -- | A query to be processed on the metric.
    metricQuery :: Prelude.Maybe PerformanceInsightsMetricQuery
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PerformanceInsightsReferenceMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricQuery', 'performanceInsightsReferenceMetric_metricQuery' - A query to be processed on the metric.
newPerformanceInsightsReferenceMetric ::
  PerformanceInsightsReferenceMetric
newPerformanceInsightsReferenceMetric =
  PerformanceInsightsReferenceMetric'
    { metricQuery =
        Prelude.Nothing
    }

-- | A query to be processed on the metric.
performanceInsightsReferenceMetric_metricQuery :: Lens.Lens' PerformanceInsightsReferenceMetric (Prelude.Maybe PerformanceInsightsMetricQuery)
performanceInsightsReferenceMetric_metricQuery = Lens.lens (\PerformanceInsightsReferenceMetric' {metricQuery} -> metricQuery) (\s@PerformanceInsightsReferenceMetric' {} a -> s {metricQuery = a} :: PerformanceInsightsReferenceMetric)

instance
  Data.FromJSON
    PerformanceInsightsReferenceMetric
  where
  parseJSON =
    Data.withObject
      "PerformanceInsightsReferenceMetric"
      ( \x ->
          PerformanceInsightsReferenceMetric'
            Prelude.<$> (x Data..:? "MetricQuery")
      )

instance
  Prelude.Hashable
    PerformanceInsightsReferenceMetric
  where
  hashWithSalt
    _salt
    PerformanceInsightsReferenceMetric' {..} =
      _salt `Prelude.hashWithSalt` metricQuery

instance
  Prelude.NFData
    PerformanceInsightsReferenceMetric
  where
  rnf PerformanceInsightsReferenceMetric' {..} =
    Prelude.rnf metricQuery
