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
-- Module      : Amazonka.DevOpsGuru.Types.AnomalySourceDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.AnomalySourceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DevOpsGuru.Types.CloudWatchMetricsDetail
import Amazonka.DevOpsGuru.Types.PerformanceInsightsMetricsDetail
import qualified Amazonka.Prelude as Prelude

-- | Details about the source of the anomalous operational data that
-- triggered the anomaly.
--
-- /See:/ 'newAnomalySourceDetails' smart constructor.
data AnomalySourceDetails = AnomalySourceDetails'
  { -- | An array of @CloudWatchMetricsDetail@ objects that contain information
    -- about analyzed CloudWatch metrics that show anomalous behavior.
    cloudWatchMetrics :: Prelude.Maybe [CloudWatchMetricsDetail],
    -- | An array of @PerformanceInsightsMetricsDetail@ objects that contain
    -- information about analyzed Performance Insights metrics that show
    -- anomalous behavior.
    performanceInsightsMetrics :: Prelude.Maybe [PerformanceInsightsMetricsDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnomalySourceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchMetrics', 'anomalySourceDetails_cloudWatchMetrics' - An array of @CloudWatchMetricsDetail@ objects that contain information
-- about analyzed CloudWatch metrics that show anomalous behavior.
--
-- 'performanceInsightsMetrics', 'anomalySourceDetails_performanceInsightsMetrics' - An array of @PerformanceInsightsMetricsDetail@ objects that contain
-- information about analyzed Performance Insights metrics that show
-- anomalous behavior.
newAnomalySourceDetails ::
  AnomalySourceDetails
newAnomalySourceDetails =
  AnomalySourceDetails'
    { cloudWatchMetrics =
        Prelude.Nothing,
      performanceInsightsMetrics = Prelude.Nothing
    }

-- | An array of @CloudWatchMetricsDetail@ objects that contain information
-- about analyzed CloudWatch metrics that show anomalous behavior.
anomalySourceDetails_cloudWatchMetrics :: Lens.Lens' AnomalySourceDetails (Prelude.Maybe [CloudWatchMetricsDetail])
anomalySourceDetails_cloudWatchMetrics = Lens.lens (\AnomalySourceDetails' {cloudWatchMetrics} -> cloudWatchMetrics) (\s@AnomalySourceDetails' {} a -> s {cloudWatchMetrics = a} :: AnomalySourceDetails) Prelude.. Lens.mapping Lens.coerced

-- | An array of @PerformanceInsightsMetricsDetail@ objects that contain
-- information about analyzed Performance Insights metrics that show
-- anomalous behavior.
anomalySourceDetails_performanceInsightsMetrics :: Lens.Lens' AnomalySourceDetails (Prelude.Maybe [PerformanceInsightsMetricsDetail])
anomalySourceDetails_performanceInsightsMetrics = Lens.lens (\AnomalySourceDetails' {performanceInsightsMetrics} -> performanceInsightsMetrics) (\s@AnomalySourceDetails' {} a -> s {performanceInsightsMetrics = a} :: AnomalySourceDetails) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AnomalySourceDetails where
  parseJSON =
    Core.withObject
      "AnomalySourceDetails"
      ( \x ->
          AnomalySourceDetails'
            Prelude.<$> ( x Core..:? "CloudWatchMetrics"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "PerformanceInsightsMetrics"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AnomalySourceDetails where
  hashWithSalt _salt AnomalySourceDetails' {..} =
    _salt `Prelude.hashWithSalt` cloudWatchMetrics
      `Prelude.hashWithSalt` performanceInsightsMetrics

instance Prelude.NFData AnomalySourceDetails where
  rnf AnomalySourceDetails' {..} =
    Prelude.rnf cloudWatchMetrics
      `Prelude.seq` Prelude.rnf performanceInsightsMetrics
