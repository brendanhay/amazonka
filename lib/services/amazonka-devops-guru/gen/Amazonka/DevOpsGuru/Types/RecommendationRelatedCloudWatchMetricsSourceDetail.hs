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
-- Module      : Amazonka.DevOpsGuru.Types.RecommendationRelatedCloudWatchMetricsSourceDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.RecommendationRelatedCloudWatchMetricsSourceDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an Amazon CloudWatch metric that is analyzed by DevOps
-- Guru. It is one of many analyzed metrics that are used to generate
-- insights.
--
-- /See:/ 'newRecommendationRelatedCloudWatchMetricsSourceDetail' smart constructor.
data RecommendationRelatedCloudWatchMetricsSourceDetail = RecommendationRelatedCloudWatchMetricsSourceDetail'
  { -- | The name of the CloudWatch metric.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the CloudWatch metric. A namespace is a container for
    -- CloudWatch metrics.
    namespace :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationRelatedCloudWatchMetricsSourceDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'recommendationRelatedCloudWatchMetricsSourceDetail_metricName' - The name of the CloudWatch metric.
--
-- 'namespace', 'recommendationRelatedCloudWatchMetricsSourceDetail_namespace' - The namespace of the CloudWatch metric. A namespace is a container for
-- CloudWatch metrics.
newRecommendationRelatedCloudWatchMetricsSourceDetail ::
  RecommendationRelatedCloudWatchMetricsSourceDetail
newRecommendationRelatedCloudWatchMetricsSourceDetail =
  RecommendationRelatedCloudWatchMetricsSourceDetail'
    { metricName =
        Prelude.Nothing,
      namespace =
        Prelude.Nothing
    }

-- | The name of the CloudWatch metric.
recommendationRelatedCloudWatchMetricsSourceDetail_metricName :: Lens.Lens' RecommendationRelatedCloudWatchMetricsSourceDetail (Prelude.Maybe Prelude.Text)
recommendationRelatedCloudWatchMetricsSourceDetail_metricName = Lens.lens (\RecommendationRelatedCloudWatchMetricsSourceDetail' {metricName} -> metricName) (\s@RecommendationRelatedCloudWatchMetricsSourceDetail' {} a -> s {metricName = a} :: RecommendationRelatedCloudWatchMetricsSourceDetail)

-- | The namespace of the CloudWatch metric. A namespace is a container for
-- CloudWatch metrics.
recommendationRelatedCloudWatchMetricsSourceDetail_namespace :: Lens.Lens' RecommendationRelatedCloudWatchMetricsSourceDetail (Prelude.Maybe Prelude.Text)
recommendationRelatedCloudWatchMetricsSourceDetail_namespace = Lens.lens (\RecommendationRelatedCloudWatchMetricsSourceDetail' {namespace} -> namespace) (\s@RecommendationRelatedCloudWatchMetricsSourceDetail' {} a -> s {namespace = a} :: RecommendationRelatedCloudWatchMetricsSourceDetail)

instance
  Core.FromJSON
    RecommendationRelatedCloudWatchMetricsSourceDetail
  where
  parseJSON =
    Core.withObject
      "RecommendationRelatedCloudWatchMetricsSourceDetail"
      ( \x ->
          RecommendationRelatedCloudWatchMetricsSourceDetail'
            Prelude.<$> (x Core..:? "MetricName")
              Prelude.<*> (x Core..:? "Namespace")
      )

instance
  Prelude.Hashable
    RecommendationRelatedCloudWatchMetricsSourceDetail
  where
  hashWithSalt
    _salt
    RecommendationRelatedCloudWatchMetricsSourceDetail' {..} =
      _salt `Prelude.hashWithSalt` metricName
        `Prelude.hashWithSalt` namespace

instance
  Prelude.NFData
    RecommendationRelatedCloudWatchMetricsSourceDetail
  where
  rnf
    RecommendationRelatedCloudWatchMetricsSourceDetail' {..} =
      Prelude.rnf metricName
        `Prelude.seq` Prelude.rnf namespace
