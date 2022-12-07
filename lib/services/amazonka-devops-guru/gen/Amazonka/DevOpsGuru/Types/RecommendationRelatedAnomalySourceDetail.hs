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
-- Module      : Amazonka.DevOpsGuru.Types.RecommendationRelatedAnomalySourceDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.RecommendationRelatedAnomalySourceDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.RecommendationRelatedCloudWatchMetricsSourceDetail
import qualified Amazonka.Prelude as Prelude

-- | Contains an array of
-- @RecommendationRelatedCloudWatchMetricsSourceDetail@ objects that
-- contain the name and namespace of an Amazon CloudWatch metric.
--
-- /See:/ 'newRecommendationRelatedAnomalySourceDetail' smart constructor.
data RecommendationRelatedAnomalySourceDetail = RecommendationRelatedAnomalySourceDetail'
  { -- | An array of @CloudWatchMetricsDetail@ objects that contains information
    -- about the analyzed metrics that displayed anomalous behavior.
    cloudWatchMetrics :: Prelude.Maybe [RecommendationRelatedCloudWatchMetricsSourceDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationRelatedAnomalySourceDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchMetrics', 'recommendationRelatedAnomalySourceDetail_cloudWatchMetrics' - An array of @CloudWatchMetricsDetail@ objects that contains information
-- about the analyzed metrics that displayed anomalous behavior.
newRecommendationRelatedAnomalySourceDetail ::
  RecommendationRelatedAnomalySourceDetail
newRecommendationRelatedAnomalySourceDetail =
  RecommendationRelatedAnomalySourceDetail'
    { cloudWatchMetrics =
        Prelude.Nothing
    }

-- | An array of @CloudWatchMetricsDetail@ objects that contains information
-- about the analyzed metrics that displayed anomalous behavior.
recommendationRelatedAnomalySourceDetail_cloudWatchMetrics :: Lens.Lens' RecommendationRelatedAnomalySourceDetail (Prelude.Maybe [RecommendationRelatedCloudWatchMetricsSourceDetail])
recommendationRelatedAnomalySourceDetail_cloudWatchMetrics = Lens.lens (\RecommendationRelatedAnomalySourceDetail' {cloudWatchMetrics} -> cloudWatchMetrics) (\s@RecommendationRelatedAnomalySourceDetail' {} a -> s {cloudWatchMetrics = a} :: RecommendationRelatedAnomalySourceDetail) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    RecommendationRelatedAnomalySourceDetail
  where
  parseJSON =
    Data.withObject
      "RecommendationRelatedAnomalySourceDetail"
      ( \x ->
          RecommendationRelatedAnomalySourceDetail'
            Prelude.<$> ( x Data..:? "CloudWatchMetrics"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    RecommendationRelatedAnomalySourceDetail
  where
  hashWithSalt
    _salt
    RecommendationRelatedAnomalySourceDetail' {..} =
      _salt `Prelude.hashWithSalt` cloudWatchMetrics

instance
  Prelude.NFData
    RecommendationRelatedAnomalySourceDetail
  where
  rnf RecommendationRelatedAnomalySourceDetail' {..} =
    Prelude.rnf cloudWatchMetrics
