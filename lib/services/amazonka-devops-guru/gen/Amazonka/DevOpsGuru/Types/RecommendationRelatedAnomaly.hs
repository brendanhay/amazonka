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
-- Module      : Amazonka.DevOpsGuru.Types.RecommendationRelatedAnomaly
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.RecommendationRelatedAnomaly where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.RecommendationRelatedAnomalyResource
import Amazonka.DevOpsGuru.Types.RecommendationRelatedAnomalySourceDetail
import qualified Amazonka.Prelude as Prelude

-- | Information about an anomaly that is related to a recommendation.
--
-- /See:/ 'newRecommendationRelatedAnomaly' smart constructor.
data RecommendationRelatedAnomaly = RecommendationRelatedAnomaly'
  { -- | The ID of an anomaly that generated the insight with this
    -- recommendation.
    anomalyId :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that represent resources in which DevOps Guru
    -- detected anomalous behavior. Each object contains the name and type of
    -- the resource.
    resources :: Prelude.Maybe [RecommendationRelatedAnomalyResource],
    -- | Information about where the anomalous behavior related the
    -- recommendation was found. For example, details in Amazon CloudWatch
    -- metrics.
    sourceDetails :: Prelude.Maybe [RecommendationRelatedAnomalySourceDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationRelatedAnomaly' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyId', 'recommendationRelatedAnomaly_anomalyId' - The ID of an anomaly that generated the insight with this
-- recommendation.
--
-- 'resources', 'recommendationRelatedAnomaly_resources' - An array of objects that represent resources in which DevOps Guru
-- detected anomalous behavior. Each object contains the name and type of
-- the resource.
--
-- 'sourceDetails', 'recommendationRelatedAnomaly_sourceDetails' - Information about where the anomalous behavior related the
-- recommendation was found. For example, details in Amazon CloudWatch
-- metrics.
newRecommendationRelatedAnomaly ::
  RecommendationRelatedAnomaly
newRecommendationRelatedAnomaly =
  RecommendationRelatedAnomaly'
    { anomalyId =
        Prelude.Nothing,
      resources = Prelude.Nothing,
      sourceDetails = Prelude.Nothing
    }

-- | The ID of an anomaly that generated the insight with this
-- recommendation.
recommendationRelatedAnomaly_anomalyId :: Lens.Lens' RecommendationRelatedAnomaly (Prelude.Maybe Prelude.Text)
recommendationRelatedAnomaly_anomalyId = Lens.lens (\RecommendationRelatedAnomaly' {anomalyId} -> anomalyId) (\s@RecommendationRelatedAnomaly' {} a -> s {anomalyId = a} :: RecommendationRelatedAnomaly)

-- | An array of objects that represent resources in which DevOps Guru
-- detected anomalous behavior. Each object contains the name and type of
-- the resource.
recommendationRelatedAnomaly_resources :: Lens.Lens' RecommendationRelatedAnomaly (Prelude.Maybe [RecommendationRelatedAnomalyResource])
recommendationRelatedAnomaly_resources = Lens.lens (\RecommendationRelatedAnomaly' {resources} -> resources) (\s@RecommendationRelatedAnomaly' {} a -> s {resources = a} :: RecommendationRelatedAnomaly) Prelude.. Lens.mapping Lens.coerced

-- | Information about where the anomalous behavior related the
-- recommendation was found. For example, details in Amazon CloudWatch
-- metrics.
recommendationRelatedAnomaly_sourceDetails :: Lens.Lens' RecommendationRelatedAnomaly (Prelude.Maybe [RecommendationRelatedAnomalySourceDetail])
recommendationRelatedAnomaly_sourceDetails = Lens.lens (\RecommendationRelatedAnomaly' {sourceDetails} -> sourceDetails) (\s@RecommendationRelatedAnomaly' {} a -> s {sourceDetails = a} :: RecommendationRelatedAnomaly) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RecommendationRelatedAnomaly where
  parseJSON =
    Data.withObject
      "RecommendationRelatedAnomaly"
      ( \x ->
          RecommendationRelatedAnomaly'
            Prelude.<$> (x Data..:? "AnomalyId")
            Prelude.<*> (x Data..:? "Resources" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SourceDetails" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    RecommendationRelatedAnomaly
  where
  hashWithSalt _salt RecommendationRelatedAnomaly' {..} =
    _salt `Prelude.hashWithSalt` anomalyId
      `Prelude.hashWithSalt` resources
      `Prelude.hashWithSalt` sourceDetails

instance Prelude.NFData RecommendationRelatedAnomaly where
  rnf RecommendationRelatedAnomaly' {..} =
    Prelude.rnf anomalyId
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf sourceDetails
