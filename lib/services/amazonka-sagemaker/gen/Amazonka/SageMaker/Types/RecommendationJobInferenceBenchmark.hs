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
-- Module      : Amazonka.SageMaker.Types.RecommendationJobInferenceBenchmark
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RecommendationJobInferenceBenchmark where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.EndpointOutputConfiguration
import Amazonka.SageMaker.Types.ModelConfiguration
import Amazonka.SageMaker.Types.RecommendationMetrics

-- | The details for a specific benchmark from an Inference Recommender job.
--
-- /See:/ 'newRecommendationJobInferenceBenchmark' smart constructor.
data RecommendationJobInferenceBenchmark = RecommendationJobInferenceBenchmark'
  { metrics :: Prelude.Maybe RecommendationMetrics,
    endpointConfiguration :: Prelude.Maybe EndpointOutputConfiguration,
    -- | The reason why a benchmark failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    modelConfiguration :: ModelConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationJobInferenceBenchmark' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metrics', 'recommendationJobInferenceBenchmark_metrics' - Undocumented member.
--
-- 'endpointConfiguration', 'recommendationJobInferenceBenchmark_endpointConfiguration' - Undocumented member.
--
-- 'failureReason', 'recommendationJobInferenceBenchmark_failureReason' - The reason why a benchmark failed.
--
-- 'modelConfiguration', 'recommendationJobInferenceBenchmark_modelConfiguration' - Undocumented member.
newRecommendationJobInferenceBenchmark ::
  -- | 'modelConfiguration'
  ModelConfiguration ->
  RecommendationJobInferenceBenchmark
newRecommendationJobInferenceBenchmark
  pModelConfiguration_ =
    RecommendationJobInferenceBenchmark'
      { metrics =
          Prelude.Nothing,
        endpointConfiguration =
          Prelude.Nothing,
        failureReason = Prelude.Nothing,
        modelConfiguration =
          pModelConfiguration_
      }

-- | Undocumented member.
recommendationJobInferenceBenchmark_metrics :: Lens.Lens' RecommendationJobInferenceBenchmark (Prelude.Maybe RecommendationMetrics)
recommendationJobInferenceBenchmark_metrics = Lens.lens (\RecommendationJobInferenceBenchmark' {metrics} -> metrics) (\s@RecommendationJobInferenceBenchmark' {} a -> s {metrics = a} :: RecommendationJobInferenceBenchmark)

-- | Undocumented member.
recommendationJobInferenceBenchmark_endpointConfiguration :: Lens.Lens' RecommendationJobInferenceBenchmark (Prelude.Maybe EndpointOutputConfiguration)
recommendationJobInferenceBenchmark_endpointConfiguration = Lens.lens (\RecommendationJobInferenceBenchmark' {endpointConfiguration} -> endpointConfiguration) (\s@RecommendationJobInferenceBenchmark' {} a -> s {endpointConfiguration = a} :: RecommendationJobInferenceBenchmark)

-- | The reason why a benchmark failed.
recommendationJobInferenceBenchmark_failureReason :: Lens.Lens' RecommendationJobInferenceBenchmark (Prelude.Maybe Prelude.Text)
recommendationJobInferenceBenchmark_failureReason = Lens.lens (\RecommendationJobInferenceBenchmark' {failureReason} -> failureReason) (\s@RecommendationJobInferenceBenchmark' {} a -> s {failureReason = a} :: RecommendationJobInferenceBenchmark)

-- | Undocumented member.
recommendationJobInferenceBenchmark_modelConfiguration :: Lens.Lens' RecommendationJobInferenceBenchmark ModelConfiguration
recommendationJobInferenceBenchmark_modelConfiguration = Lens.lens (\RecommendationJobInferenceBenchmark' {modelConfiguration} -> modelConfiguration) (\s@RecommendationJobInferenceBenchmark' {} a -> s {modelConfiguration = a} :: RecommendationJobInferenceBenchmark)

instance
  Core.FromJSON
    RecommendationJobInferenceBenchmark
  where
  parseJSON =
    Core.withObject
      "RecommendationJobInferenceBenchmark"
      ( \x ->
          RecommendationJobInferenceBenchmark'
            Prelude.<$> (x Core..:? "Metrics")
            Prelude.<*> (x Core..:? "EndpointConfiguration")
            Prelude.<*> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..: "ModelConfiguration")
      )

instance
  Prelude.Hashable
    RecommendationJobInferenceBenchmark
  where
  hashWithSalt
    _salt
    RecommendationJobInferenceBenchmark' {..} =
      _salt `Prelude.hashWithSalt` metrics
        `Prelude.hashWithSalt` endpointConfiguration
        `Prelude.hashWithSalt` failureReason
        `Prelude.hashWithSalt` modelConfiguration

instance
  Prelude.NFData
    RecommendationJobInferenceBenchmark
  where
  rnf RecommendationJobInferenceBenchmark' {..} =
    Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf endpointConfiguration
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf modelConfiguration
