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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RecommendationJobInferenceBenchmark where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.EndpointOutputConfiguration
import Amazonka.SageMaker.Types.InferenceMetrics
import Amazonka.SageMaker.Types.ModelConfiguration
import Amazonka.SageMaker.Types.RecommendationMetrics

-- | The details for a specific benchmark from an Inference Recommender job.
--
-- /See:/ 'newRecommendationJobInferenceBenchmark' smart constructor.
data RecommendationJobInferenceBenchmark = RecommendationJobInferenceBenchmark'
  { endpointConfiguration :: Prelude.Maybe EndpointOutputConfiguration,
    endpointMetrics :: Prelude.Maybe InferenceMetrics,
    -- | The reason why a benchmark failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that shows when the benchmark completed.
    invocationEndTime :: Prelude.Maybe Data.POSIX,
    -- | A timestamp that shows when the benchmark started.
    invocationStartTime :: Prelude.Maybe Data.POSIX,
    metrics :: Prelude.Maybe RecommendationMetrics,
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
-- 'endpointConfiguration', 'recommendationJobInferenceBenchmark_endpointConfiguration' - Undocumented member.
--
-- 'endpointMetrics', 'recommendationJobInferenceBenchmark_endpointMetrics' - Undocumented member.
--
-- 'failureReason', 'recommendationJobInferenceBenchmark_failureReason' - The reason why a benchmark failed.
--
-- 'invocationEndTime', 'recommendationJobInferenceBenchmark_invocationEndTime' - A timestamp that shows when the benchmark completed.
--
-- 'invocationStartTime', 'recommendationJobInferenceBenchmark_invocationStartTime' - A timestamp that shows when the benchmark started.
--
-- 'metrics', 'recommendationJobInferenceBenchmark_metrics' - Undocumented member.
--
-- 'modelConfiguration', 'recommendationJobInferenceBenchmark_modelConfiguration' - Undocumented member.
newRecommendationJobInferenceBenchmark ::
  -- | 'modelConfiguration'
  ModelConfiguration ->
  RecommendationJobInferenceBenchmark
newRecommendationJobInferenceBenchmark
  pModelConfiguration_ =
    RecommendationJobInferenceBenchmark'
      { endpointConfiguration =
          Prelude.Nothing,
        endpointMetrics = Prelude.Nothing,
        failureReason = Prelude.Nothing,
        invocationEndTime = Prelude.Nothing,
        invocationStartTime = Prelude.Nothing,
        metrics = Prelude.Nothing,
        modelConfiguration =
          pModelConfiguration_
      }

-- | Undocumented member.
recommendationJobInferenceBenchmark_endpointConfiguration :: Lens.Lens' RecommendationJobInferenceBenchmark (Prelude.Maybe EndpointOutputConfiguration)
recommendationJobInferenceBenchmark_endpointConfiguration = Lens.lens (\RecommendationJobInferenceBenchmark' {endpointConfiguration} -> endpointConfiguration) (\s@RecommendationJobInferenceBenchmark' {} a -> s {endpointConfiguration = a} :: RecommendationJobInferenceBenchmark)

-- | Undocumented member.
recommendationJobInferenceBenchmark_endpointMetrics :: Lens.Lens' RecommendationJobInferenceBenchmark (Prelude.Maybe InferenceMetrics)
recommendationJobInferenceBenchmark_endpointMetrics = Lens.lens (\RecommendationJobInferenceBenchmark' {endpointMetrics} -> endpointMetrics) (\s@RecommendationJobInferenceBenchmark' {} a -> s {endpointMetrics = a} :: RecommendationJobInferenceBenchmark)

-- | The reason why a benchmark failed.
recommendationJobInferenceBenchmark_failureReason :: Lens.Lens' RecommendationJobInferenceBenchmark (Prelude.Maybe Prelude.Text)
recommendationJobInferenceBenchmark_failureReason = Lens.lens (\RecommendationJobInferenceBenchmark' {failureReason} -> failureReason) (\s@RecommendationJobInferenceBenchmark' {} a -> s {failureReason = a} :: RecommendationJobInferenceBenchmark)

-- | A timestamp that shows when the benchmark completed.
recommendationJobInferenceBenchmark_invocationEndTime :: Lens.Lens' RecommendationJobInferenceBenchmark (Prelude.Maybe Prelude.UTCTime)
recommendationJobInferenceBenchmark_invocationEndTime = Lens.lens (\RecommendationJobInferenceBenchmark' {invocationEndTime} -> invocationEndTime) (\s@RecommendationJobInferenceBenchmark' {} a -> s {invocationEndTime = a} :: RecommendationJobInferenceBenchmark) Prelude.. Lens.mapping Data._Time

-- | A timestamp that shows when the benchmark started.
recommendationJobInferenceBenchmark_invocationStartTime :: Lens.Lens' RecommendationJobInferenceBenchmark (Prelude.Maybe Prelude.UTCTime)
recommendationJobInferenceBenchmark_invocationStartTime = Lens.lens (\RecommendationJobInferenceBenchmark' {invocationStartTime} -> invocationStartTime) (\s@RecommendationJobInferenceBenchmark' {} a -> s {invocationStartTime = a} :: RecommendationJobInferenceBenchmark) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
recommendationJobInferenceBenchmark_metrics :: Lens.Lens' RecommendationJobInferenceBenchmark (Prelude.Maybe RecommendationMetrics)
recommendationJobInferenceBenchmark_metrics = Lens.lens (\RecommendationJobInferenceBenchmark' {metrics} -> metrics) (\s@RecommendationJobInferenceBenchmark' {} a -> s {metrics = a} :: RecommendationJobInferenceBenchmark)

-- | Undocumented member.
recommendationJobInferenceBenchmark_modelConfiguration :: Lens.Lens' RecommendationJobInferenceBenchmark ModelConfiguration
recommendationJobInferenceBenchmark_modelConfiguration = Lens.lens (\RecommendationJobInferenceBenchmark' {modelConfiguration} -> modelConfiguration) (\s@RecommendationJobInferenceBenchmark' {} a -> s {modelConfiguration = a} :: RecommendationJobInferenceBenchmark)

instance
  Data.FromJSON
    RecommendationJobInferenceBenchmark
  where
  parseJSON =
    Data.withObject
      "RecommendationJobInferenceBenchmark"
      ( \x ->
          RecommendationJobInferenceBenchmark'
            Prelude.<$> (x Data..:? "EndpointConfiguration")
            Prelude.<*> (x Data..:? "EndpointMetrics")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "InvocationEndTime")
            Prelude.<*> (x Data..:? "InvocationStartTime")
            Prelude.<*> (x Data..:? "Metrics")
            Prelude.<*> (x Data..: "ModelConfiguration")
      )

instance
  Prelude.Hashable
    RecommendationJobInferenceBenchmark
  where
  hashWithSalt
    _salt
    RecommendationJobInferenceBenchmark' {..} =
      _salt
        `Prelude.hashWithSalt` endpointConfiguration
        `Prelude.hashWithSalt` endpointMetrics
        `Prelude.hashWithSalt` failureReason
        `Prelude.hashWithSalt` invocationEndTime
        `Prelude.hashWithSalt` invocationStartTime
        `Prelude.hashWithSalt` metrics
        `Prelude.hashWithSalt` modelConfiguration

instance
  Prelude.NFData
    RecommendationJobInferenceBenchmark
  where
  rnf RecommendationJobInferenceBenchmark' {..} =
    Prelude.rnf endpointConfiguration
      `Prelude.seq` Prelude.rnf endpointMetrics
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf invocationEndTime
      `Prelude.seq` Prelude.rnf invocationStartTime
      `Prelude.seq` Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf modelConfiguration
