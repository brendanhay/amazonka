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
-- Module      : Amazonka.SageMaker.Types.InferenceRecommendation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.InferenceRecommendation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.EndpointOutputConfiguration
import Amazonka.SageMaker.Types.ModelConfiguration
import Amazonka.SageMaker.Types.RecommendationMetrics

-- | A list of recommendations made by Amazon SageMaker Inference
-- Recommender.
--
-- /See:/ 'newInferenceRecommendation' smart constructor.
data InferenceRecommendation = InferenceRecommendation'
  { -- | A timestamp that shows when the benchmark completed.
    invocationEndTime :: Prelude.Maybe Data.POSIX,
    -- | A timestamp that shows when the benchmark started.
    invocationStartTime :: Prelude.Maybe Data.POSIX,
    -- | The recommendation ID which uniquely identifies each recommendation.
    recommendationId :: Prelude.Maybe Prelude.Text,
    -- | The metrics used to decide what recommendation to make.
    metrics :: RecommendationMetrics,
    -- | Defines the endpoint configuration parameters.
    endpointConfiguration :: EndpointOutputConfiguration,
    -- | Defines the model configuration.
    modelConfiguration :: ModelConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferenceRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invocationEndTime', 'inferenceRecommendation_invocationEndTime' - A timestamp that shows when the benchmark completed.
--
-- 'invocationStartTime', 'inferenceRecommendation_invocationStartTime' - A timestamp that shows when the benchmark started.
--
-- 'recommendationId', 'inferenceRecommendation_recommendationId' - The recommendation ID which uniquely identifies each recommendation.
--
-- 'metrics', 'inferenceRecommendation_metrics' - The metrics used to decide what recommendation to make.
--
-- 'endpointConfiguration', 'inferenceRecommendation_endpointConfiguration' - Defines the endpoint configuration parameters.
--
-- 'modelConfiguration', 'inferenceRecommendation_modelConfiguration' - Defines the model configuration.
newInferenceRecommendation ::
  -- | 'metrics'
  RecommendationMetrics ->
  -- | 'endpointConfiguration'
  EndpointOutputConfiguration ->
  -- | 'modelConfiguration'
  ModelConfiguration ->
  InferenceRecommendation
newInferenceRecommendation
  pMetrics_
  pEndpointConfiguration_
  pModelConfiguration_ =
    InferenceRecommendation'
      { invocationEndTime =
          Prelude.Nothing,
        invocationStartTime = Prelude.Nothing,
        recommendationId = Prelude.Nothing,
        metrics = pMetrics_,
        endpointConfiguration = pEndpointConfiguration_,
        modelConfiguration = pModelConfiguration_
      }

-- | A timestamp that shows when the benchmark completed.
inferenceRecommendation_invocationEndTime :: Lens.Lens' InferenceRecommendation (Prelude.Maybe Prelude.UTCTime)
inferenceRecommendation_invocationEndTime = Lens.lens (\InferenceRecommendation' {invocationEndTime} -> invocationEndTime) (\s@InferenceRecommendation' {} a -> s {invocationEndTime = a} :: InferenceRecommendation) Prelude.. Lens.mapping Data._Time

-- | A timestamp that shows when the benchmark started.
inferenceRecommendation_invocationStartTime :: Lens.Lens' InferenceRecommendation (Prelude.Maybe Prelude.UTCTime)
inferenceRecommendation_invocationStartTime = Lens.lens (\InferenceRecommendation' {invocationStartTime} -> invocationStartTime) (\s@InferenceRecommendation' {} a -> s {invocationStartTime = a} :: InferenceRecommendation) Prelude.. Lens.mapping Data._Time

-- | The recommendation ID which uniquely identifies each recommendation.
inferenceRecommendation_recommendationId :: Lens.Lens' InferenceRecommendation (Prelude.Maybe Prelude.Text)
inferenceRecommendation_recommendationId = Lens.lens (\InferenceRecommendation' {recommendationId} -> recommendationId) (\s@InferenceRecommendation' {} a -> s {recommendationId = a} :: InferenceRecommendation)

-- | The metrics used to decide what recommendation to make.
inferenceRecommendation_metrics :: Lens.Lens' InferenceRecommendation RecommendationMetrics
inferenceRecommendation_metrics = Lens.lens (\InferenceRecommendation' {metrics} -> metrics) (\s@InferenceRecommendation' {} a -> s {metrics = a} :: InferenceRecommendation)

-- | Defines the endpoint configuration parameters.
inferenceRecommendation_endpointConfiguration :: Lens.Lens' InferenceRecommendation EndpointOutputConfiguration
inferenceRecommendation_endpointConfiguration = Lens.lens (\InferenceRecommendation' {endpointConfiguration} -> endpointConfiguration) (\s@InferenceRecommendation' {} a -> s {endpointConfiguration = a} :: InferenceRecommendation)

-- | Defines the model configuration.
inferenceRecommendation_modelConfiguration :: Lens.Lens' InferenceRecommendation ModelConfiguration
inferenceRecommendation_modelConfiguration = Lens.lens (\InferenceRecommendation' {modelConfiguration} -> modelConfiguration) (\s@InferenceRecommendation' {} a -> s {modelConfiguration = a} :: InferenceRecommendation)

instance Data.FromJSON InferenceRecommendation where
  parseJSON =
    Data.withObject
      "InferenceRecommendation"
      ( \x ->
          InferenceRecommendation'
            Prelude.<$> (x Data..:? "InvocationEndTime")
            Prelude.<*> (x Data..:? "InvocationStartTime")
            Prelude.<*> (x Data..:? "RecommendationId")
            Prelude.<*> (x Data..: "Metrics")
            Prelude.<*> (x Data..: "EndpointConfiguration")
            Prelude.<*> (x Data..: "ModelConfiguration")
      )

instance Prelude.Hashable InferenceRecommendation where
  hashWithSalt _salt InferenceRecommendation' {..} =
    _salt
      `Prelude.hashWithSalt` invocationEndTime
      `Prelude.hashWithSalt` invocationStartTime
      `Prelude.hashWithSalt` recommendationId
      `Prelude.hashWithSalt` metrics
      `Prelude.hashWithSalt` endpointConfiguration
      `Prelude.hashWithSalt` modelConfiguration

instance Prelude.NFData InferenceRecommendation where
  rnf InferenceRecommendation' {..} =
    Prelude.rnf invocationEndTime
      `Prelude.seq` Prelude.rnf invocationStartTime
      `Prelude.seq` Prelude.rnf recommendationId
      `Prelude.seq` Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf endpointConfiguration
      `Prelude.seq` Prelude.rnf modelConfiguration
