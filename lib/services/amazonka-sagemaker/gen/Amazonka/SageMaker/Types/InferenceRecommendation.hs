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
  { -- | The metrics used to decide what recommendation to make.
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
      { metrics = pMetrics_,
        endpointConfiguration = pEndpointConfiguration_,
        modelConfiguration = pModelConfiguration_
      }

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
            Prelude.<$> (x Data..: "Metrics")
            Prelude.<*> (x Data..: "EndpointConfiguration")
            Prelude.<*> (x Data..: "ModelConfiguration")
      )

instance Prelude.Hashable InferenceRecommendation where
  hashWithSalt _salt InferenceRecommendation' {..} =
    _salt `Prelude.hashWithSalt` metrics
      `Prelude.hashWithSalt` endpointConfiguration
      `Prelude.hashWithSalt` modelConfiguration

instance Prelude.NFData InferenceRecommendation where
  rnf InferenceRecommendation' {..} =
    Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf endpointConfiguration
      `Prelude.seq` Prelude.rnf modelConfiguration
