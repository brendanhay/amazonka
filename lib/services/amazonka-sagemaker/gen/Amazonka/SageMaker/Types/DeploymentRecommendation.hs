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
-- Module      : Amazonka.SageMaker.Types.DeploymentRecommendation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DeploymentRecommendation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.RealTimeInferenceRecommendation
import Amazonka.SageMaker.Types.RecommendationStatus

-- | A set of recommended deployment configurations for the model. To get
-- more advanced recommendations, see
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateInferenceRecommendationsJob.html CreateInferenceRecommendationsJob>
-- to create an inference recommendation job.
--
-- /See:/ 'newDeploymentRecommendation' smart constructor.
data DeploymentRecommendation = DeploymentRecommendation'
  { -- | A list of
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_RealTimeInferenceRecommendation.html RealTimeInferenceRecommendation>
    -- items.
    realTimeInferenceRecommendations :: Prelude.Maybe [RealTimeInferenceRecommendation],
    -- | Status of the deployment recommendation. The status @NOT_APPLICABLE@
    -- means that SageMaker is unable to provide a default recommendation for
    -- the model using the information provided. If the deployment status is
    -- @IN_PROGRESS@, retry your API call after a few seconds to get a
    -- @COMPLETED@ deployment recommendation.
    recommendationStatus :: RecommendationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'realTimeInferenceRecommendations', 'deploymentRecommendation_realTimeInferenceRecommendations' - A list of
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_RealTimeInferenceRecommendation.html RealTimeInferenceRecommendation>
-- items.
--
-- 'recommendationStatus', 'deploymentRecommendation_recommendationStatus' - Status of the deployment recommendation. The status @NOT_APPLICABLE@
-- means that SageMaker is unable to provide a default recommendation for
-- the model using the information provided. If the deployment status is
-- @IN_PROGRESS@, retry your API call after a few seconds to get a
-- @COMPLETED@ deployment recommendation.
newDeploymentRecommendation ::
  -- | 'recommendationStatus'
  RecommendationStatus ->
  DeploymentRecommendation
newDeploymentRecommendation pRecommendationStatus_ =
  DeploymentRecommendation'
    { realTimeInferenceRecommendations =
        Prelude.Nothing,
      recommendationStatus = pRecommendationStatus_
    }

-- | A list of
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_RealTimeInferenceRecommendation.html RealTimeInferenceRecommendation>
-- items.
deploymentRecommendation_realTimeInferenceRecommendations :: Lens.Lens' DeploymentRecommendation (Prelude.Maybe [RealTimeInferenceRecommendation])
deploymentRecommendation_realTimeInferenceRecommendations = Lens.lens (\DeploymentRecommendation' {realTimeInferenceRecommendations} -> realTimeInferenceRecommendations) (\s@DeploymentRecommendation' {} a -> s {realTimeInferenceRecommendations = a} :: DeploymentRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | Status of the deployment recommendation. The status @NOT_APPLICABLE@
-- means that SageMaker is unable to provide a default recommendation for
-- the model using the information provided. If the deployment status is
-- @IN_PROGRESS@, retry your API call after a few seconds to get a
-- @COMPLETED@ deployment recommendation.
deploymentRecommendation_recommendationStatus :: Lens.Lens' DeploymentRecommendation RecommendationStatus
deploymentRecommendation_recommendationStatus = Lens.lens (\DeploymentRecommendation' {recommendationStatus} -> recommendationStatus) (\s@DeploymentRecommendation' {} a -> s {recommendationStatus = a} :: DeploymentRecommendation)

instance Data.FromJSON DeploymentRecommendation where
  parseJSON =
    Data.withObject
      "DeploymentRecommendation"
      ( \x ->
          DeploymentRecommendation'
            Prelude.<$> ( x
                            Data..:? "RealTimeInferenceRecommendations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "RecommendationStatus")
      )

instance Prelude.Hashable DeploymentRecommendation where
  hashWithSalt _salt DeploymentRecommendation' {..} =
    _salt
      `Prelude.hashWithSalt` realTimeInferenceRecommendations
      `Prelude.hashWithSalt` recommendationStatus

instance Prelude.NFData DeploymentRecommendation where
  rnf DeploymentRecommendation' {..} =
    Prelude.rnf realTimeInferenceRecommendations
      `Prelude.seq` Prelude.rnf recommendationStatus
