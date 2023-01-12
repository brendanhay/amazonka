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
-- Module      : Amazonka.SageMaker.Types.RecommendationMetrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RecommendationMetrics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The metrics of recommendations.
--
-- /See:/ 'newRecommendationMetrics' smart constructor.
data RecommendationMetrics = RecommendationMetrics'
  { -- | Defines the cost per hour for the instance.
    costPerHour :: Prelude.Double,
    -- | Defines the cost per inference for the instance .
    costPerInference :: Prelude.Double,
    -- | The expected maximum number of requests per minute for the instance.
    maxInvocations :: Prelude.Int,
    -- | The expected model latency at maximum invocation per minute for the
    -- instance.
    modelLatency :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'costPerHour', 'recommendationMetrics_costPerHour' - Defines the cost per hour for the instance.
--
-- 'costPerInference', 'recommendationMetrics_costPerInference' - Defines the cost per inference for the instance .
--
-- 'maxInvocations', 'recommendationMetrics_maxInvocations' - The expected maximum number of requests per minute for the instance.
--
-- 'modelLatency', 'recommendationMetrics_modelLatency' - The expected model latency at maximum invocation per minute for the
-- instance.
newRecommendationMetrics ::
  -- | 'costPerHour'
  Prelude.Double ->
  -- | 'costPerInference'
  Prelude.Double ->
  -- | 'maxInvocations'
  Prelude.Int ->
  -- | 'modelLatency'
  Prelude.Int ->
  RecommendationMetrics
newRecommendationMetrics
  pCostPerHour_
  pCostPerInference_
  pMaxInvocations_
  pModelLatency_ =
    RecommendationMetrics'
      { costPerHour = pCostPerHour_,
        costPerInference = pCostPerInference_,
        maxInvocations = pMaxInvocations_,
        modelLatency = pModelLatency_
      }

-- | Defines the cost per hour for the instance.
recommendationMetrics_costPerHour :: Lens.Lens' RecommendationMetrics Prelude.Double
recommendationMetrics_costPerHour = Lens.lens (\RecommendationMetrics' {costPerHour} -> costPerHour) (\s@RecommendationMetrics' {} a -> s {costPerHour = a} :: RecommendationMetrics)

-- | Defines the cost per inference for the instance .
recommendationMetrics_costPerInference :: Lens.Lens' RecommendationMetrics Prelude.Double
recommendationMetrics_costPerInference = Lens.lens (\RecommendationMetrics' {costPerInference} -> costPerInference) (\s@RecommendationMetrics' {} a -> s {costPerInference = a} :: RecommendationMetrics)

-- | The expected maximum number of requests per minute for the instance.
recommendationMetrics_maxInvocations :: Lens.Lens' RecommendationMetrics Prelude.Int
recommendationMetrics_maxInvocations = Lens.lens (\RecommendationMetrics' {maxInvocations} -> maxInvocations) (\s@RecommendationMetrics' {} a -> s {maxInvocations = a} :: RecommendationMetrics)

-- | The expected model latency at maximum invocation per minute for the
-- instance.
recommendationMetrics_modelLatency :: Lens.Lens' RecommendationMetrics Prelude.Int
recommendationMetrics_modelLatency = Lens.lens (\RecommendationMetrics' {modelLatency} -> modelLatency) (\s@RecommendationMetrics' {} a -> s {modelLatency = a} :: RecommendationMetrics)

instance Data.FromJSON RecommendationMetrics where
  parseJSON =
    Data.withObject
      "RecommendationMetrics"
      ( \x ->
          RecommendationMetrics'
            Prelude.<$> (x Data..: "CostPerHour")
            Prelude.<*> (x Data..: "CostPerInference")
            Prelude.<*> (x Data..: "MaxInvocations")
            Prelude.<*> (x Data..: "ModelLatency")
      )

instance Prelude.Hashable RecommendationMetrics where
  hashWithSalt _salt RecommendationMetrics' {..} =
    _salt `Prelude.hashWithSalt` costPerHour
      `Prelude.hashWithSalt` costPerInference
      `Prelude.hashWithSalt` maxInvocations
      `Prelude.hashWithSalt` modelLatency

instance Prelude.NFData RecommendationMetrics where
  rnf RecommendationMetrics' {..} =
    Prelude.rnf costPerHour
      `Prelude.seq` Prelude.rnf costPerInference
      `Prelude.seq` Prelude.rnf maxInvocations
      `Prelude.seq` Prelude.rnf modelLatency
