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
-- Module      : Amazonka.SageMaker.Types.RecommendationJobStoppingConditions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RecommendationJobStoppingConditions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ModelLatencyThreshold

-- | Specifies conditions for stopping a job. When a job reaches a stopping
-- condition limit, SageMaker ends the job.
--
-- /See:/ 'newRecommendationJobStoppingConditions' smart constructor.
data RecommendationJobStoppingConditions = RecommendationJobStoppingConditions'
  { -- | The maximum number of requests per minute expected for the endpoint.
    maxInvocations :: Prelude.Maybe Prelude.Int,
    -- | The interval of time taken by a model to respond as viewed from
    -- SageMaker. The interval includes the local communication time taken to
    -- send the request and to fetch the response from the container of a model
    -- and the time taken to complete the inference in the container.
    modelLatencyThresholds :: Prelude.Maybe (Prelude.NonEmpty ModelLatencyThreshold)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationJobStoppingConditions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxInvocations', 'recommendationJobStoppingConditions_maxInvocations' - The maximum number of requests per minute expected for the endpoint.
--
-- 'modelLatencyThresholds', 'recommendationJobStoppingConditions_modelLatencyThresholds' - The interval of time taken by a model to respond as viewed from
-- SageMaker. The interval includes the local communication time taken to
-- send the request and to fetch the response from the container of a model
-- and the time taken to complete the inference in the container.
newRecommendationJobStoppingConditions ::
  RecommendationJobStoppingConditions
newRecommendationJobStoppingConditions =
  RecommendationJobStoppingConditions'
    { maxInvocations =
        Prelude.Nothing,
      modelLatencyThresholds =
        Prelude.Nothing
    }

-- | The maximum number of requests per minute expected for the endpoint.
recommendationJobStoppingConditions_maxInvocations :: Lens.Lens' RecommendationJobStoppingConditions (Prelude.Maybe Prelude.Int)
recommendationJobStoppingConditions_maxInvocations = Lens.lens (\RecommendationJobStoppingConditions' {maxInvocations} -> maxInvocations) (\s@RecommendationJobStoppingConditions' {} a -> s {maxInvocations = a} :: RecommendationJobStoppingConditions)

-- | The interval of time taken by a model to respond as viewed from
-- SageMaker. The interval includes the local communication time taken to
-- send the request and to fetch the response from the container of a model
-- and the time taken to complete the inference in the container.
recommendationJobStoppingConditions_modelLatencyThresholds :: Lens.Lens' RecommendationJobStoppingConditions (Prelude.Maybe (Prelude.NonEmpty ModelLatencyThreshold))
recommendationJobStoppingConditions_modelLatencyThresholds = Lens.lens (\RecommendationJobStoppingConditions' {modelLatencyThresholds} -> modelLatencyThresholds) (\s@RecommendationJobStoppingConditions' {} a -> s {modelLatencyThresholds = a} :: RecommendationJobStoppingConditions) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    RecommendationJobStoppingConditions
  where
  parseJSON =
    Data.withObject
      "RecommendationJobStoppingConditions"
      ( \x ->
          RecommendationJobStoppingConditions'
            Prelude.<$> (x Data..:? "MaxInvocations")
            Prelude.<*> (x Data..:? "ModelLatencyThresholds")
      )

instance
  Prelude.Hashable
    RecommendationJobStoppingConditions
  where
  hashWithSalt
    _salt
    RecommendationJobStoppingConditions' {..} =
      _salt `Prelude.hashWithSalt` maxInvocations
        `Prelude.hashWithSalt` modelLatencyThresholds

instance
  Prelude.NFData
    RecommendationJobStoppingConditions
  where
  rnf RecommendationJobStoppingConditions' {..} =
    Prelude.rnf maxInvocations
      `Prelude.seq` Prelude.rnf modelLatencyThresholds

instance
  Data.ToJSON
    RecommendationJobStoppingConditions
  where
  toJSON RecommendationJobStoppingConditions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxInvocations" Data..=)
              Prelude.<$> maxInvocations,
            ("ModelLatencyThresholds" Data..=)
              Prelude.<$> modelLatencyThresholds
          ]
      )
