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
-- Module      : Amazonka.SageMaker.Types.RealTimeInferenceRecommendation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RealTimeInferenceRecommendation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ProductionVariantInstanceType

-- | The recommended configuration to use for Real-Time Inference.
--
-- /See:/ 'newRealTimeInferenceRecommendation' smart constructor.
data RealTimeInferenceRecommendation = RealTimeInferenceRecommendation'
  { -- | The recommended environment variables to set in the model container for
    -- Real-Time Inference.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The recommendation ID which uniquely identifies each recommendation.
    recommendationId :: Prelude.Text,
    -- | The recommended instance type for Real-Time Inference.
    instanceType :: ProductionVariantInstanceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RealTimeInferenceRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environment', 'realTimeInferenceRecommendation_environment' - The recommended environment variables to set in the model container for
-- Real-Time Inference.
--
-- 'recommendationId', 'realTimeInferenceRecommendation_recommendationId' - The recommendation ID which uniquely identifies each recommendation.
--
-- 'instanceType', 'realTimeInferenceRecommendation_instanceType' - The recommended instance type for Real-Time Inference.
newRealTimeInferenceRecommendation ::
  -- | 'recommendationId'
  Prelude.Text ->
  -- | 'instanceType'
  ProductionVariantInstanceType ->
  RealTimeInferenceRecommendation
newRealTimeInferenceRecommendation
  pRecommendationId_
  pInstanceType_ =
    RealTimeInferenceRecommendation'
      { environment =
          Prelude.Nothing,
        recommendationId = pRecommendationId_,
        instanceType = pInstanceType_
      }

-- | The recommended environment variables to set in the model container for
-- Real-Time Inference.
realTimeInferenceRecommendation_environment :: Lens.Lens' RealTimeInferenceRecommendation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
realTimeInferenceRecommendation_environment = Lens.lens (\RealTimeInferenceRecommendation' {environment} -> environment) (\s@RealTimeInferenceRecommendation' {} a -> s {environment = a} :: RealTimeInferenceRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | The recommendation ID which uniquely identifies each recommendation.
realTimeInferenceRecommendation_recommendationId :: Lens.Lens' RealTimeInferenceRecommendation Prelude.Text
realTimeInferenceRecommendation_recommendationId = Lens.lens (\RealTimeInferenceRecommendation' {recommendationId} -> recommendationId) (\s@RealTimeInferenceRecommendation' {} a -> s {recommendationId = a} :: RealTimeInferenceRecommendation)

-- | The recommended instance type for Real-Time Inference.
realTimeInferenceRecommendation_instanceType :: Lens.Lens' RealTimeInferenceRecommendation ProductionVariantInstanceType
realTimeInferenceRecommendation_instanceType = Lens.lens (\RealTimeInferenceRecommendation' {instanceType} -> instanceType) (\s@RealTimeInferenceRecommendation' {} a -> s {instanceType = a} :: RealTimeInferenceRecommendation)

instance
  Data.FromJSON
    RealTimeInferenceRecommendation
  where
  parseJSON =
    Data.withObject
      "RealTimeInferenceRecommendation"
      ( \x ->
          RealTimeInferenceRecommendation'
            Prelude.<$> (x Data..:? "Environment" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "RecommendationId")
            Prelude.<*> (x Data..: "InstanceType")
      )

instance
  Prelude.Hashable
    RealTimeInferenceRecommendation
  where
  hashWithSalt
    _salt
    RealTimeInferenceRecommendation' {..} =
      _salt
        `Prelude.hashWithSalt` environment
        `Prelude.hashWithSalt` recommendationId
        `Prelude.hashWithSalt` instanceType

instance
  Prelude.NFData
    RealTimeInferenceRecommendation
  where
  rnf RealTimeInferenceRecommendation' {..} =
    Prelude.rnf environment
      `Prelude.seq` Prelude.rnf recommendationId
      `Prelude.seq` Prelude.rnf instanceType
