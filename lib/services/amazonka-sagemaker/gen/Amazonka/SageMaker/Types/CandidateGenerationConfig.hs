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
-- Module      : Amazonka.SageMaker.Types.CandidateGenerationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CandidateGenerationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AutoMLAlgorithmConfig

-- | Stores the configuration information for how model candidates are
-- generated using an AutoML job V2.
--
-- /See:/ 'newCandidateGenerationConfig' smart constructor.
data CandidateGenerationConfig = CandidateGenerationConfig'
  { -- | Stores the configuration information for the selection of algorithms
    -- used to train model candidates on tabular data.
    --
    -- The list of available algorithms to choose from depends on the training
    -- mode set in
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_TabularJobConfig.html TabularJobConfig.Mode>
    -- .
    --
    -- -   @AlgorithmsConfig@ should not be set in @AUTO@ training mode.
    --
    -- -   When @AlgorithmsConfig@ is provided, one @AutoMLAlgorithms@
    --     attribute must be set and one only.
    --
    --     If the list of algorithms provided as values for @AutoMLAlgorithms@
    --     is empty, @CandidateGenerationConfig@ uses the full set of
    --     algorithms for the given training mode.
    --
    -- -   When @AlgorithmsConfig@ is not provided, @CandidateGenerationConfig@
    --     uses the full set of algorithms for the given training mode.
    --
    -- For the list of all algorithms per problem type and training mode, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_AutoMLAlgorithmConfig.html AutoMLAlgorithmConfig>.
    --
    -- For more information on each algorithm, see the
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-model-support-validation.html#autopilot-algorithm-support Algorithm support>
    -- section in Autopilot developer guide.
    algorithmsConfig :: Prelude.Maybe [AutoMLAlgorithmConfig]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CandidateGenerationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithmsConfig', 'candidateGenerationConfig_algorithmsConfig' - Stores the configuration information for the selection of algorithms
-- used to train model candidates on tabular data.
--
-- The list of available algorithms to choose from depends on the training
-- mode set in
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_TabularJobConfig.html TabularJobConfig.Mode>
-- .
--
-- -   @AlgorithmsConfig@ should not be set in @AUTO@ training mode.
--
-- -   When @AlgorithmsConfig@ is provided, one @AutoMLAlgorithms@
--     attribute must be set and one only.
--
--     If the list of algorithms provided as values for @AutoMLAlgorithms@
--     is empty, @CandidateGenerationConfig@ uses the full set of
--     algorithms for the given training mode.
--
-- -   When @AlgorithmsConfig@ is not provided, @CandidateGenerationConfig@
--     uses the full set of algorithms for the given training mode.
--
-- For the list of all algorithms per problem type and training mode, see
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_AutoMLAlgorithmConfig.html AutoMLAlgorithmConfig>.
--
-- For more information on each algorithm, see the
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-model-support-validation.html#autopilot-algorithm-support Algorithm support>
-- section in Autopilot developer guide.
newCandidateGenerationConfig ::
  CandidateGenerationConfig
newCandidateGenerationConfig =
  CandidateGenerationConfig'
    { algorithmsConfig =
        Prelude.Nothing
    }

-- | Stores the configuration information for the selection of algorithms
-- used to train model candidates on tabular data.
--
-- The list of available algorithms to choose from depends on the training
-- mode set in
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_TabularJobConfig.html TabularJobConfig.Mode>
-- .
--
-- -   @AlgorithmsConfig@ should not be set in @AUTO@ training mode.
--
-- -   When @AlgorithmsConfig@ is provided, one @AutoMLAlgorithms@
--     attribute must be set and one only.
--
--     If the list of algorithms provided as values for @AutoMLAlgorithms@
--     is empty, @CandidateGenerationConfig@ uses the full set of
--     algorithms for the given training mode.
--
-- -   When @AlgorithmsConfig@ is not provided, @CandidateGenerationConfig@
--     uses the full set of algorithms for the given training mode.
--
-- For the list of all algorithms per problem type and training mode, see
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_AutoMLAlgorithmConfig.html AutoMLAlgorithmConfig>.
--
-- For more information on each algorithm, see the
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-model-support-validation.html#autopilot-algorithm-support Algorithm support>
-- section in Autopilot developer guide.
candidateGenerationConfig_algorithmsConfig :: Lens.Lens' CandidateGenerationConfig (Prelude.Maybe [AutoMLAlgorithmConfig])
candidateGenerationConfig_algorithmsConfig = Lens.lens (\CandidateGenerationConfig' {algorithmsConfig} -> algorithmsConfig) (\s@CandidateGenerationConfig' {} a -> s {algorithmsConfig = a} :: CandidateGenerationConfig) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CandidateGenerationConfig where
  parseJSON =
    Data.withObject
      "CandidateGenerationConfig"
      ( \x ->
          CandidateGenerationConfig'
            Prelude.<$> ( x
                            Data..:? "AlgorithmsConfig"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CandidateGenerationConfig where
  hashWithSalt _salt CandidateGenerationConfig' {..} =
    _salt `Prelude.hashWithSalt` algorithmsConfig

instance Prelude.NFData CandidateGenerationConfig where
  rnf CandidateGenerationConfig' {..} =
    Prelude.rnf algorithmsConfig

instance Data.ToJSON CandidateGenerationConfig where
  toJSON CandidateGenerationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AlgorithmsConfig" Data..=)
              Prelude.<$> algorithmsConfig
          ]
      )
