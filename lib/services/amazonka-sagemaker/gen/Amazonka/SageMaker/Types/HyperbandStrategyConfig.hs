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
-- Module      : Amazonka.SageMaker.Types.HyperbandStrategyConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HyperbandStrategyConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration for @Hyperband@, a multi-fidelity based hyperparameter
-- tuning strategy. @Hyperband@ uses the final and intermediate results of
-- a training job to dynamically allocate resources to utilized
-- hyperparameter configurations while automatically stopping
-- under-performing configurations. This parameter should be provided only
-- if @Hyperband@ is selected as the @StrategyConfig@ under the
-- @HyperParameterTuningJobConfig@ API.
--
-- /See:/ 'newHyperbandStrategyConfig' smart constructor.
data HyperbandStrategyConfig = HyperbandStrategyConfig'
  { -- | The maximum number of resources (such as epochs) that can be used by a
    -- training job launched by a hyperparameter tuning job. Once a job reaches
    -- the @MaxResource@ value, it is stopped. If a value for @MaxResource@ is
    -- not provided, and @Hyperband@ is selected as the hyperparameter tuning
    -- strategy, @HyperbandTrainingJ@ attempts to infer @MaxResource@ from the
    -- following keys (if present) in
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_HyperParameterTrainingJobDefinition.html#sagemaker-Type-HyperParameterTrainingJobDefinition-StaticHyperParameters StaticsHyperParameters>:
    --
    -- -   @epochs@
    --
    -- -   @numepochs@
    --
    -- -   @n-epochs@
    --
    -- -   @n_epochs@
    --
    -- -   @num_epochs@
    --
    -- If @HyperbandStrategyConfig@ is unable to infer a value for
    -- @MaxResource@, it generates a validation error. The maximum value is
    -- 20,000 epochs. All metrics that correspond to an objective metric are
    -- used to derive
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-early-stopping.html early stopping decisions>.
    -- For
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/distributed-training.html distributive>
    -- training jobs, ensure that duplicate metrics are not printed in the logs
    -- across the individual nodes in a training job. If multiple nodes are
    -- publishing duplicate or incorrect metrics, training jobs may make an
    -- incorrect stopping decision and stop the job prematurely.
    maxResource :: Prelude.Maybe Prelude.Natural,
    -- | The minimum number of resources (such as epochs) that can be used by a
    -- training job launched by a hyperparameter tuning job. If the value for
    -- @MinResource@ has not been reached, the training job will not be stopped
    -- by @Hyperband@.
    minResource :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HyperbandStrategyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResource', 'hyperbandStrategyConfig_maxResource' - The maximum number of resources (such as epochs) that can be used by a
-- training job launched by a hyperparameter tuning job. Once a job reaches
-- the @MaxResource@ value, it is stopped. If a value for @MaxResource@ is
-- not provided, and @Hyperband@ is selected as the hyperparameter tuning
-- strategy, @HyperbandTrainingJ@ attempts to infer @MaxResource@ from the
-- following keys (if present) in
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_HyperParameterTrainingJobDefinition.html#sagemaker-Type-HyperParameterTrainingJobDefinition-StaticHyperParameters StaticsHyperParameters>:
--
-- -   @epochs@
--
-- -   @numepochs@
--
-- -   @n-epochs@
--
-- -   @n_epochs@
--
-- -   @num_epochs@
--
-- If @HyperbandStrategyConfig@ is unable to infer a value for
-- @MaxResource@, it generates a validation error. The maximum value is
-- 20,000 epochs. All metrics that correspond to an objective metric are
-- used to derive
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-early-stopping.html early stopping decisions>.
-- For
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/distributed-training.html distributive>
-- training jobs, ensure that duplicate metrics are not printed in the logs
-- across the individual nodes in a training job. If multiple nodes are
-- publishing duplicate or incorrect metrics, training jobs may make an
-- incorrect stopping decision and stop the job prematurely.
--
-- 'minResource', 'hyperbandStrategyConfig_minResource' - The minimum number of resources (such as epochs) that can be used by a
-- training job launched by a hyperparameter tuning job. If the value for
-- @MinResource@ has not been reached, the training job will not be stopped
-- by @Hyperband@.
newHyperbandStrategyConfig ::
  HyperbandStrategyConfig
newHyperbandStrategyConfig =
  HyperbandStrategyConfig'
    { maxResource =
        Prelude.Nothing,
      minResource = Prelude.Nothing
    }

-- | The maximum number of resources (such as epochs) that can be used by a
-- training job launched by a hyperparameter tuning job. Once a job reaches
-- the @MaxResource@ value, it is stopped. If a value for @MaxResource@ is
-- not provided, and @Hyperband@ is selected as the hyperparameter tuning
-- strategy, @HyperbandTrainingJ@ attempts to infer @MaxResource@ from the
-- following keys (if present) in
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_HyperParameterTrainingJobDefinition.html#sagemaker-Type-HyperParameterTrainingJobDefinition-StaticHyperParameters StaticsHyperParameters>:
--
-- -   @epochs@
--
-- -   @numepochs@
--
-- -   @n-epochs@
--
-- -   @n_epochs@
--
-- -   @num_epochs@
--
-- If @HyperbandStrategyConfig@ is unable to infer a value for
-- @MaxResource@, it generates a validation error. The maximum value is
-- 20,000 epochs. All metrics that correspond to an objective metric are
-- used to derive
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-early-stopping.html early stopping decisions>.
-- For
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/distributed-training.html distributive>
-- training jobs, ensure that duplicate metrics are not printed in the logs
-- across the individual nodes in a training job. If multiple nodes are
-- publishing duplicate or incorrect metrics, training jobs may make an
-- incorrect stopping decision and stop the job prematurely.
hyperbandStrategyConfig_maxResource :: Lens.Lens' HyperbandStrategyConfig (Prelude.Maybe Prelude.Natural)
hyperbandStrategyConfig_maxResource = Lens.lens (\HyperbandStrategyConfig' {maxResource} -> maxResource) (\s@HyperbandStrategyConfig' {} a -> s {maxResource = a} :: HyperbandStrategyConfig)

-- | The minimum number of resources (such as epochs) that can be used by a
-- training job launched by a hyperparameter tuning job. If the value for
-- @MinResource@ has not been reached, the training job will not be stopped
-- by @Hyperband@.
hyperbandStrategyConfig_minResource :: Lens.Lens' HyperbandStrategyConfig (Prelude.Maybe Prelude.Natural)
hyperbandStrategyConfig_minResource = Lens.lens (\HyperbandStrategyConfig' {minResource} -> minResource) (\s@HyperbandStrategyConfig' {} a -> s {minResource = a} :: HyperbandStrategyConfig)

instance Data.FromJSON HyperbandStrategyConfig where
  parseJSON =
    Data.withObject
      "HyperbandStrategyConfig"
      ( \x ->
          HyperbandStrategyConfig'
            Prelude.<$> (x Data..:? "MaxResource")
            Prelude.<*> (x Data..:? "MinResource")
      )

instance Prelude.Hashable HyperbandStrategyConfig where
  hashWithSalt _salt HyperbandStrategyConfig' {..} =
    _salt
      `Prelude.hashWithSalt` maxResource
      `Prelude.hashWithSalt` minResource

instance Prelude.NFData HyperbandStrategyConfig where
  rnf HyperbandStrategyConfig' {..} =
    Prelude.rnf maxResource
      `Prelude.seq` Prelude.rnf minResource

instance Data.ToJSON HyperbandStrategyConfig where
  toJSON HyperbandStrategyConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResource" Data..=) Prelude.<$> maxResource,
            ("MinResource" Data..=) Prelude.<$> minResource
          ]
      )
