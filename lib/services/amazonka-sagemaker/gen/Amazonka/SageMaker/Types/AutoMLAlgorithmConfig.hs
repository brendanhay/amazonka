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
-- Module      : Amazonka.SageMaker.Types.AutoMLAlgorithmConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLAlgorithmConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AutoMLAlgorithm

-- | The collection of algorithms run on a dataset for training the model
-- candidates of an Autopilot job.
--
-- /See:/ 'newAutoMLAlgorithmConfig' smart constructor.
data AutoMLAlgorithmConfig = AutoMLAlgorithmConfig'
  { -- | The selection of algorithms run on a dataset to train the model
    -- candidates of an Autopilot job.
    --
    -- Selected algorithms must belong to the list corresponding to the
    -- training mode set in
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_AutoMLJobConfig.html#sagemaker-Type-AutoMLJobConfig-Mode AutoMLJobConfig.Mode>
    -- (@ENSEMBLING@ or @HYPERPARAMETER_TUNING@). Choose a minimum of 1
    -- algorithm.
    --
    -- -   In @ENSEMBLING@ mode:
    --
    --     -   \"catboost\"
    --
    --     -   \"extra-trees\"
    --
    --     -   \"fastai\"
    --
    --     -   \"lightgbm\"
    --
    --     -   \"linear-learner\"
    --
    --     -   \"nn-torch\"
    --
    --     -   \"randomforest\"
    --
    --     -   \"xgboost\"
    --
    -- -   In @HYPERPARAMETER_TUNING@ mode:
    --
    --     -   \"linear-learner\"
    --
    --     -   \"mlp\"
    --
    --     -   \"xgboost\"
    autoMLAlgorithms :: [AutoMLAlgorithm]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLAlgorithmConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoMLAlgorithms', 'autoMLAlgorithmConfig_autoMLAlgorithms' - The selection of algorithms run on a dataset to train the model
-- candidates of an Autopilot job.
--
-- Selected algorithms must belong to the list corresponding to the
-- training mode set in
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_AutoMLJobConfig.html#sagemaker-Type-AutoMLJobConfig-Mode AutoMLJobConfig.Mode>
-- (@ENSEMBLING@ or @HYPERPARAMETER_TUNING@). Choose a minimum of 1
-- algorithm.
--
-- -   In @ENSEMBLING@ mode:
--
--     -   \"catboost\"
--
--     -   \"extra-trees\"
--
--     -   \"fastai\"
--
--     -   \"lightgbm\"
--
--     -   \"linear-learner\"
--
--     -   \"nn-torch\"
--
--     -   \"randomforest\"
--
--     -   \"xgboost\"
--
-- -   In @HYPERPARAMETER_TUNING@ mode:
--
--     -   \"linear-learner\"
--
--     -   \"mlp\"
--
--     -   \"xgboost\"
newAutoMLAlgorithmConfig ::
  AutoMLAlgorithmConfig
newAutoMLAlgorithmConfig =
  AutoMLAlgorithmConfig'
    { autoMLAlgorithms =
        Prelude.mempty
    }

-- | The selection of algorithms run on a dataset to train the model
-- candidates of an Autopilot job.
--
-- Selected algorithms must belong to the list corresponding to the
-- training mode set in
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_AutoMLJobConfig.html#sagemaker-Type-AutoMLJobConfig-Mode AutoMLJobConfig.Mode>
-- (@ENSEMBLING@ or @HYPERPARAMETER_TUNING@). Choose a minimum of 1
-- algorithm.
--
-- -   In @ENSEMBLING@ mode:
--
--     -   \"catboost\"
--
--     -   \"extra-trees\"
--
--     -   \"fastai\"
--
--     -   \"lightgbm\"
--
--     -   \"linear-learner\"
--
--     -   \"nn-torch\"
--
--     -   \"randomforest\"
--
--     -   \"xgboost\"
--
-- -   In @HYPERPARAMETER_TUNING@ mode:
--
--     -   \"linear-learner\"
--
--     -   \"mlp\"
--
--     -   \"xgboost\"
autoMLAlgorithmConfig_autoMLAlgorithms :: Lens.Lens' AutoMLAlgorithmConfig [AutoMLAlgorithm]
autoMLAlgorithmConfig_autoMLAlgorithms = Lens.lens (\AutoMLAlgorithmConfig' {autoMLAlgorithms} -> autoMLAlgorithms) (\s@AutoMLAlgorithmConfig' {} a -> s {autoMLAlgorithms = a} :: AutoMLAlgorithmConfig) Prelude.. Lens.coerced

instance Data.FromJSON AutoMLAlgorithmConfig where
  parseJSON =
    Data.withObject
      "AutoMLAlgorithmConfig"
      ( \x ->
          AutoMLAlgorithmConfig'
            Prelude.<$> ( x
                            Data..:? "AutoMLAlgorithms"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AutoMLAlgorithmConfig where
  hashWithSalt _salt AutoMLAlgorithmConfig' {..} =
    _salt `Prelude.hashWithSalt` autoMLAlgorithms

instance Prelude.NFData AutoMLAlgorithmConfig where
  rnf AutoMLAlgorithmConfig' {..} =
    Prelude.rnf autoMLAlgorithms

instance Data.ToJSON AutoMLAlgorithmConfig where
  toJSON AutoMLAlgorithmConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AutoMLAlgorithms" Data..= autoMLAlgorithms)
          ]
      )
