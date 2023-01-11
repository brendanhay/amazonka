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
-- Module      : Amazonka.SageMaker.Types.AutoMLJobConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLJobConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AutoMLCandidateGenerationConfig
import Amazonka.SageMaker.Types.AutoMLDataSplitConfig
import Amazonka.SageMaker.Types.AutoMLJobCompletionCriteria
import Amazonka.SageMaker.Types.AutoMLMode
import Amazonka.SageMaker.Types.AutoMLSecurityConfig

-- | A collection of settings used for an AutoML job.
--
-- /See:/ 'newAutoMLJobConfig' smart constructor.
data AutoMLJobConfig = AutoMLJobConfig'
  { -- | The configuration for generating a candidate for an AutoML job
    -- (optional).
    candidateGenerationConfig :: Prelude.Maybe AutoMLCandidateGenerationConfig,
    -- | How long an AutoML job is allowed to run, or how many candidates a job
    -- is allowed to generate.
    completionCriteria :: Prelude.Maybe AutoMLJobCompletionCriteria,
    -- | The configuration for splitting the input training dataset.
    --
    -- Type: AutoMLDataSplitConfig
    dataSplitConfig :: Prelude.Maybe AutoMLDataSplitConfig,
    -- | The method that Autopilot uses to train the data. You can either specify
    -- the mode manually or let Autopilot choose for you based on the dataset
    -- size by selecting @AUTO@. In @AUTO@ mode, Autopilot chooses @ENSEMBLING@
    -- for datasets smaller than 100 MB, and @HYPERPARAMETER_TUNING@ for larger
    -- ones.
    --
    -- The @ENSEMBLING@ mode uses a multi-stack ensemble model to predict
    -- classification and regression tasks directly from your dataset. This
    -- machine learning mode combines several base models to produce an optimal
    -- predictive model. It then uses a stacking ensemble method to combine
    -- predictions from contributing members. A multi-stack ensemble model can
    -- provide better performance over a single model by combining the
    -- predictive capabilities of multiple models. See
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-model-support-validation.html#autopilot-algorithm-suppprt Autopilot algorithm support>
    -- for a list of algorithms supported by @ENSEMBLING@ mode.
    --
    -- The @HYPERPARAMETER_TUNING@ (HPO) mode uses the best hyperparameters to
    -- train the best version of a model. HPO will automatically select an
    -- algorithm for the type of problem you want to solve. Then HPO finds the
    -- best hyperparameters according to your objective metric. See
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-model-support-validation.html#autopilot-algorithm-suppprt Autopilot algorithm support>
    -- for a list of algorithms supported by @HYPERPARAMETER_TUNING@ mode.
    mode :: Prelude.Maybe AutoMLMode,
    -- | The security configuration for traffic encryption or Amazon VPC
    -- settings.
    securityConfig :: Prelude.Maybe AutoMLSecurityConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLJobConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'candidateGenerationConfig', 'autoMLJobConfig_candidateGenerationConfig' - The configuration for generating a candidate for an AutoML job
-- (optional).
--
-- 'completionCriteria', 'autoMLJobConfig_completionCriteria' - How long an AutoML job is allowed to run, or how many candidates a job
-- is allowed to generate.
--
-- 'dataSplitConfig', 'autoMLJobConfig_dataSplitConfig' - The configuration for splitting the input training dataset.
--
-- Type: AutoMLDataSplitConfig
--
-- 'mode', 'autoMLJobConfig_mode' - The method that Autopilot uses to train the data. You can either specify
-- the mode manually or let Autopilot choose for you based on the dataset
-- size by selecting @AUTO@. In @AUTO@ mode, Autopilot chooses @ENSEMBLING@
-- for datasets smaller than 100 MB, and @HYPERPARAMETER_TUNING@ for larger
-- ones.
--
-- The @ENSEMBLING@ mode uses a multi-stack ensemble model to predict
-- classification and regression tasks directly from your dataset. This
-- machine learning mode combines several base models to produce an optimal
-- predictive model. It then uses a stacking ensemble method to combine
-- predictions from contributing members. A multi-stack ensemble model can
-- provide better performance over a single model by combining the
-- predictive capabilities of multiple models. See
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-model-support-validation.html#autopilot-algorithm-suppprt Autopilot algorithm support>
-- for a list of algorithms supported by @ENSEMBLING@ mode.
--
-- The @HYPERPARAMETER_TUNING@ (HPO) mode uses the best hyperparameters to
-- train the best version of a model. HPO will automatically select an
-- algorithm for the type of problem you want to solve. Then HPO finds the
-- best hyperparameters according to your objective metric. See
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-model-support-validation.html#autopilot-algorithm-suppprt Autopilot algorithm support>
-- for a list of algorithms supported by @HYPERPARAMETER_TUNING@ mode.
--
-- 'securityConfig', 'autoMLJobConfig_securityConfig' - The security configuration for traffic encryption or Amazon VPC
-- settings.
newAutoMLJobConfig ::
  AutoMLJobConfig
newAutoMLJobConfig =
  AutoMLJobConfig'
    { candidateGenerationConfig =
        Prelude.Nothing,
      completionCriteria = Prelude.Nothing,
      dataSplitConfig = Prelude.Nothing,
      mode = Prelude.Nothing,
      securityConfig = Prelude.Nothing
    }

-- | The configuration for generating a candidate for an AutoML job
-- (optional).
autoMLJobConfig_candidateGenerationConfig :: Lens.Lens' AutoMLJobConfig (Prelude.Maybe AutoMLCandidateGenerationConfig)
autoMLJobConfig_candidateGenerationConfig = Lens.lens (\AutoMLJobConfig' {candidateGenerationConfig} -> candidateGenerationConfig) (\s@AutoMLJobConfig' {} a -> s {candidateGenerationConfig = a} :: AutoMLJobConfig)

-- | How long an AutoML job is allowed to run, or how many candidates a job
-- is allowed to generate.
autoMLJobConfig_completionCriteria :: Lens.Lens' AutoMLJobConfig (Prelude.Maybe AutoMLJobCompletionCriteria)
autoMLJobConfig_completionCriteria = Lens.lens (\AutoMLJobConfig' {completionCriteria} -> completionCriteria) (\s@AutoMLJobConfig' {} a -> s {completionCriteria = a} :: AutoMLJobConfig)

-- | The configuration for splitting the input training dataset.
--
-- Type: AutoMLDataSplitConfig
autoMLJobConfig_dataSplitConfig :: Lens.Lens' AutoMLJobConfig (Prelude.Maybe AutoMLDataSplitConfig)
autoMLJobConfig_dataSplitConfig = Lens.lens (\AutoMLJobConfig' {dataSplitConfig} -> dataSplitConfig) (\s@AutoMLJobConfig' {} a -> s {dataSplitConfig = a} :: AutoMLJobConfig)

-- | The method that Autopilot uses to train the data. You can either specify
-- the mode manually or let Autopilot choose for you based on the dataset
-- size by selecting @AUTO@. In @AUTO@ mode, Autopilot chooses @ENSEMBLING@
-- for datasets smaller than 100 MB, and @HYPERPARAMETER_TUNING@ for larger
-- ones.
--
-- The @ENSEMBLING@ mode uses a multi-stack ensemble model to predict
-- classification and regression tasks directly from your dataset. This
-- machine learning mode combines several base models to produce an optimal
-- predictive model. It then uses a stacking ensemble method to combine
-- predictions from contributing members. A multi-stack ensemble model can
-- provide better performance over a single model by combining the
-- predictive capabilities of multiple models. See
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-model-support-validation.html#autopilot-algorithm-suppprt Autopilot algorithm support>
-- for a list of algorithms supported by @ENSEMBLING@ mode.
--
-- The @HYPERPARAMETER_TUNING@ (HPO) mode uses the best hyperparameters to
-- train the best version of a model. HPO will automatically select an
-- algorithm for the type of problem you want to solve. Then HPO finds the
-- best hyperparameters according to your objective metric. See
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-model-support-validation.html#autopilot-algorithm-suppprt Autopilot algorithm support>
-- for a list of algorithms supported by @HYPERPARAMETER_TUNING@ mode.
autoMLJobConfig_mode :: Lens.Lens' AutoMLJobConfig (Prelude.Maybe AutoMLMode)
autoMLJobConfig_mode = Lens.lens (\AutoMLJobConfig' {mode} -> mode) (\s@AutoMLJobConfig' {} a -> s {mode = a} :: AutoMLJobConfig)

-- | The security configuration for traffic encryption or Amazon VPC
-- settings.
autoMLJobConfig_securityConfig :: Lens.Lens' AutoMLJobConfig (Prelude.Maybe AutoMLSecurityConfig)
autoMLJobConfig_securityConfig = Lens.lens (\AutoMLJobConfig' {securityConfig} -> securityConfig) (\s@AutoMLJobConfig' {} a -> s {securityConfig = a} :: AutoMLJobConfig)

instance Data.FromJSON AutoMLJobConfig where
  parseJSON =
    Data.withObject
      "AutoMLJobConfig"
      ( \x ->
          AutoMLJobConfig'
            Prelude.<$> (x Data..:? "CandidateGenerationConfig")
            Prelude.<*> (x Data..:? "CompletionCriteria")
            Prelude.<*> (x Data..:? "DataSplitConfig")
            Prelude.<*> (x Data..:? "Mode")
            Prelude.<*> (x Data..:? "SecurityConfig")
      )

instance Prelude.Hashable AutoMLJobConfig where
  hashWithSalt _salt AutoMLJobConfig' {..} =
    _salt
      `Prelude.hashWithSalt` candidateGenerationConfig
      `Prelude.hashWithSalt` completionCriteria
      `Prelude.hashWithSalt` dataSplitConfig
      `Prelude.hashWithSalt` mode
      `Prelude.hashWithSalt` securityConfig

instance Prelude.NFData AutoMLJobConfig where
  rnf AutoMLJobConfig' {..} =
    Prelude.rnf candidateGenerationConfig
      `Prelude.seq` Prelude.rnf completionCriteria
      `Prelude.seq` Prelude.rnf dataSplitConfig
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf securityConfig

instance Data.ToJSON AutoMLJobConfig where
  toJSON AutoMLJobConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CandidateGenerationConfig" Data..=)
              Prelude.<$> candidateGenerationConfig,
            ("CompletionCriteria" Data..=)
              Prelude.<$> completionCriteria,
            ("DataSplitConfig" Data..=)
              Prelude.<$> dataSplitConfig,
            ("Mode" Data..=) Prelude.<$> mode,
            ("SecurityConfig" Data..=)
              Prelude.<$> securityConfig
          ]
      )
