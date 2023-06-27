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
-- Module      : Amazonka.SageMaker.Types.Autotune
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.Autotune where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AutotuneMode

-- | A flag to indicate if you want to use Autotune to automatically find
-- optimal values for the following fields:
--
-- -   <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_HyperParameterTuningJobConfig.html#sagemaker-Type-HyperParameterTuningJobConfig-ParameterRanges ParameterRanges>:
--     The names and ranges of parameters that a hyperparameter tuning job
--     can optimize.
--
-- -   <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_ResourceLimits.html ResourceLimits>:
--     The maximum resources that can be used for a training job. These
--     resources include the maximum number of training jobs, the maximum
--     runtime of a tuning job, and the maximum number of training jobs to
--     run at the same time.
--
-- -   <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_HyperParameterTuningJobConfig.html#sagemaker-Type-HyperParameterTuningJobConfig-TrainingJobEarlyStoppingType TrainingJobEarlyStoppingType>:
--     A flag that specifies whether or not to use early stopping for
--     training jobs launched by a hyperparameter tuning job.
--
-- -   <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_HyperParameterTrainingJobDefinition.html#sagemaker-Type-HyperParameterTrainingJobDefinition-RetryStrategy RetryStrategy>:
--     The number of times to retry a training job.
--
-- -   <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_HyperParameterTuningJobConfig.html Strategy>:
--     Specifies how hyperparameter tuning chooses the combinations of
--     hyperparameter values to use for the training jobs that it launches.
--
-- -   <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_ConvergenceDetected.html ConvergenceDetected>:
--     A flag to indicate that Automatic model tuning (AMT) has detected
--     model convergence.
--
-- /See:/ 'newAutotune' smart constructor.
data Autotune = Autotune'
  { -- | Set @Mode@ to @Enabled@ if you want to use Autotune.
    mode :: AutotuneMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Autotune' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mode', 'autotune_mode' - Set @Mode@ to @Enabled@ if you want to use Autotune.
newAutotune ::
  -- | 'mode'
  AutotuneMode ->
  Autotune
newAutotune pMode_ = Autotune' {mode = pMode_}

-- | Set @Mode@ to @Enabled@ if you want to use Autotune.
autotune_mode :: Lens.Lens' Autotune AutotuneMode
autotune_mode = Lens.lens (\Autotune' {mode} -> mode) (\s@Autotune' {} a -> s {mode = a} :: Autotune)

instance Data.FromJSON Autotune where
  parseJSON =
    Data.withObject
      "Autotune"
      (\x -> Autotune' Prelude.<$> (x Data..: "Mode"))

instance Prelude.Hashable Autotune where
  hashWithSalt _salt Autotune' {..} =
    _salt `Prelude.hashWithSalt` mode

instance Prelude.NFData Autotune where
  rnf Autotune' {..} = Prelude.rnf mode

instance Data.ToJSON Autotune where
  toJSON Autotune' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Mode" Data..= mode)]
      )
