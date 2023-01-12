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
-- Module      : Amazonka.SageMaker.Types.HyperParameterTuningJobWarmStartConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HyperParameterTuningJobWarmStartConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.HyperParameterTuningJobWarmStartType
import Amazonka.SageMaker.Types.ParentHyperParameterTuningJob

-- | Specifies the configuration for a hyperparameter tuning job that uses
-- one or more previous hyperparameter tuning jobs as a starting point. The
-- results of previous tuning jobs are used to inform which combinations of
-- hyperparameters to search over in the new tuning job.
--
-- All training jobs launched by the new hyperparameter tuning job are
-- evaluated by using the objective metric, and the training job that
-- performs the best is compared to the best training jobs from the parent
-- tuning jobs. From these, the training job that performs the best as
-- measured by the objective metric is returned as the overall best
-- training job.
--
-- All training jobs launched by parent hyperparameter tuning jobs and the
-- new hyperparameter tuning jobs count against the limit of training jobs
-- for the tuning job.
--
-- /See:/ 'newHyperParameterTuningJobWarmStartConfig' smart constructor.
data HyperParameterTuningJobWarmStartConfig = HyperParameterTuningJobWarmStartConfig'
  { -- | An array of hyperparameter tuning jobs that are used as the starting
    -- point for the new hyperparameter tuning job. For more information about
    -- warm starting a hyperparameter tuning job, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-warm-start.html Using a Previous Hyperparameter Tuning Job as a Starting Point>.
    --
    -- Hyperparameter tuning jobs created before October 1, 2018 cannot be used
    -- as parent jobs for warm start tuning jobs.
    parentHyperParameterTuningJobs :: Prelude.NonEmpty ParentHyperParameterTuningJob,
    -- | Specifies one of the following:
    --
    -- [IDENTICAL_DATA_AND_ALGORITHM]
    --     The new hyperparameter tuning job uses the same input data and
    --     training image as the parent tuning jobs. You can change the
    --     hyperparameter ranges to search and the maximum number of training
    --     jobs that the hyperparameter tuning job launches. You cannot use a
    --     new version of the training algorithm, unless the changes in the new
    --     version do not affect the algorithm itself. For example, changes
    --     that improve logging or adding support for a different data format
    --     are allowed. You can also change hyperparameters from tunable to
    --     static, and from static to tunable, but the total number of static
    --     plus tunable hyperparameters must remain the same as it is in all
    --     parent jobs. The objective metric for the new tuning job must be the
    --     same as for all parent jobs.
    --
    -- [TRANSFER_LEARNING]
    --     The new hyperparameter tuning job can include input data,
    --     hyperparameter ranges, maximum number of concurrent training jobs,
    --     and maximum number of training jobs that are different than those of
    --     its parent hyperparameter tuning jobs. The training image can also
    --     be a different version from the version used in the parent
    --     hyperparameter tuning job. You can also change hyperparameters from
    --     tunable to static, and from static to tunable, but the total number
    --     of static plus tunable hyperparameters must remain the same as it is
    --     in all parent jobs. The objective metric for the new tuning job must
    --     be the same as for all parent jobs.
    warmStartType :: HyperParameterTuningJobWarmStartType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HyperParameterTuningJobWarmStartConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentHyperParameterTuningJobs', 'hyperParameterTuningJobWarmStartConfig_parentHyperParameterTuningJobs' - An array of hyperparameter tuning jobs that are used as the starting
-- point for the new hyperparameter tuning job. For more information about
-- warm starting a hyperparameter tuning job, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-warm-start.html Using a Previous Hyperparameter Tuning Job as a Starting Point>.
--
-- Hyperparameter tuning jobs created before October 1, 2018 cannot be used
-- as parent jobs for warm start tuning jobs.
--
-- 'warmStartType', 'hyperParameterTuningJobWarmStartConfig_warmStartType' - Specifies one of the following:
--
-- [IDENTICAL_DATA_AND_ALGORITHM]
--     The new hyperparameter tuning job uses the same input data and
--     training image as the parent tuning jobs. You can change the
--     hyperparameter ranges to search and the maximum number of training
--     jobs that the hyperparameter tuning job launches. You cannot use a
--     new version of the training algorithm, unless the changes in the new
--     version do not affect the algorithm itself. For example, changes
--     that improve logging or adding support for a different data format
--     are allowed. You can also change hyperparameters from tunable to
--     static, and from static to tunable, but the total number of static
--     plus tunable hyperparameters must remain the same as it is in all
--     parent jobs. The objective metric for the new tuning job must be the
--     same as for all parent jobs.
--
-- [TRANSFER_LEARNING]
--     The new hyperparameter tuning job can include input data,
--     hyperparameter ranges, maximum number of concurrent training jobs,
--     and maximum number of training jobs that are different than those of
--     its parent hyperparameter tuning jobs. The training image can also
--     be a different version from the version used in the parent
--     hyperparameter tuning job. You can also change hyperparameters from
--     tunable to static, and from static to tunable, but the total number
--     of static plus tunable hyperparameters must remain the same as it is
--     in all parent jobs. The objective metric for the new tuning job must
--     be the same as for all parent jobs.
newHyperParameterTuningJobWarmStartConfig ::
  -- | 'parentHyperParameterTuningJobs'
  Prelude.NonEmpty ParentHyperParameterTuningJob ->
  -- | 'warmStartType'
  HyperParameterTuningJobWarmStartType ->
  HyperParameterTuningJobWarmStartConfig
newHyperParameterTuningJobWarmStartConfig
  pParentHyperParameterTuningJobs_
  pWarmStartType_ =
    HyperParameterTuningJobWarmStartConfig'
      { parentHyperParameterTuningJobs =
          Lens.coerced
            Lens.# pParentHyperParameterTuningJobs_,
        warmStartType = pWarmStartType_
      }

-- | An array of hyperparameter tuning jobs that are used as the starting
-- point for the new hyperparameter tuning job. For more information about
-- warm starting a hyperparameter tuning job, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-warm-start.html Using a Previous Hyperparameter Tuning Job as a Starting Point>.
--
-- Hyperparameter tuning jobs created before October 1, 2018 cannot be used
-- as parent jobs for warm start tuning jobs.
hyperParameterTuningJobWarmStartConfig_parentHyperParameterTuningJobs :: Lens.Lens' HyperParameterTuningJobWarmStartConfig (Prelude.NonEmpty ParentHyperParameterTuningJob)
hyperParameterTuningJobWarmStartConfig_parentHyperParameterTuningJobs = Lens.lens (\HyperParameterTuningJobWarmStartConfig' {parentHyperParameterTuningJobs} -> parentHyperParameterTuningJobs) (\s@HyperParameterTuningJobWarmStartConfig' {} a -> s {parentHyperParameterTuningJobs = a} :: HyperParameterTuningJobWarmStartConfig) Prelude.. Lens.coerced

-- | Specifies one of the following:
--
-- [IDENTICAL_DATA_AND_ALGORITHM]
--     The new hyperparameter tuning job uses the same input data and
--     training image as the parent tuning jobs. You can change the
--     hyperparameter ranges to search and the maximum number of training
--     jobs that the hyperparameter tuning job launches. You cannot use a
--     new version of the training algorithm, unless the changes in the new
--     version do not affect the algorithm itself. For example, changes
--     that improve logging or adding support for a different data format
--     are allowed. You can also change hyperparameters from tunable to
--     static, and from static to tunable, but the total number of static
--     plus tunable hyperparameters must remain the same as it is in all
--     parent jobs. The objective metric for the new tuning job must be the
--     same as for all parent jobs.
--
-- [TRANSFER_LEARNING]
--     The new hyperparameter tuning job can include input data,
--     hyperparameter ranges, maximum number of concurrent training jobs,
--     and maximum number of training jobs that are different than those of
--     its parent hyperparameter tuning jobs. The training image can also
--     be a different version from the version used in the parent
--     hyperparameter tuning job. You can also change hyperparameters from
--     tunable to static, and from static to tunable, but the total number
--     of static plus tunable hyperparameters must remain the same as it is
--     in all parent jobs. The objective metric for the new tuning job must
--     be the same as for all parent jobs.
hyperParameterTuningJobWarmStartConfig_warmStartType :: Lens.Lens' HyperParameterTuningJobWarmStartConfig HyperParameterTuningJobWarmStartType
hyperParameterTuningJobWarmStartConfig_warmStartType = Lens.lens (\HyperParameterTuningJobWarmStartConfig' {warmStartType} -> warmStartType) (\s@HyperParameterTuningJobWarmStartConfig' {} a -> s {warmStartType = a} :: HyperParameterTuningJobWarmStartConfig)

instance
  Data.FromJSON
    HyperParameterTuningJobWarmStartConfig
  where
  parseJSON =
    Data.withObject
      "HyperParameterTuningJobWarmStartConfig"
      ( \x ->
          HyperParameterTuningJobWarmStartConfig'
            Prelude.<$> (x Data..: "ParentHyperParameterTuningJobs")
            Prelude.<*> (x Data..: "WarmStartType")
      )

instance
  Prelude.Hashable
    HyperParameterTuningJobWarmStartConfig
  where
  hashWithSalt
    _salt
    HyperParameterTuningJobWarmStartConfig' {..} =
      _salt
        `Prelude.hashWithSalt` parentHyperParameterTuningJobs
        `Prelude.hashWithSalt` warmStartType

instance
  Prelude.NFData
    HyperParameterTuningJobWarmStartConfig
  where
  rnf HyperParameterTuningJobWarmStartConfig' {..} =
    Prelude.rnf parentHyperParameterTuningJobs
      `Prelude.seq` Prelude.rnf warmStartType

instance
  Data.ToJSON
    HyperParameterTuningJobWarmStartConfig
  where
  toJSON HyperParameterTuningJobWarmStartConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ParentHyperParameterTuningJobs"
                  Data..= parentHyperParameterTuningJobs
              ),
            Prelude.Just
              ("WarmStartType" Data..= warmStartType)
          ]
      )
