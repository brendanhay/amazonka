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
-- Module      : Amazonka.SageMaker.Types.HyperParameterTuningInstanceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HyperParameterTuningInstanceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.TrainingInstanceType

-- | The configuration for hyperparameter tuning resources for use in
-- training jobs launched by the tuning job. These resources include
-- compute instances and storage volumes. Specify one or more compute
-- instance configurations and allocation strategies to select resources
-- (optional).
--
-- /See:/ 'newHyperParameterTuningInstanceConfig' smart constructor.
data HyperParameterTuningInstanceConfig = HyperParameterTuningInstanceConfig'
  { -- | The instance type used for processing of hyperparameter optimization
    -- jobs. Choose from general purpose (no GPUs) instance types:
    -- ml.m5.xlarge, ml.m5.2xlarge, and ml.m5.4xlarge or compute optimized (no
    -- GPUs) instance types: ml.c5.xlarge and ml.c5.2xlarge. For more
    -- information about instance types, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/notebooks-available-instance-types.html instance type descriptions>.
    instanceType :: TrainingInstanceType,
    -- | The number of instances of the type specified by @InstanceType@. Choose
    -- an instance count larger than 1 for distributed training algorithms. See
    -- <https://docs.aws.amazon.com/data-parallel-use-api.html SageMaker distributed training jobs>
    -- for more informcration.
    instanceCount :: Prelude.Natural,
    -- | The volume size in GB of the data to be processed for hyperparameter
    -- optimization (optional).
    volumeSizeInGB :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HyperParameterTuningInstanceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceType', 'hyperParameterTuningInstanceConfig_instanceType' - The instance type used for processing of hyperparameter optimization
-- jobs. Choose from general purpose (no GPUs) instance types:
-- ml.m5.xlarge, ml.m5.2xlarge, and ml.m5.4xlarge or compute optimized (no
-- GPUs) instance types: ml.c5.xlarge and ml.c5.2xlarge. For more
-- information about instance types, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/notebooks-available-instance-types.html instance type descriptions>.
--
-- 'instanceCount', 'hyperParameterTuningInstanceConfig_instanceCount' - The number of instances of the type specified by @InstanceType@. Choose
-- an instance count larger than 1 for distributed training algorithms. See
-- <https://docs.aws.amazon.com/data-parallel-use-api.html SageMaker distributed training jobs>
-- for more informcration.
--
-- 'volumeSizeInGB', 'hyperParameterTuningInstanceConfig_volumeSizeInGB' - The volume size in GB of the data to be processed for hyperparameter
-- optimization (optional).
newHyperParameterTuningInstanceConfig ::
  -- | 'instanceType'
  TrainingInstanceType ->
  -- | 'instanceCount'
  Prelude.Natural ->
  -- | 'volumeSizeInGB'
  Prelude.Natural ->
  HyperParameterTuningInstanceConfig
newHyperParameterTuningInstanceConfig
  pInstanceType_
  pInstanceCount_
  pVolumeSizeInGB_ =
    HyperParameterTuningInstanceConfig'
      { instanceType =
          pInstanceType_,
        instanceCount = pInstanceCount_,
        volumeSizeInGB = pVolumeSizeInGB_
      }

-- | The instance type used for processing of hyperparameter optimization
-- jobs. Choose from general purpose (no GPUs) instance types:
-- ml.m5.xlarge, ml.m5.2xlarge, and ml.m5.4xlarge or compute optimized (no
-- GPUs) instance types: ml.c5.xlarge and ml.c5.2xlarge. For more
-- information about instance types, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/notebooks-available-instance-types.html instance type descriptions>.
hyperParameterTuningInstanceConfig_instanceType :: Lens.Lens' HyperParameterTuningInstanceConfig TrainingInstanceType
hyperParameterTuningInstanceConfig_instanceType = Lens.lens (\HyperParameterTuningInstanceConfig' {instanceType} -> instanceType) (\s@HyperParameterTuningInstanceConfig' {} a -> s {instanceType = a} :: HyperParameterTuningInstanceConfig)

-- | The number of instances of the type specified by @InstanceType@. Choose
-- an instance count larger than 1 for distributed training algorithms. See
-- <https://docs.aws.amazon.com/data-parallel-use-api.html SageMaker distributed training jobs>
-- for more informcration.
hyperParameterTuningInstanceConfig_instanceCount :: Lens.Lens' HyperParameterTuningInstanceConfig Prelude.Natural
hyperParameterTuningInstanceConfig_instanceCount = Lens.lens (\HyperParameterTuningInstanceConfig' {instanceCount} -> instanceCount) (\s@HyperParameterTuningInstanceConfig' {} a -> s {instanceCount = a} :: HyperParameterTuningInstanceConfig)

-- | The volume size in GB of the data to be processed for hyperparameter
-- optimization (optional).
hyperParameterTuningInstanceConfig_volumeSizeInGB :: Lens.Lens' HyperParameterTuningInstanceConfig Prelude.Natural
hyperParameterTuningInstanceConfig_volumeSizeInGB = Lens.lens (\HyperParameterTuningInstanceConfig' {volumeSizeInGB} -> volumeSizeInGB) (\s@HyperParameterTuningInstanceConfig' {} a -> s {volumeSizeInGB = a} :: HyperParameterTuningInstanceConfig)

instance
  Data.FromJSON
    HyperParameterTuningInstanceConfig
  where
  parseJSON =
    Data.withObject
      "HyperParameterTuningInstanceConfig"
      ( \x ->
          HyperParameterTuningInstanceConfig'
            Prelude.<$> (x Data..: "InstanceType")
            Prelude.<*> (x Data..: "InstanceCount")
            Prelude.<*> (x Data..: "VolumeSizeInGB")
      )

instance
  Prelude.Hashable
    HyperParameterTuningInstanceConfig
  where
  hashWithSalt
    _salt
    HyperParameterTuningInstanceConfig' {..} =
      _salt `Prelude.hashWithSalt` instanceType
        `Prelude.hashWithSalt` instanceCount
        `Prelude.hashWithSalt` volumeSizeInGB

instance
  Prelude.NFData
    HyperParameterTuningInstanceConfig
  where
  rnf HyperParameterTuningInstanceConfig' {..} =
    Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf instanceCount
      `Prelude.seq` Prelude.rnf volumeSizeInGB

instance
  Data.ToJSON
    HyperParameterTuningInstanceConfig
  where
  toJSON HyperParameterTuningInstanceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceType" Data..= instanceType),
            Prelude.Just ("InstanceCount" Data..= instanceCount),
            Prelude.Just
              ("VolumeSizeInGB" Data..= volumeSizeInGB)
          ]
      )
