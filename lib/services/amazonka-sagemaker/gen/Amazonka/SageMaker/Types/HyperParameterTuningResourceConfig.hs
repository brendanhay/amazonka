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
-- Module      : Amazonka.SageMaker.Types.HyperParameterTuningResourceConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HyperParameterTuningResourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.HyperParameterTuningAllocationStrategy
import Amazonka.SageMaker.Types.HyperParameterTuningInstanceConfig
import Amazonka.SageMaker.Types.TrainingInstanceType

-- | The configuration of resources, including compute instances and storage
-- volumes for use in training jobs launched by hyperparameter tuning jobs.
-- Specify one or more instance type and count and the allocation strategy
-- for instance selection.
--
-- @HyperParameterTuningResourceConfig@ supports all of the capabilities of
-- ResourceConfig with added functionality for flexible instance
-- management.
--
-- /See:/ 'newHyperParameterTuningResourceConfig' smart constructor.
data HyperParameterTuningResourceConfig = HyperParameterTuningResourceConfig'
  { -- | A key used by Amazon Web Services Key Management Service to encrypt data
    -- on the storage volume attached to the compute instances used to run the
    -- training job. You can use either of the following formats to specify a
    -- key.
    --
    -- KMS Key ID:
    --
    -- @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- Amazon Resource Name (ARN) of a KMS key:
    --
    -- @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- Some instances use local storage, which use a
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ssd-instance-store.html hardware module to encrypt>
    -- storage volumes. If you choose one of these instance types, you cannot
    -- request a @VolumeKmsKeyId@. For a list of instance types that use local
    -- storage, see
    -- <http://aws.amazon.com/releasenotes/host-instance-storage-volumes-table/ instance store volumes>.
    -- For more information about Amazon Web Services Key Management Service,
    -- see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-security-kms-permissions.html KMS encryption>
    -- for more information.
    volumeKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The instance type used to run hyperparameter optimization tuning jobs.
    -- See
    -- <https://docs.aws.amazon.com/notebooks-available-instance-types.html descriptions of instance types>
    -- for more information.
    instanceType :: Prelude.Maybe TrainingInstanceType,
    -- | The strategy that determines the order of preference for resources
    -- specified in @InstanceConfigs@ used in hyperparameter optimization.
    allocationStrategy :: Prelude.Maybe HyperParameterTuningAllocationStrategy,
    -- | The number of compute instances of type @InstanceType@ to use. For
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/data-parallel-use-api.html distributed training>,
    -- select a value greater than 1.
    instanceCount :: Prelude.Maybe Prelude.Natural,
    -- | A list containing the configuration(s) for one or more resources for
    -- processing hyperparameter jobs. These resources include compute
    -- instances and storage volumes to use in model training jobs launched by
    -- hyperparameter tuning jobs. The @AllocationStrategy@ controls the order
    -- in which multiple configurations provided in @InstanceConfigs@ are used.
    --
    -- If you only want to use a single instance configuration inside the
    -- @HyperParameterTuningResourceConfig@ API, do not provide a value for
    -- @InstanceConfigs@. Instead, use @InstanceType@, @VolumeSizeInGB@ and
    -- @InstanceCount@. If you use @InstanceConfigs@, do not provide values for
    -- @InstanceType@, @VolumeSizeInGB@ or @InstanceCount@.
    instanceConfigs :: Prelude.Maybe (Prelude.NonEmpty HyperParameterTuningInstanceConfig),
    -- | The volume size in GB for the storage volume to be used in processing
    -- hyperparameter optimization jobs (optional). These volumes store model
    -- artifacts, incremental states and optionally, scratch space for training
    -- algorithms. Do not provide a value for this parameter if a value for
    -- @InstanceConfigs@ is also specified.
    --
    -- Some instance types have a fixed total local storage size. If you select
    -- one of these instances for training, @VolumeSizeInGB@ cannot be greater
    -- than this total size. For a list of instance types with local instance
    -- storage and their sizes, see
    -- <http://aws.amazon.com/releasenotes/host-instance-storage-volumes-table/ instance store volumes>.
    --
    -- SageMaker supports only the
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-volume-types.html General Purpose SSD (gp2)>
    -- storage volume type.
    volumeSizeInGB :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HyperParameterTuningResourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeKmsKeyId', 'hyperParameterTuningResourceConfig_volumeKmsKeyId' - A key used by Amazon Web Services Key Management Service to encrypt data
-- on the storage volume attached to the compute instances used to run the
-- training job. You can use either of the following formats to specify a
-- key.
--
-- KMS Key ID:
--
-- @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- Amazon Resource Name (ARN) of a KMS key:
--
-- @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- Some instances use local storage, which use a
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ssd-instance-store.html hardware module to encrypt>
-- storage volumes. If you choose one of these instance types, you cannot
-- request a @VolumeKmsKeyId@. For a list of instance types that use local
-- storage, see
-- <http://aws.amazon.com/releasenotes/host-instance-storage-volumes-table/ instance store volumes>.
-- For more information about Amazon Web Services Key Management Service,
-- see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-security-kms-permissions.html KMS encryption>
-- for more information.
--
-- 'instanceType', 'hyperParameterTuningResourceConfig_instanceType' - The instance type used to run hyperparameter optimization tuning jobs.
-- See
-- <https://docs.aws.amazon.com/notebooks-available-instance-types.html descriptions of instance types>
-- for more information.
--
-- 'allocationStrategy', 'hyperParameterTuningResourceConfig_allocationStrategy' - The strategy that determines the order of preference for resources
-- specified in @InstanceConfigs@ used in hyperparameter optimization.
--
-- 'instanceCount', 'hyperParameterTuningResourceConfig_instanceCount' - The number of compute instances of type @InstanceType@ to use. For
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/data-parallel-use-api.html distributed training>,
-- select a value greater than 1.
--
-- 'instanceConfigs', 'hyperParameterTuningResourceConfig_instanceConfigs' - A list containing the configuration(s) for one or more resources for
-- processing hyperparameter jobs. These resources include compute
-- instances and storage volumes to use in model training jobs launched by
-- hyperparameter tuning jobs. The @AllocationStrategy@ controls the order
-- in which multiple configurations provided in @InstanceConfigs@ are used.
--
-- If you only want to use a single instance configuration inside the
-- @HyperParameterTuningResourceConfig@ API, do not provide a value for
-- @InstanceConfigs@. Instead, use @InstanceType@, @VolumeSizeInGB@ and
-- @InstanceCount@. If you use @InstanceConfigs@, do not provide values for
-- @InstanceType@, @VolumeSizeInGB@ or @InstanceCount@.
--
-- 'volumeSizeInGB', 'hyperParameterTuningResourceConfig_volumeSizeInGB' - The volume size in GB for the storage volume to be used in processing
-- hyperparameter optimization jobs (optional). These volumes store model
-- artifacts, incremental states and optionally, scratch space for training
-- algorithms. Do not provide a value for this parameter if a value for
-- @InstanceConfigs@ is also specified.
--
-- Some instance types have a fixed total local storage size. If you select
-- one of these instances for training, @VolumeSizeInGB@ cannot be greater
-- than this total size. For a list of instance types with local instance
-- storage and their sizes, see
-- <http://aws.amazon.com/releasenotes/host-instance-storage-volumes-table/ instance store volumes>.
--
-- SageMaker supports only the
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-volume-types.html General Purpose SSD (gp2)>
-- storage volume type.
newHyperParameterTuningResourceConfig ::
  HyperParameterTuningResourceConfig
newHyperParameterTuningResourceConfig =
  HyperParameterTuningResourceConfig'
    { volumeKmsKeyId =
        Prelude.Nothing,
      instanceType = Prelude.Nothing,
      allocationStrategy = Prelude.Nothing,
      instanceCount = Prelude.Nothing,
      instanceConfigs = Prelude.Nothing,
      volumeSizeInGB = Prelude.Nothing
    }

-- | A key used by Amazon Web Services Key Management Service to encrypt data
-- on the storage volume attached to the compute instances used to run the
-- training job. You can use either of the following formats to specify a
-- key.
--
-- KMS Key ID:
--
-- @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- Amazon Resource Name (ARN) of a KMS key:
--
-- @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- Some instances use local storage, which use a
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ssd-instance-store.html hardware module to encrypt>
-- storage volumes. If you choose one of these instance types, you cannot
-- request a @VolumeKmsKeyId@. For a list of instance types that use local
-- storage, see
-- <http://aws.amazon.com/releasenotes/host-instance-storage-volumes-table/ instance store volumes>.
-- For more information about Amazon Web Services Key Management Service,
-- see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-security-kms-permissions.html KMS encryption>
-- for more information.
hyperParameterTuningResourceConfig_volumeKmsKeyId :: Lens.Lens' HyperParameterTuningResourceConfig (Prelude.Maybe Prelude.Text)
hyperParameterTuningResourceConfig_volumeKmsKeyId = Lens.lens (\HyperParameterTuningResourceConfig' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@HyperParameterTuningResourceConfig' {} a -> s {volumeKmsKeyId = a} :: HyperParameterTuningResourceConfig)

-- | The instance type used to run hyperparameter optimization tuning jobs.
-- See
-- <https://docs.aws.amazon.com/notebooks-available-instance-types.html descriptions of instance types>
-- for more information.
hyperParameterTuningResourceConfig_instanceType :: Lens.Lens' HyperParameterTuningResourceConfig (Prelude.Maybe TrainingInstanceType)
hyperParameterTuningResourceConfig_instanceType = Lens.lens (\HyperParameterTuningResourceConfig' {instanceType} -> instanceType) (\s@HyperParameterTuningResourceConfig' {} a -> s {instanceType = a} :: HyperParameterTuningResourceConfig)

-- | The strategy that determines the order of preference for resources
-- specified in @InstanceConfigs@ used in hyperparameter optimization.
hyperParameterTuningResourceConfig_allocationStrategy :: Lens.Lens' HyperParameterTuningResourceConfig (Prelude.Maybe HyperParameterTuningAllocationStrategy)
hyperParameterTuningResourceConfig_allocationStrategy = Lens.lens (\HyperParameterTuningResourceConfig' {allocationStrategy} -> allocationStrategy) (\s@HyperParameterTuningResourceConfig' {} a -> s {allocationStrategy = a} :: HyperParameterTuningResourceConfig)

-- | The number of compute instances of type @InstanceType@ to use. For
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/data-parallel-use-api.html distributed training>,
-- select a value greater than 1.
hyperParameterTuningResourceConfig_instanceCount :: Lens.Lens' HyperParameterTuningResourceConfig (Prelude.Maybe Prelude.Natural)
hyperParameterTuningResourceConfig_instanceCount = Lens.lens (\HyperParameterTuningResourceConfig' {instanceCount} -> instanceCount) (\s@HyperParameterTuningResourceConfig' {} a -> s {instanceCount = a} :: HyperParameterTuningResourceConfig)

-- | A list containing the configuration(s) for one or more resources for
-- processing hyperparameter jobs. These resources include compute
-- instances and storage volumes to use in model training jobs launched by
-- hyperparameter tuning jobs. The @AllocationStrategy@ controls the order
-- in which multiple configurations provided in @InstanceConfigs@ are used.
--
-- If you only want to use a single instance configuration inside the
-- @HyperParameterTuningResourceConfig@ API, do not provide a value for
-- @InstanceConfigs@. Instead, use @InstanceType@, @VolumeSizeInGB@ and
-- @InstanceCount@. If you use @InstanceConfigs@, do not provide values for
-- @InstanceType@, @VolumeSizeInGB@ or @InstanceCount@.
hyperParameterTuningResourceConfig_instanceConfigs :: Lens.Lens' HyperParameterTuningResourceConfig (Prelude.Maybe (Prelude.NonEmpty HyperParameterTuningInstanceConfig))
hyperParameterTuningResourceConfig_instanceConfigs = Lens.lens (\HyperParameterTuningResourceConfig' {instanceConfigs} -> instanceConfigs) (\s@HyperParameterTuningResourceConfig' {} a -> s {instanceConfigs = a} :: HyperParameterTuningResourceConfig) Prelude.. Lens.mapping Lens.coerced

-- | The volume size in GB for the storage volume to be used in processing
-- hyperparameter optimization jobs (optional). These volumes store model
-- artifacts, incremental states and optionally, scratch space for training
-- algorithms. Do not provide a value for this parameter if a value for
-- @InstanceConfigs@ is also specified.
--
-- Some instance types have a fixed total local storage size. If you select
-- one of these instances for training, @VolumeSizeInGB@ cannot be greater
-- than this total size. For a list of instance types with local instance
-- storage and their sizes, see
-- <http://aws.amazon.com/releasenotes/host-instance-storage-volumes-table/ instance store volumes>.
--
-- SageMaker supports only the
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-volume-types.html General Purpose SSD (gp2)>
-- storage volume type.
hyperParameterTuningResourceConfig_volumeSizeInGB :: Lens.Lens' HyperParameterTuningResourceConfig (Prelude.Maybe Prelude.Natural)
hyperParameterTuningResourceConfig_volumeSizeInGB = Lens.lens (\HyperParameterTuningResourceConfig' {volumeSizeInGB} -> volumeSizeInGB) (\s@HyperParameterTuningResourceConfig' {} a -> s {volumeSizeInGB = a} :: HyperParameterTuningResourceConfig)

instance
  Core.FromJSON
    HyperParameterTuningResourceConfig
  where
  parseJSON =
    Core.withObject
      "HyperParameterTuningResourceConfig"
      ( \x ->
          HyperParameterTuningResourceConfig'
            Prelude.<$> (x Core..:? "VolumeKmsKeyId")
            Prelude.<*> (x Core..:? "InstanceType")
            Prelude.<*> (x Core..:? "AllocationStrategy")
            Prelude.<*> (x Core..:? "InstanceCount")
            Prelude.<*> (x Core..:? "InstanceConfigs")
            Prelude.<*> (x Core..:? "VolumeSizeInGB")
      )

instance
  Prelude.Hashable
    HyperParameterTuningResourceConfig
  where
  hashWithSalt
    _salt
    HyperParameterTuningResourceConfig' {..} =
      _salt `Prelude.hashWithSalt` volumeKmsKeyId
        `Prelude.hashWithSalt` instanceType
        `Prelude.hashWithSalt` allocationStrategy
        `Prelude.hashWithSalt` instanceCount
        `Prelude.hashWithSalt` instanceConfigs
        `Prelude.hashWithSalt` volumeSizeInGB

instance
  Prelude.NFData
    HyperParameterTuningResourceConfig
  where
  rnf HyperParameterTuningResourceConfig' {..} =
    Prelude.rnf volumeKmsKeyId
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf allocationStrategy
      `Prelude.seq` Prelude.rnf instanceCount
      `Prelude.seq` Prelude.rnf instanceConfigs
      `Prelude.seq` Prelude.rnf volumeSizeInGB

instance
  Core.ToJSON
    HyperParameterTuningResourceConfig
  where
  toJSON HyperParameterTuningResourceConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VolumeKmsKeyId" Core..=)
              Prelude.<$> volumeKmsKeyId,
            ("InstanceType" Core..=) Prelude.<$> instanceType,
            ("AllocationStrategy" Core..=)
              Prelude.<$> allocationStrategy,
            ("InstanceCount" Core..=) Prelude.<$> instanceCount,
            ("InstanceConfigs" Core..=)
              Prelude.<$> instanceConfigs,
            ("VolumeSizeInGB" Core..=)
              Prelude.<$> volumeSizeInGB
          ]
      )
