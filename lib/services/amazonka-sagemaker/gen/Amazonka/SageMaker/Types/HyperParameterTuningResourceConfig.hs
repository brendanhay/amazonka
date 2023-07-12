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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HyperParameterTuningResourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.HyperParameterTuningAllocationStrategy
import Amazonka.SageMaker.Types.HyperParameterTuningInstanceConfig
import Amazonka.SageMaker.Types.TrainingInstanceType

-- | The configuration of resources, including compute instances and storage
-- volumes for use in training jobs launched by hyperparameter tuning jobs.
-- @HyperParameterTuningResourceConfig@ is similar to @ResourceConfig@, but
-- has the additional @InstanceConfigs@ and @AllocationStrategy@ fields to
-- allow for flexible instance management. Specify one or more instance
-- types, count, and the allocation strategy for instance selection.
--
-- @HyperParameterTuningResourceConfig@ supports the capabilities of
-- @ResourceConfig@ with the exception of @KeepAlivePeriodInSeconds@.
-- Hyperparameter tuning jobs use warm pools by default, which reuse
-- clusters between training jobs.
--
-- /See:/ 'newHyperParameterTuningResourceConfig' smart constructor.
data HyperParameterTuningResourceConfig = HyperParameterTuningResourceConfig'
  { -- | The strategy that determines the order of preference for resources
    -- specified in @InstanceConfigs@ used in hyperparameter optimization.
    allocationStrategy :: Prelude.Maybe HyperParameterTuningAllocationStrategy,
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
    -- | The number of compute instances of type @InstanceType@ to use. For
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/data-parallel-use-api.html distributed training>,
    -- select a value greater than 1.
    instanceCount :: Prelude.Maybe Prelude.Natural,
    -- | The instance type used to run hyperparameter optimization tuning jobs.
    -- See
    -- <https://docs.aws.amazon.com/notebooks-available-instance-types.html descriptions of instance types>
    -- for more information.
    instanceType :: Prelude.Maybe TrainingInstanceType,
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
    volumeKmsKeyId :: Prelude.Maybe Prelude.Text,
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
-- 'allocationStrategy', 'hyperParameterTuningResourceConfig_allocationStrategy' - The strategy that determines the order of preference for resources
-- specified in @InstanceConfigs@ used in hyperparameter optimization.
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
-- 'instanceCount', 'hyperParameterTuningResourceConfig_instanceCount' - The number of compute instances of type @InstanceType@ to use. For
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/data-parallel-use-api.html distributed training>,
-- select a value greater than 1.
--
-- 'instanceType', 'hyperParameterTuningResourceConfig_instanceType' - The instance type used to run hyperparameter optimization tuning jobs.
-- See
-- <https://docs.aws.amazon.com/notebooks-available-instance-types.html descriptions of instance types>
-- for more information.
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
    { allocationStrategy =
        Prelude.Nothing,
      instanceConfigs = Prelude.Nothing,
      instanceCount = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      volumeKmsKeyId = Prelude.Nothing,
      volumeSizeInGB = Prelude.Nothing
    }

-- | The strategy that determines the order of preference for resources
-- specified in @InstanceConfigs@ used in hyperparameter optimization.
hyperParameterTuningResourceConfig_allocationStrategy :: Lens.Lens' HyperParameterTuningResourceConfig (Prelude.Maybe HyperParameterTuningAllocationStrategy)
hyperParameterTuningResourceConfig_allocationStrategy = Lens.lens (\HyperParameterTuningResourceConfig' {allocationStrategy} -> allocationStrategy) (\s@HyperParameterTuningResourceConfig' {} a -> s {allocationStrategy = a} :: HyperParameterTuningResourceConfig)

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

-- | The number of compute instances of type @InstanceType@ to use. For
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/data-parallel-use-api.html distributed training>,
-- select a value greater than 1.
hyperParameterTuningResourceConfig_instanceCount :: Lens.Lens' HyperParameterTuningResourceConfig (Prelude.Maybe Prelude.Natural)
hyperParameterTuningResourceConfig_instanceCount = Lens.lens (\HyperParameterTuningResourceConfig' {instanceCount} -> instanceCount) (\s@HyperParameterTuningResourceConfig' {} a -> s {instanceCount = a} :: HyperParameterTuningResourceConfig)

-- | The instance type used to run hyperparameter optimization tuning jobs.
-- See
-- <https://docs.aws.amazon.com/notebooks-available-instance-types.html descriptions of instance types>
-- for more information.
hyperParameterTuningResourceConfig_instanceType :: Lens.Lens' HyperParameterTuningResourceConfig (Prelude.Maybe TrainingInstanceType)
hyperParameterTuningResourceConfig_instanceType = Lens.lens (\HyperParameterTuningResourceConfig' {instanceType} -> instanceType) (\s@HyperParameterTuningResourceConfig' {} a -> s {instanceType = a} :: HyperParameterTuningResourceConfig)

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
  Data.FromJSON
    HyperParameterTuningResourceConfig
  where
  parseJSON =
    Data.withObject
      "HyperParameterTuningResourceConfig"
      ( \x ->
          HyperParameterTuningResourceConfig'
            Prelude.<$> (x Data..:? "AllocationStrategy")
            Prelude.<*> (x Data..:? "InstanceConfigs")
            Prelude.<*> (x Data..:? "InstanceCount")
            Prelude.<*> (x Data..:? "InstanceType")
            Prelude.<*> (x Data..:? "VolumeKmsKeyId")
            Prelude.<*> (x Data..:? "VolumeSizeInGB")
      )

instance
  Prelude.Hashable
    HyperParameterTuningResourceConfig
  where
  hashWithSalt
    _salt
    HyperParameterTuningResourceConfig' {..} =
      _salt
        `Prelude.hashWithSalt` allocationStrategy
        `Prelude.hashWithSalt` instanceConfigs
        `Prelude.hashWithSalt` instanceCount
        `Prelude.hashWithSalt` instanceType
        `Prelude.hashWithSalt` volumeKmsKeyId
        `Prelude.hashWithSalt` volumeSizeInGB

instance
  Prelude.NFData
    HyperParameterTuningResourceConfig
  where
  rnf HyperParameterTuningResourceConfig' {..} =
    Prelude.rnf allocationStrategy
      `Prelude.seq` Prelude.rnf instanceConfigs
      `Prelude.seq` Prelude.rnf instanceCount
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf volumeKmsKeyId
      `Prelude.seq` Prelude.rnf volumeSizeInGB

instance
  Data.ToJSON
    HyperParameterTuningResourceConfig
  where
  toJSON HyperParameterTuningResourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllocationStrategy" Data..=)
              Prelude.<$> allocationStrategy,
            ("InstanceConfigs" Data..=)
              Prelude.<$> instanceConfigs,
            ("InstanceCount" Data..=) Prelude.<$> instanceCount,
            ("InstanceType" Data..=) Prelude.<$> instanceType,
            ("VolumeKmsKeyId" Data..=)
              Prelude.<$> volumeKmsKeyId,
            ("VolumeSizeInGB" Data..=)
              Prelude.<$> volumeSizeInGB
          ]
      )
