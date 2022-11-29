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
-- Module      : Amazonka.SageMaker.Types.ResourceConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ResourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.InstanceGroup
import Amazonka.SageMaker.Types.TrainingInstanceType

-- | Describes the resources, including ML compute instances and ML storage
-- volumes, to use for model training.
--
-- /See:/ 'newResourceConfig' smart constructor.
data ResourceConfig = ResourceConfig'
  { -- | The duration of time in seconds to retain configured resources in a warm
    -- pool for subsequent training jobs.
    keepAlivePeriodInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Web Services KMS key that SageMaker uses to encrypt data on
    -- the storage volume attached to the ML compute instance(s) that run the
    -- training job.
    --
    -- Certain Nitro-based instances include local storage, dependent on the
    -- instance type. Local storage volumes are encrypted using a hardware
    -- module on the instance. You can\'t request a @VolumeKmsKeyId@ when using
    -- an instance type with local storage.
    --
    -- For a list of instance types that support local instance storage, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html#instance-store-volumes Instance Store Volumes>.
    --
    -- For more information about local instance storage encryption, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ssd-instance-store.html SSD Instance Store Volumes>.
    --
    -- The @VolumeKmsKeyId@ can be in any of the following formats:
    --
    -- -   \/\/ KMS Key ID
    --
    --     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   \/\/ Amazon Resource Name (ARN) of a KMS Key
    --
    --     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
    volumeKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The ML compute instance type.
    instanceType :: Prelude.Maybe TrainingInstanceType,
    -- | The number of ML compute instances to use. For distributed training,
    -- provide a value greater than 1.
    instanceCount :: Prelude.Maybe Prelude.Natural,
    -- | The configuration of a heterogeneous cluster in JSON format.
    instanceGroups :: Prelude.Maybe [InstanceGroup],
    -- | The size of the ML storage volume that you want to provision.
    --
    -- ML storage volumes store model artifacts and incremental states.
    -- Training algorithms might also use the ML storage volume for scratch
    -- space. If you want to store the training data in the ML storage volume,
    -- choose @File@ as the @TrainingInputMode@ in the algorithm specification.
    --
    -- When using an ML instance with
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ssd-instance-store.html#nvme-ssd-volumes NVMe SSD volumes>,
    -- SageMaker doesn\'t provision Amazon EBS General Purpose SSD (gp2)
    -- storage. Available storage is fixed to the NVMe-type instance\'s storage
    -- capacity. SageMaker configures storage paths for training datasets,
    -- checkpoints, model artifacts, and outputs to use the entire capacity of
    -- the instance storage. For example, ML instance families with the
    -- NVMe-type instance storage include @ml.p4d@, @ml.g4dn@, and @ml.g5@.
    --
    -- When using an ML instance with the EBS-only storage option and without
    -- instance storage, you must define the size of EBS volume through
    -- @VolumeSizeInGB@ in the @ResourceConfig@ API. For example, ML instance
    -- families that use EBS volumes include @ml.c5@ and @ml.p2@.
    --
    -- To look up instance types and their instance storage types and volumes,
    -- see
    -- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>.
    --
    -- To find the default local paths defined by the SageMaker training
    -- platform, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-train-storage.html Amazon SageMaker Training Storage Folders for Training Datasets, Checkpoints, Model Artifacts, and Outputs>.
    volumeSizeInGB :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keepAlivePeriodInSeconds', 'resourceConfig_keepAlivePeriodInSeconds' - The duration of time in seconds to retain configured resources in a warm
-- pool for subsequent training jobs.
--
-- 'volumeKmsKeyId', 'resourceConfig_volumeKmsKeyId' - The Amazon Web Services KMS key that SageMaker uses to encrypt data on
-- the storage volume attached to the ML compute instance(s) that run the
-- training job.
--
-- Certain Nitro-based instances include local storage, dependent on the
-- instance type. Local storage volumes are encrypted using a hardware
-- module on the instance. You can\'t request a @VolumeKmsKeyId@ when using
-- an instance type with local storage.
--
-- For a list of instance types that support local instance storage, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html#instance-store-volumes Instance Store Volumes>.
--
-- For more information about local instance storage encryption, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ssd-instance-store.html SSD Instance Store Volumes>.
--
-- The @VolumeKmsKeyId@ can be in any of the following formats:
--
-- -   \/\/ KMS Key ID
--
--     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   \/\/ Amazon Resource Name (ARN) of a KMS Key
--
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'instanceType', 'resourceConfig_instanceType' - The ML compute instance type.
--
-- 'instanceCount', 'resourceConfig_instanceCount' - The number of ML compute instances to use. For distributed training,
-- provide a value greater than 1.
--
-- 'instanceGroups', 'resourceConfig_instanceGroups' - The configuration of a heterogeneous cluster in JSON format.
--
-- 'volumeSizeInGB', 'resourceConfig_volumeSizeInGB' - The size of the ML storage volume that you want to provision.
--
-- ML storage volumes store model artifacts and incremental states.
-- Training algorithms might also use the ML storage volume for scratch
-- space. If you want to store the training data in the ML storage volume,
-- choose @File@ as the @TrainingInputMode@ in the algorithm specification.
--
-- When using an ML instance with
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ssd-instance-store.html#nvme-ssd-volumes NVMe SSD volumes>,
-- SageMaker doesn\'t provision Amazon EBS General Purpose SSD (gp2)
-- storage. Available storage is fixed to the NVMe-type instance\'s storage
-- capacity. SageMaker configures storage paths for training datasets,
-- checkpoints, model artifacts, and outputs to use the entire capacity of
-- the instance storage. For example, ML instance families with the
-- NVMe-type instance storage include @ml.p4d@, @ml.g4dn@, and @ml.g5@.
--
-- When using an ML instance with the EBS-only storage option and without
-- instance storage, you must define the size of EBS volume through
-- @VolumeSizeInGB@ in the @ResourceConfig@ API. For example, ML instance
-- families that use EBS volumes include @ml.c5@ and @ml.p2@.
--
-- To look up instance types and their instance storage types and volumes,
-- see
-- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>.
--
-- To find the default local paths defined by the SageMaker training
-- platform, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-train-storage.html Amazon SageMaker Training Storage Folders for Training Datasets, Checkpoints, Model Artifacts, and Outputs>.
newResourceConfig ::
  -- | 'volumeSizeInGB'
  Prelude.Natural ->
  ResourceConfig
newResourceConfig pVolumeSizeInGB_ =
  ResourceConfig'
    { keepAlivePeriodInSeconds =
        Prelude.Nothing,
      volumeKmsKeyId = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      instanceCount = Prelude.Nothing,
      instanceGroups = Prelude.Nothing,
      volumeSizeInGB = pVolumeSizeInGB_
    }

-- | The duration of time in seconds to retain configured resources in a warm
-- pool for subsequent training jobs.
resourceConfig_keepAlivePeriodInSeconds :: Lens.Lens' ResourceConfig (Prelude.Maybe Prelude.Natural)
resourceConfig_keepAlivePeriodInSeconds = Lens.lens (\ResourceConfig' {keepAlivePeriodInSeconds} -> keepAlivePeriodInSeconds) (\s@ResourceConfig' {} a -> s {keepAlivePeriodInSeconds = a} :: ResourceConfig)

-- | The Amazon Web Services KMS key that SageMaker uses to encrypt data on
-- the storage volume attached to the ML compute instance(s) that run the
-- training job.
--
-- Certain Nitro-based instances include local storage, dependent on the
-- instance type. Local storage volumes are encrypted using a hardware
-- module on the instance. You can\'t request a @VolumeKmsKeyId@ when using
-- an instance type with local storage.
--
-- For a list of instance types that support local instance storage, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html#instance-store-volumes Instance Store Volumes>.
--
-- For more information about local instance storage encryption, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ssd-instance-store.html SSD Instance Store Volumes>.
--
-- The @VolumeKmsKeyId@ can be in any of the following formats:
--
-- -   \/\/ KMS Key ID
--
--     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   \/\/ Amazon Resource Name (ARN) of a KMS Key
--
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
resourceConfig_volumeKmsKeyId :: Lens.Lens' ResourceConfig (Prelude.Maybe Prelude.Text)
resourceConfig_volumeKmsKeyId = Lens.lens (\ResourceConfig' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@ResourceConfig' {} a -> s {volumeKmsKeyId = a} :: ResourceConfig)

-- | The ML compute instance type.
resourceConfig_instanceType :: Lens.Lens' ResourceConfig (Prelude.Maybe TrainingInstanceType)
resourceConfig_instanceType = Lens.lens (\ResourceConfig' {instanceType} -> instanceType) (\s@ResourceConfig' {} a -> s {instanceType = a} :: ResourceConfig)

-- | The number of ML compute instances to use. For distributed training,
-- provide a value greater than 1.
resourceConfig_instanceCount :: Lens.Lens' ResourceConfig (Prelude.Maybe Prelude.Natural)
resourceConfig_instanceCount = Lens.lens (\ResourceConfig' {instanceCount} -> instanceCount) (\s@ResourceConfig' {} a -> s {instanceCount = a} :: ResourceConfig)

-- | The configuration of a heterogeneous cluster in JSON format.
resourceConfig_instanceGroups :: Lens.Lens' ResourceConfig (Prelude.Maybe [InstanceGroup])
resourceConfig_instanceGroups = Lens.lens (\ResourceConfig' {instanceGroups} -> instanceGroups) (\s@ResourceConfig' {} a -> s {instanceGroups = a} :: ResourceConfig) Prelude.. Lens.mapping Lens.coerced

-- | The size of the ML storage volume that you want to provision.
--
-- ML storage volumes store model artifacts and incremental states.
-- Training algorithms might also use the ML storage volume for scratch
-- space. If you want to store the training data in the ML storage volume,
-- choose @File@ as the @TrainingInputMode@ in the algorithm specification.
--
-- When using an ML instance with
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ssd-instance-store.html#nvme-ssd-volumes NVMe SSD volumes>,
-- SageMaker doesn\'t provision Amazon EBS General Purpose SSD (gp2)
-- storage. Available storage is fixed to the NVMe-type instance\'s storage
-- capacity. SageMaker configures storage paths for training datasets,
-- checkpoints, model artifacts, and outputs to use the entire capacity of
-- the instance storage. For example, ML instance families with the
-- NVMe-type instance storage include @ml.p4d@, @ml.g4dn@, and @ml.g5@.
--
-- When using an ML instance with the EBS-only storage option and without
-- instance storage, you must define the size of EBS volume through
-- @VolumeSizeInGB@ in the @ResourceConfig@ API. For example, ML instance
-- families that use EBS volumes include @ml.c5@ and @ml.p2@.
--
-- To look up instance types and their instance storage types and volumes,
-- see
-- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>.
--
-- To find the default local paths defined by the SageMaker training
-- platform, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-train-storage.html Amazon SageMaker Training Storage Folders for Training Datasets, Checkpoints, Model Artifacts, and Outputs>.
resourceConfig_volumeSizeInGB :: Lens.Lens' ResourceConfig Prelude.Natural
resourceConfig_volumeSizeInGB = Lens.lens (\ResourceConfig' {volumeSizeInGB} -> volumeSizeInGB) (\s@ResourceConfig' {} a -> s {volumeSizeInGB = a} :: ResourceConfig)

instance Core.FromJSON ResourceConfig where
  parseJSON =
    Core.withObject
      "ResourceConfig"
      ( \x ->
          ResourceConfig'
            Prelude.<$> (x Core..:? "KeepAlivePeriodInSeconds")
            Prelude.<*> (x Core..:? "VolumeKmsKeyId")
            Prelude.<*> (x Core..:? "InstanceType")
            Prelude.<*> (x Core..:? "InstanceCount")
            Prelude.<*> (x Core..:? "InstanceGroups" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "VolumeSizeInGB")
      )

instance Prelude.Hashable ResourceConfig where
  hashWithSalt _salt ResourceConfig' {..} =
    _salt
      `Prelude.hashWithSalt` keepAlivePeriodInSeconds
      `Prelude.hashWithSalt` volumeKmsKeyId
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` instanceGroups
      `Prelude.hashWithSalt` volumeSizeInGB

instance Prelude.NFData ResourceConfig where
  rnf ResourceConfig' {..} =
    Prelude.rnf keepAlivePeriodInSeconds
      `Prelude.seq` Prelude.rnf volumeKmsKeyId
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf instanceCount
      `Prelude.seq` Prelude.rnf instanceGroups
      `Prelude.seq` Prelude.rnf volumeSizeInGB

instance Core.ToJSON ResourceConfig where
  toJSON ResourceConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KeepAlivePeriodInSeconds" Core..=)
              Prelude.<$> keepAlivePeriodInSeconds,
            ("VolumeKmsKeyId" Core..=)
              Prelude.<$> volumeKmsKeyId,
            ("InstanceType" Core..=) Prelude.<$> instanceType,
            ("InstanceCount" Core..=) Prelude.<$> instanceCount,
            ("InstanceGroups" Core..=)
              Prelude.<$> instanceGroups,
            Prelude.Just
              ("VolumeSizeInGB" Core..= volumeSizeInGB)
          ]
      )
