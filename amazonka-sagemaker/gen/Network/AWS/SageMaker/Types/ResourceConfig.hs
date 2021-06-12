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
-- Module      : Network.AWS.SageMaker.Types.ResourceConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ResourceConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.TrainingInstanceType

-- | Describes the resources, including ML compute instances and ML storage
-- volumes, to use for model training.
--
-- /See:/ 'newResourceConfig' smart constructor.
data ResourceConfig = ResourceConfig'
  { -- | The AWS KMS key that Amazon SageMaker uses to encrypt data on the
    -- storage volume attached to the ML compute instance(s) that run the
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
    volumeKmsKeyId :: Core.Maybe Core.Text,
    -- | The ML compute instance type.
    instanceType :: TrainingInstanceType,
    -- | The number of ML compute instances to use. For distributed training,
    -- provide a value greater than 1.
    instanceCount :: Core.Natural,
    -- | The size of the ML storage volume that you want to provision.
    --
    -- ML storage volumes store model artifacts and incremental states.
    -- Training algorithms might also use the ML storage volume for scratch
    -- space. If you want to store the training data in the ML storage volume,
    -- choose @File@ as the @TrainingInputMode@ in the algorithm specification.
    --
    -- You must specify sufficient ML storage for your scenario.
    --
    -- Amazon SageMaker supports only the General Purpose SSD (gp2) ML storage
    -- volume type.
    --
    -- Certain Nitro-based instances include local storage with a fixed total
    -- size, dependent on the instance type. When using these instances for
    -- training, Amazon SageMaker mounts the local instance storage instead of
    -- Amazon EBS gp2 storage. You can\'t request a @VolumeSizeInGB@ greater
    -- than the total size of the local instance storage.
    --
    -- For a list of instance types that support local instance storage,
    -- including the total size per instance type, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html#instance-store-volumes Instance Store Volumes>.
    volumeSizeInGB :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeKmsKeyId', 'resourceConfig_volumeKmsKeyId' - The AWS KMS key that Amazon SageMaker uses to encrypt data on the
-- storage volume attached to the ML compute instance(s) that run the
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
-- 'volumeSizeInGB', 'resourceConfig_volumeSizeInGB' - The size of the ML storage volume that you want to provision.
--
-- ML storage volumes store model artifacts and incremental states.
-- Training algorithms might also use the ML storage volume for scratch
-- space. If you want to store the training data in the ML storage volume,
-- choose @File@ as the @TrainingInputMode@ in the algorithm specification.
--
-- You must specify sufficient ML storage for your scenario.
--
-- Amazon SageMaker supports only the General Purpose SSD (gp2) ML storage
-- volume type.
--
-- Certain Nitro-based instances include local storage with a fixed total
-- size, dependent on the instance type. When using these instances for
-- training, Amazon SageMaker mounts the local instance storage instead of
-- Amazon EBS gp2 storage. You can\'t request a @VolumeSizeInGB@ greater
-- than the total size of the local instance storage.
--
-- For a list of instance types that support local instance storage,
-- including the total size per instance type, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html#instance-store-volumes Instance Store Volumes>.
newResourceConfig ::
  -- | 'instanceType'
  TrainingInstanceType ->
  -- | 'instanceCount'
  Core.Natural ->
  -- | 'volumeSizeInGB'
  Core.Natural ->
  ResourceConfig
newResourceConfig
  pInstanceType_
  pInstanceCount_
  pVolumeSizeInGB_ =
    ResourceConfig'
      { volumeKmsKeyId = Core.Nothing,
        instanceType = pInstanceType_,
        instanceCount = pInstanceCount_,
        volumeSizeInGB = pVolumeSizeInGB_
      }

-- | The AWS KMS key that Amazon SageMaker uses to encrypt data on the
-- storage volume attached to the ML compute instance(s) that run the
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
resourceConfig_volumeKmsKeyId :: Lens.Lens' ResourceConfig (Core.Maybe Core.Text)
resourceConfig_volumeKmsKeyId = Lens.lens (\ResourceConfig' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@ResourceConfig' {} a -> s {volumeKmsKeyId = a} :: ResourceConfig)

-- | The ML compute instance type.
resourceConfig_instanceType :: Lens.Lens' ResourceConfig TrainingInstanceType
resourceConfig_instanceType = Lens.lens (\ResourceConfig' {instanceType} -> instanceType) (\s@ResourceConfig' {} a -> s {instanceType = a} :: ResourceConfig)

-- | The number of ML compute instances to use. For distributed training,
-- provide a value greater than 1.
resourceConfig_instanceCount :: Lens.Lens' ResourceConfig Core.Natural
resourceConfig_instanceCount = Lens.lens (\ResourceConfig' {instanceCount} -> instanceCount) (\s@ResourceConfig' {} a -> s {instanceCount = a} :: ResourceConfig)

-- | The size of the ML storage volume that you want to provision.
--
-- ML storage volumes store model artifacts and incremental states.
-- Training algorithms might also use the ML storage volume for scratch
-- space. If you want to store the training data in the ML storage volume,
-- choose @File@ as the @TrainingInputMode@ in the algorithm specification.
--
-- You must specify sufficient ML storage for your scenario.
--
-- Amazon SageMaker supports only the General Purpose SSD (gp2) ML storage
-- volume type.
--
-- Certain Nitro-based instances include local storage with a fixed total
-- size, dependent on the instance type. When using these instances for
-- training, Amazon SageMaker mounts the local instance storage instead of
-- Amazon EBS gp2 storage. You can\'t request a @VolumeSizeInGB@ greater
-- than the total size of the local instance storage.
--
-- For a list of instance types that support local instance storage,
-- including the total size per instance type, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html#instance-store-volumes Instance Store Volumes>.
resourceConfig_volumeSizeInGB :: Lens.Lens' ResourceConfig Core.Natural
resourceConfig_volumeSizeInGB = Lens.lens (\ResourceConfig' {volumeSizeInGB} -> volumeSizeInGB) (\s@ResourceConfig' {} a -> s {volumeSizeInGB = a} :: ResourceConfig)

instance Core.FromJSON ResourceConfig where
  parseJSON =
    Core.withObject
      "ResourceConfig"
      ( \x ->
          ResourceConfig'
            Core.<$> (x Core..:? "VolumeKmsKeyId")
            Core.<*> (x Core..: "InstanceType")
            Core.<*> (x Core..: "InstanceCount")
            Core.<*> (x Core..: "VolumeSizeInGB")
      )

instance Core.Hashable ResourceConfig

instance Core.NFData ResourceConfig

instance Core.ToJSON ResourceConfig where
  toJSON ResourceConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("VolumeKmsKeyId" Core..=) Core.<$> volumeKmsKeyId,
            Core.Just ("InstanceType" Core..= instanceType),
            Core.Just ("InstanceCount" Core..= instanceCount),
            Core.Just ("VolumeSizeInGB" Core..= volumeSizeInGB)
          ]
      )
