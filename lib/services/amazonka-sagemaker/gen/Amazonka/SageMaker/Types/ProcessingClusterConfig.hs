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
-- Module      : Amazonka.SageMaker.Types.ProcessingClusterConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProcessingClusterConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ProcessingInstanceType

-- | Configuration for the cluster used to run a processing job.
--
-- /See:/ 'newProcessingClusterConfig' smart constructor.
data ProcessingClusterConfig = ProcessingClusterConfig'
  { -- | The Amazon Web Services Key Management Service (Amazon Web Services KMS)
    -- key that Amazon SageMaker uses to encrypt data on the storage volume
    -- attached to the ML compute instance(s) that run the processing job.
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
    volumeKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The number of ML compute instances to use in the processing job. For
    -- distributed processing jobs, specify a value greater than 1. The default
    -- value is 1.
    instanceCount :: Prelude.Natural,
    -- | The ML compute instance type for the processing job.
    instanceType :: ProcessingInstanceType,
    -- | The size of the ML storage volume in gigabytes that you want to
    -- provision. You must specify sufficient ML storage for your scenario.
    --
    -- Certain Nitro-based instances include local storage with a fixed total
    -- size, dependent on the instance type. When using these instances for
    -- processing, Amazon SageMaker mounts the local instance storage instead
    -- of Amazon EBS gp2 storage. You can\'t request a @VolumeSizeInGB@ greater
    -- than the total size of the local instance storage.
    --
    -- For a list of instance types that support local instance storage,
    -- including the total size per instance type, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html#instance-store-volumes Instance Store Volumes>.
    volumeSizeInGB :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProcessingClusterConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeKmsKeyId', 'processingClusterConfig_volumeKmsKeyId' - The Amazon Web Services Key Management Service (Amazon Web Services KMS)
-- key that Amazon SageMaker uses to encrypt data on the storage volume
-- attached to the ML compute instance(s) that run the processing job.
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
-- 'instanceCount', 'processingClusterConfig_instanceCount' - The number of ML compute instances to use in the processing job. For
-- distributed processing jobs, specify a value greater than 1. The default
-- value is 1.
--
-- 'instanceType', 'processingClusterConfig_instanceType' - The ML compute instance type for the processing job.
--
-- 'volumeSizeInGB', 'processingClusterConfig_volumeSizeInGB' - The size of the ML storage volume in gigabytes that you want to
-- provision. You must specify sufficient ML storage for your scenario.
--
-- Certain Nitro-based instances include local storage with a fixed total
-- size, dependent on the instance type. When using these instances for
-- processing, Amazon SageMaker mounts the local instance storage instead
-- of Amazon EBS gp2 storage. You can\'t request a @VolumeSizeInGB@ greater
-- than the total size of the local instance storage.
--
-- For a list of instance types that support local instance storage,
-- including the total size per instance type, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html#instance-store-volumes Instance Store Volumes>.
newProcessingClusterConfig ::
  -- | 'instanceCount'
  Prelude.Natural ->
  -- | 'instanceType'
  ProcessingInstanceType ->
  -- | 'volumeSizeInGB'
  Prelude.Natural ->
  ProcessingClusterConfig
newProcessingClusterConfig
  pInstanceCount_
  pInstanceType_
  pVolumeSizeInGB_ =
    ProcessingClusterConfig'
      { volumeKmsKeyId =
          Prelude.Nothing,
        instanceCount = pInstanceCount_,
        instanceType = pInstanceType_,
        volumeSizeInGB = pVolumeSizeInGB_
      }

-- | The Amazon Web Services Key Management Service (Amazon Web Services KMS)
-- key that Amazon SageMaker uses to encrypt data on the storage volume
-- attached to the ML compute instance(s) that run the processing job.
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
processingClusterConfig_volumeKmsKeyId :: Lens.Lens' ProcessingClusterConfig (Prelude.Maybe Prelude.Text)
processingClusterConfig_volumeKmsKeyId = Lens.lens (\ProcessingClusterConfig' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@ProcessingClusterConfig' {} a -> s {volumeKmsKeyId = a} :: ProcessingClusterConfig)

-- | The number of ML compute instances to use in the processing job. For
-- distributed processing jobs, specify a value greater than 1. The default
-- value is 1.
processingClusterConfig_instanceCount :: Lens.Lens' ProcessingClusterConfig Prelude.Natural
processingClusterConfig_instanceCount = Lens.lens (\ProcessingClusterConfig' {instanceCount} -> instanceCount) (\s@ProcessingClusterConfig' {} a -> s {instanceCount = a} :: ProcessingClusterConfig)

-- | The ML compute instance type for the processing job.
processingClusterConfig_instanceType :: Lens.Lens' ProcessingClusterConfig ProcessingInstanceType
processingClusterConfig_instanceType = Lens.lens (\ProcessingClusterConfig' {instanceType} -> instanceType) (\s@ProcessingClusterConfig' {} a -> s {instanceType = a} :: ProcessingClusterConfig)

-- | The size of the ML storage volume in gigabytes that you want to
-- provision. You must specify sufficient ML storage for your scenario.
--
-- Certain Nitro-based instances include local storage with a fixed total
-- size, dependent on the instance type. When using these instances for
-- processing, Amazon SageMaker mounts the local instance storage instead
-- of Amazon EBS gp2 storage. You can\'t request a @VolumeSizeInGB@ greater
-- than the total size of the local instance storage.
--
-- For a list of instance types that support local instance storage,
-- including the total size per instance type, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html#instance-store-volumes Instance Store Volumes>.
processingClusterConfig_volumeSizeInGB :: Lens.Lens' ProcessingClusterConfig Prelude.Natural
processingClusterConfig_volumeSizeInGB = Lens.lens (\ProcessingClusterConfig' {volumeSizeInGB} -> volumeSizeInGB) (\s@ProcessingClusterConfig' {} a -> s {volumeSizeInGB = a} :: ProcessingClusterConfig)

instance Core.FromJSON ProcessingClusterConfig where
  parseJSON =
    Core.withObject
      "ProcessingClusterConfig"
      ( \x ->
          ProcessingClusterConfig'
            Prelude.<$> (x Core..:? "VolumeKmsKeyId")
            Prelude.<*> (x Core..: "InstanceCount")
            Prelude.<*> (x Core..: "InstanceType")
            Prelude.<*> (x Core..: "VolumeSizeInGB")
      )

instance Prelude.Hashable ProcessingClusterConfig where
  hashWithSalt _salt ProcessingClusterConfig' {..} =
    _salt `Prelude.hashWithSalt` volumeKmsKeyId
      `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` volumeSizeInGB

instance Prelude.NFData ProcessingClusterConfig where
  rnf ProcessingClusterConfig' {..} =
    Prelude.rnf volumeKmsKeyId
      `Prelude.seq` Prelude.rnf instanceCount
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf volumeSizeInGB

instance Core.ToJSON ProcessingClusterConfig where
  toJSON ProcessingClusterConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VolumeKmsKeyId" Core..=)
              Prelude.<$> volumeKmsKeyId,
            Prelude.Just ("InstanceCount" Core..= instanceCount),
            Prelude.Just ("InstanceType" Core..= instanceType),
            Prelude.Just
              ("VolumeSizeInGB" Core..= volumeSizeInGB)
          ]
      )
