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
-- Module      : Network.AWS.SageMaker.Types.ProcessingClusterConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingClusterConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.ProcessingInstanceType

-- | Configuration for the cluster used to run a processing job.
--
-- /See:/ 'newProcessingClusterConfig' smart constructor.
data ProcessingClusterConfig = ProcessingClusterConfig'
  { -- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
    -- to encrypt data on the storage volume attached to the ML compute
    -- instance(s) that run the processing job.
    volumeKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The number of ML compute instances to use in the processing job. For
    -- distributed processing jobs, specify a value greater than 1. The default
    -- value is 1.
    instanceCount :: Prelude.Natural,
    -- | The ML compute instance type for the processing job.
    instanceType :: ProcessingInstanceType,
    -- | The size of the ML storage volume in gigabytes that you want to
    -- provision. You must specify sufficient ML storage for your scenario.
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
-- 'volumeKmsKeyId', 'processingClusterConfig_volumeKmsKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
-- to encrypt data on the storage volume attached to the ML compute
-- instance(s) that run the processing job.
--
-- 'instanceCount', 'processingClusterConfig_instanceCount' - The number of ML compute instances to use in the processing job. For
-- distributed processing jobs, specify a value greater than 1. The default
-- value is 1.
--
-- 'instanceType', 'processingClusterConfig_instanceType' - The ML compute instance type for the processing job.
--
-- 'volumeSizeInGB', 'processingClusterConfig_volumeSizeInGB' - The size of the ML storage volume in gigabytes that you want to
-- provision. You must specify sufficient ML storage for your scenario.
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

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
-- to encrypt data on the storage volume attached to the ML compute
-- instance(s) that run the processing job.
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

instance Prelude.Hashable ProcessingClusterConfig

instance Prelude.NFData ProcessingClusterConfig

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
