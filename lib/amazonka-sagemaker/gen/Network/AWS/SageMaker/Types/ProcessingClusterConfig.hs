-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingClusterConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingClusterConfig
  ( ProcessingClusterConfig (..),

    -- * Smart constructor
    mkProcessingClusterConfig,

    -- * Lenses
    pccVolumeKMSKeyId,
    pccInstanceCount,
    pccInstanceType,
    pccVolumeSizeInGB,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ProcessingInstanceType

-- | Configuration for the cluster used to run a processing job.
--
-- /See:/ 'mkProcessingClusterConfig' smart constructor.
data ProcessingClusterConfig = ProcessingClusterConfig'
  { volumeKMSKeyId ::
      Lude.Maybe Lude.Text,
    instanceCount :: Lude.Natural,
    instanceType :: ProcessingInstanceType,
    volumeSizeInGB :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProcessingClusterConfig' with the minimum fields required to make a request.
--
-- * 'instanceCount' - The number of ML compute instances to use in the processing job. For distributed processing jobs, specify a value greater than 1. The default value is 1.
-- * 'instanceType' - The ML compute instance type for the processing job.
-- * 'volumeKMSKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the processing job.
-- * 'volumeSizeInGB' - The size of the ML storage volume in gigabytes that you want to provision. You must specify sufficient ML storage for your scenario.
mkProcessingClusterConfig ::
  -- | 'instanceCount'
  Lude.Natural ->
  -- | 'instanceType'
  ProcessingInstanceType ->
  -- | 'volumeSizeInGB'
  Lude.Natural ->
  ProcessingClusterConfig
mkProcessingClusterConfig
  pInstanceCount_
  pInstanceType_
  pVolumeSizeInGB_ =
    ProcessingClusterConfig'
      { volumeKMSKeyId = Lude.Nothing,
        instanceCount = pInstanceCount_,
        instanceType = pInstanceType_,
        volumeSizeInGB = pVolumeSizeInGB_
      }

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the processing job.
--
-- /Note:/ Consider using 'volumeKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccVolumeKMSKeyId :: Lens.Lens' ProcessingClusterConfig (Lude.Maybe Lude.Text)
pccVolumeKMSKeyId = Lens.lens (volumeKMSKeyId :: ProcessingClusterConfig -> Lude.Maybe Lude.Text) (\s a -> s {volumeKMSKeyId = a} :: ProcessingClusterConfig)
{-# DEPRECATED pccVolumeKMSKeyId "Use generic-lens or generic-optics with 'volumeKMSKeyId' instead." #-}

-- | The number of ML compute instances to use in the processing job. For distributed processing jobs, specify a value greater than 1. The default value is 1.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccInstanceCount :: Lens.Lens' ProcessingClusterConfig Lude.Natural
pccInstanceCount = Lens.lens (instanceCount :: ProcessingClusterConfig -> Lude.Natural) (\s a -> s {instanceCount = a} :: ProcessingClusterConfig)
{-# DEPRECATED pccInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The ML compute instance type for the processing job.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccInstanceType :: Lens.Lens' ProcessingClusterConfig ProcessingInstanceType
pccInstanceType = Lens.lens (instanceType :: ProcessingClusterConfig -> ProcessingInstanceType) (\s a -> s {instanceType = a} :: ProcessingClusterConfig)
{-# DEPRECATED pccInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The size of the ML storage volume in gigabytes that you want to provision. You must specify sufficient ML storage for your scenario.
--
-- /Note:/ Consider using 'volumeSizeInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccVolumeSizeInGB :: Lens.Lens' ProcessingClusterConfig Lude.Natural
pccVolumeSizeInGB = Lens.lens (volumeSizeInGB :: ProcessingClusterConfig -> Lude.Natural) (\s a -> s {volumeSizeInGB = a} :: ProcessingClusterConfig)
{-# DEPRECATED pccVolumeSizeInGB "Use generic-lens or generic-optics with 'volumeSizeInGB' instead." #-}

instance Lude.FromJSON ProcessingClusterConfig where
  parseJSON =
    Lude.withObject
      "ProcessingClusterConfig"
      ( \x ->
          ProcessingClusterConfig'
            Lude.<$> (x Lude..:? "VolumeKmsKeyId")
            Lude.<*> (x Lude..: "InstanceCount")
            Lude.<*> (x Lude..: "InstanceType")
            Lude.<*> (x Lude..: "VolumeSizeInGB")
      )

instance Lude.ToJSON ProcessingClusterConfig where
  toJSON ProcessingClusterConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("VolumeKmsKeyId" Lude..=) Lude.<$> volumeKMSKeyId,
            Lude.Just ("InstanceCount" Lude..= instanceCount),
            Lude.Just ("InstanceType" Lude..= instanceType),
            Lude.Just ("VolumeSizeInGB" Lude..= volumeSizeInGB)
          ]
      )
