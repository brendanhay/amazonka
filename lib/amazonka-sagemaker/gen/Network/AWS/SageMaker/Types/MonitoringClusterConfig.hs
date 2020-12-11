-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringClusterConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringClusterConfig
  ( MonitoringClusterConfig (..),

    -- * Smart constructor
    mkMonitoringClusterConfig,

    -- * Lenses
    mccVolumeKMSKeyId,
    mccInstanceCount,
    mccInstanceType,
    mccVolumeSizeInGB,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ProcessingInstanceType

-- | Configuration for the cluster used to run model monitoring jobs.
--
-- /See:/ 'mkMonitoringClusterConfig' smart constructor.
data MonitoringClusterConfig = MonitoringClusterConfig'
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

-- | Creates a value of 'MonitoringClusterConfig' with the minimum fields required to make a request.
--
-- * 'instanceCount' - The number of ML compute instances to use in the model monitoring job. For distributed processing jobs, specify a value greater than 1. The default value is 1.
-- * 'instanceType' - The ML compute instance type for the processing job.
-- * 'volumeKMSKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the model monitoring job.
-- * 'volumeSizeInGB' - The size of the ML storage volume, in gigabytes, that you want to provision. You must specify sufficient ML storage for your scenario.
mkMonitoringClusterConfig ::
  -- | 'instanceCount'
  Lude.Natural ->
  -- | 'instanceType'
  ProcessingInstanceType ->
  -- | 'volumeSizeInGB'
  Lude.Natural ->
  MonitoringClusterConfig
mkMonitoringClusterConfig
  pInstanceCount_
  pInstanceType_
  pVolumeSizeInGB_ =
    MonitoringClusterConfig'
      { volumeKMSKeyId = Lude.Nothing,
        instanceCount = pInstanceCount_,
        instanceType = pInstanceType_,
        volumeSizeInGB = pVolumeSizeInGB_
      }

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the model monitoring job.
--
-- /Note:/ Consider using 'volumeKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccVolumeKMSKeyId :: Lens.Lens' MonitoringClusterConfig (Lude.Maybe Lude.Text)
mccVolumeKMSKeyId = Lens.lens (volumeKMSKeyId :: MonitoringClusterConfig -> Lude.Maybe Lude.Text) (\s a -> s {volumeKMSKeyId = a} :: MonitoringClusterConfig)
{-# DEPRECATED mccVolumeKMSKeyId "Use generic-lens or generic-optics with 'volumeKMSKeyId' instead." #-}

-- | The number of ML compute instances to use in the model monitoring job. For distributed processing jobs, specify a value greater than 1. The default value is 1.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccInstanceCount :: Lens.Lens' MonitoringClusterConfig Lude.Natural
mccInstanceCount = Lens.lens (instanceCount :: MonitoringClusterConfig -> Lude.Natural) (\s a -> s {instanceCount = a} :: MonitoringClusterConfig)
{-# DEPRECATED mccInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The ML compute instance type for the processing job.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccInstanceType :: Lens.Lens' MonitoringClusterConfig ProcessingInstanceType
mccInstanceType = Lens.lens (instanceType :: MonitoringClusterConfig -> ProcessingInstanceType) (\s a -> s {instanceType = a} :: MonitoringClusterConfig)
{-# DEPRECATED mccInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The size of the ML storage volume, in gigabytes, that you want to provision. You must specify sufficient ML storage for your scenario.
--
-- /Note:/ Consider using 'volumeSizeInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccVolumeSizeInGB :: Lens.Lens' MonitoringClusterConfig Lude.Natural
mccVolumeSizeInGB = Lens.lens (volumeSizeInGB :: MonitoringClusterConfig -> Lude.Natural) (\s a -> s {volumeSizeInGB = a} :: MonitoringClusterConfig)
{-# DEPRECATED mccVolumeSizeInGB "Use generic-lens or generic-optics with 'volumeSizeInGB' instead." #-}

instance Lude.FromJSON MonitoringClusterConfig where
  parseJSON =
    Lude.withObject
      "MonitoringClusterConfig"
      ( \x ->
          MonitoringClusterConfig'
            Lude.<$> (x Lude..:? "VolumeKmsKeyId")
            Lude.<*> (x Lude..: "InstanceCount")
            Lude.<*> (x Lude..: "InstanceType")
            Lude.<*> (x Lude..: "VolumeSizeInGB")
      )

instance Lude.ToJSON MonitoringClusterConfig where
  toJSON MonitoringClusterConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("VolumeKmsKeyId" Lude..=) Lude.<$> volumeKMSKeyId,
            Lude.Just ("InstanceCount" Lude..= instanceCount),
            Lude.Just ("InstanceType" Lude..= instanceType),
            Lude.Just ("VolumeSizeInGB" Lude..= volumeSizeInGB)
          ]
      )
