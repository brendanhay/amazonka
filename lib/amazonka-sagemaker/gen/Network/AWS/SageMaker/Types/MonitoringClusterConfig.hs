{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    mccInstanceCount,
    mccInstanceType,
    mccVolumeSizeInGB,
    mccVolumeKMSKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ProcessingInstanceType

-- | Configuration for the cluster used to run model monitoring jobs.
--
-- /See:/ 'mkMonitoringClusterConfig' smart constructor.
data MonitoringClusterConfig = MonitoringClusterConfig'
  { -- | The number of ML compute instances to use in the model monitoring job. For distributed processing jobs, specify a value greater than 1. The default value is 1.
    instanceCount :: Lude.Natural,
    -- | The ML compute instance type for the processing job.
    instanceType :: ProcessingInstanceType,
    -- | The size of the ML storage volume, in gigabytes, that you want to provision. You must specify sufficient ML storage for your scenario.
    volumeSizeInGB :: Lude.Natural,
    -- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the model monitoring job.
    volumeKMSKeyId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MonitoringClusterConfig' with the minimum fields required to make a request.
--
-- * 'instanceCount' - The number of ML compute instances to use in the model monitoring job. For distributed processing jobs, specify a value greater than 1. The default value is 1.
-- * 'instanceType' - The ML compute instance type for the processing job.
-- * 'volumeSizeInGB' - The size of the ML storage volume, in gigabytes, that you want to provision. You must specify sufficient ML storage for your scenario.
-- * 'volumeKMSKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the model monitoring job.
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
      { instanceCount = pInstanceCount_,
        instanceType = pInstanceType_,
        volumeSizeInGB = pVolumeSizeInGB_,
        volumeKMSKeyId = Lude.Nothing
      }

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

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the model monitoring job.
--
-- /Note:/ Consider using 'volumeKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccVolumeKMSKeyId :: Lens.Lens' MonitoringClusterConfig (Lude.Maybe Lude.Text)
mccVolumeKMSKeyId = Lens.lens (volumeKMSKeyId :: MonitoringClusterConfig -> Lude.Maybe Lude.Text) (\s a -> s {volumeKMSKeyId = a} :: MonitoringClusterConfig)
{-# DEPRECATED mccVolumeKMSKeyId "Use generic-lens or generic-optics with 'volumeKMSKeyId' instead." #-}

instance Lude.FromJSON MonitoringClusterConfig where
  parseJSON =
    Lude.withObject
      "MonitoringClusterConfig"
      ( \x ->
          MonitoringClusterConfig'
            Lude.<$> (x Lude..: "InstanceCount")
            Lude.<*> (x Lude..: "InstanceType")
            Lude.<*> (x Lude..: "VolumeSizeInGB")
            Lude.<*> (x Lude..:? "VolumeKmsKeyId")
      )

instance Lude.ToJSON MonitoringClusterConfig where
  toJSON MonitoringClusterConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("InstanceCount" Lude..= instanceCount),
            Lude.Just ("InstanceType" Lude..= instanceType),
            Lude.Just ("VolumeSizeInGB" Lude..= volumeSizeInGB),
            ("VolumeKmsKeyId" Lude..=) Lude.<$> volumeKMSKeyId
          ]
      )
