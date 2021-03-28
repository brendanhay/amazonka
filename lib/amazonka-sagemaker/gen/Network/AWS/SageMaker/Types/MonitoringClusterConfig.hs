{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringClusterConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.MonitoringClusterConfig
  ( MonitoringClusterConfig (..)
  -- * Smart constructor
  , mkMonitoringClusterConfig
  -- * Lenses
  , mccInstanceCount
  , mccInstanceType
  , mccVolumeSizeInGB
  , mccVolumeKmsKeyId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ProcessingInstanceType as Types
import qualified Network.AWS.SageMaker.Types.VolumeKmsKeyId as Types

-- | Configuration for the cluster used to run model monitoring jobs.
--
-- /See:/ 'mkMonitoringClusterConfig' smart constructor.
data MonitoringClusterConfig = MonitoringClusterConfig'
  { instanceCount :: Core.Natural
    -- ^ The number of ML compute instances to use in the model monitoring job. For distributed processing jobs, specify a value greater than 1. The default value is 1.
  , instanceType :: Types.ProcessingInstanceType
    -- ^ The ML compute instance type for the processing job.
  , volumeSizeInGB :: Core.Natural
    -- ^ The size of the ML storage volume, in gigabytes, that you want to provision. You must specify sufficient ML storage for your scenario.
  , volumeKmsKeyId :: Core.Maybe Types.VolumeKmsKeyId
    -- ^ The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the model monitoring job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MonitoringClusterConfig' value with any optional fields omitted.
mkMonitoringClusterConfig
    :: Core.Natural -- ^ 'instanceCount'
    -> Types.ProcessingInstanceType -- ^ 'instanceType'
    -> Core.Natural -- ^ 'volumeSizeInGB'
    -> MonitoringClusterConfig
mkMonitoringClusterConfig instanceCount instanceType volumeSizeInGB
  = MonitoringClusterConfig'{instanceCount, instanceType,
                             volumeSizeInGB, volumeKmsKeyId = Core.Nothing}

-- | The number of ML compute instances to use in the model monitoring job. For distributed processing jobs, specify a value greater than 1. The default value is 1.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccInstanceCount :: Lens.Lens' MonitoringClusterConfig Core.Natural
mccInstanceCount = Lens.field @"instanceCount"
{-# INLINEABLE mccInstanceCount #-}
{-# DEPRECATED instanceCount "Use generic-lens or generic-optics with 'instanceCount' instead"  #-}

-- | The ML compute instance type for the processing job.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccInstanceType :: Lens.Lens' MonitoringClusterConfig Types.ProcessingInstanceType
mccInstanceType = Lens.field @"instanceType"
{-# INLINEABLE mccInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The size of the ML storage volume, in gigabytes, that you want to provision. You must specify sufficient ML storage for your scenario.
--
-- /Note:/ Consider using 'volumeSizeInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccVolumeSizeInGB :: Lens.Lens' MonitoringClusterConfig Core.Natural
mccVolumeSizeInGB = Lens.field @"volumeSizeInGB"
{-# INLINEABLE mccVolumeSizeInGB #-}
{-# DEPRECATED volumeSizeInGB "Use generic-lens or generic-optics with 'volumeSizeInGB' instead"  #-}

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the model monitoring job.
--
-- /Note:/ Consider using 'volumeKmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccVolumeKmsKeyId :: Lens.Lens' MonitoringClusterConfig (Core.Maybe Types.VolumeKmsKeyId)
mccVolumeKmsKeyId = Lens.field @"volumeKmsKeyId"
{-# INLINEABLE mccVolumeKmsKeyId #-}
{-# DEPRECATED volumeKmsKeyId "Use generic-lens or generic-optics with 'volumeKmsKeyId' instead"  #-}

instance Core.FromJSON MonitoringClusterConfig where
        toJSON MonitoringClusterConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InstanceCount" Core..= instanceCount),
                  Core.Just ("InstanceType" Core..= instanceType),
                  Core.Just ("VolumeSizeInGB" Core..= volumeSizeInGB),
                  ("VolumeKmsKeyId" Core..=) Core.<$> volumeKmsKeyId])

instance Core.FromJSON MonitoringClusterConfig where
        parseJSON
          = Core.withObject "MonitoringClusterConfig" Core.$
              \ x ->
                MonitoringClusterConfig' Core.<$>
                  (x Core..: "InstanceCount") Core.<*> x Core..: "InstanceType"
                    Core.<*> x Core..: "VolumeSizeInGB"
                    Core.<*> x Core..:? "VolumeKmsKeyId"
