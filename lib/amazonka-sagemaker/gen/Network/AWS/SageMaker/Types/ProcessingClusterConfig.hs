{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingClusterConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ProcessingClusterConfig
  ( ProcessingClusterConfig (..)
  -- * Smart constructor
  , mkProcessingClusterConfig
  -- * Lenses
  , pccInstanceCount
  , pccInstanceType
  , pccVolumeSizeInGB
  , pccVolumeKmsKeyId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ProcessingInstanceType as Types
import qualified Network.AWS.SageMaker.Types.VolumeKmsKeyId as Types

-- | Configuration for the cluster used to run a processing job.
--
-- /See:/ 'mkProcessingClusterConfig' smart constructor.
data ProcessingClusterConfig = ProcessingClusterConfig'
  { instanceCount :: Core.Natural
    -- ^ The number of ML compute instances to use in the processing job. For distributed processing jobs, specify a value greater than 1. The default value is 1.
  , instanceType :: Types.ProcessingInstanceType
    -- ^ The ML compute instance type for the processing job.
  , volumeSizeInGB :: Core.Natural
    -- ^ The size of the ML storage volume in gigabytes that you want to provision. You must specify sufficient ML storage for your scenario.
  , volumeKmsKeyId :: Core.Maybe Types.VolumeKmsKeyId
    -- ^ The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the processing job. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProcessingClusterConfig' value with any optional fields omitted.
mkProcessingClusterConfig
    :: Core.Natural -- ^ 'instanceCount'
    -> Types.ProcessingInstanceType -- ^ 'instanceType'
    -> Core.Natural -- ^ 'volumeSizeInGB'
    -> ProcessingClusterConfig
mkProcessingClusterConfig instanceCount instanceType volumeSizeInGB
  = ProcessingClusterConfig'{instanceCount, instanceType,
                             volumeSizeInGB, volumeKmsKeyId = Core.Nothing}

-- | The number of ML compute instances to use in the processing job. For distributed processing jobs, specify a value greater than 1. The default value is 1.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccInstanceCount :: Lens.Lens' ProcessingClusterConfig Core.Natural
pccInstanceCount = Lens.field @"instanceCount"
{-# INLINEABLE pccInstanceCount #-}
{-# DEPRECATED instanceCount "Use generic-lens or generic-optics with 'instanceCount' instead"  #-}

-- | The ML compute instance type for the processing job.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccInstanceType :: Lens.Lens' ProcessingClusterConfig Types.ProcessingInstanceType
pccInstanceType = Lens.field @"instanceType"
{-# INLINEABLE pccInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The size of the ML storage volume in gigabytes that you want to provision. You must specify sufficient ML storage for your scenario.
--
-- /Note:/ Consider using 'volumeSizeInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccVolumeSizeInGB :: Lens.Lens' ProcessingClusterConfig Core.Natural
pccVolumeSizeInGB = Lens.field @"volumeSizeInGB"
{-# INLINEABLE pccVolumeSizeInGB #-}
{-# DEPRECATED volumeSizeInGB "Use generic-lens or generic-optics with 'volumeSizeInGB' instead"  #-}

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the processing job. 
--
-- /Note:/ Consider using 'volumeKmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccVolumeKmsKeyId :: Lens.Lens' ProcessingClusterConfig (Core.Maybe Types.VolumeKmsKeyId)
pccVolumeKmsKeyId = Lens.field @"volumeKmsKeyId"
{-# INLINEABLE pccVolumeKmsKeyId #-}
{-# DEPRECATED volumeKmsKeyId "Use generic-lens or generic-optics with 'volumeKmsKeyId' instead"  #-}

instance Core.FromJSON ProcessingClusterConfig where
        toJSON ProcessingClusterConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InstanceCount" Core..= instanceCount),
                  Core.Just ("InstanceType" Core..= instanceType),
                  Core.Just ("VolumeSizeInGB" Core..= volumeSizeInGB),
                  ("VolumeKmsKeyId" Core..=) Core.<$> volumeKmsKeyId])

instance Core.FromJSON ProcessingClusterConfig where
        parseJSON
          = Core.withObject "ProcessingClusterConfig" Core.$
              \ x ->
                ProcessingClusterConfig' Core.<$>
                  (x Core..: "InstanceCount") Core.<*> x Core..: "InstanceType"
                    Core.<*> x Core..: "VolumeSizeInGB"
                    Core.<*> x Core..:? "VolumeKmsKeyId"
