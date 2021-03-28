{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ResourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ResourceConfig
  ( ResourceConfig (..)
  -- * Smart constructor
  , mkResourceConfig
  -- * Lenses
  , rcInstanceType
  , rcInstanceCount
  , rcVolumeSizeInGB
  , rcVolumeKmsKeyId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.KmsKeyId as Types
import qualified Network.AWS.SageMaker.Types.TrainingInstanceType as Types

-- | Describes the resources, including ML compute instances and ML storage volumes, to use for model training. 
--
-- /See:/ 'mkResourceConfig' smart constructor.
data ResourceConfig = ResourceConfig'
  { instanceType :: Types.TrainingInstanceType
    -- ^ The ML compute instance type. 
  , instanceCount :: Core.Natural
    -- ^ The number of ML compute instances to use. For distributed training, provide a value greater than 1. 
  , volumeSizeInGB :: Core.Natural
    -- ^ The size of the ML storage volume that you want to provision. 
--
-- ML storage volumes store model artifacts and incremental states. Training algorithms might also use the ML storage volume for scratch space. If you want to store the training data in the ML storage volume, choose @File@ as the @TrainingInputMode@ in the algorithm specification. 
-- You must specify sufficient ML storage for your scenario. 
  , volumeKmsKeyId :: Core.Maybe Types.KmsKeyId
    -- ^ The AWS KMS key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the training job.
--
-- The @VolumeKmsKeyId@ can be in any of the following formats:
--
--     * // KMS Key ID
-- @"1234abcd-12ab-34cd-56ef-1234567890ab"@ 
--
--
--     * // Amazon Resource Name (ARN) of a KMS Key
-- @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@ 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceConfig' value with any optional fields omitted.
mkResourceConfig
    :: Types.TrainingInstanceType -- ^ 'instanceType'
    -> Core.Natural -- ^ 'instanceCount'
    -> Core.Natural -- ^ 'volumeSizeInGB'
    -> ResourceConfig
mkResourceConfig instanceType instanceCount volumeSizeInGB
  = ResourceConfig'{instanceType, instanceCount, volumeSizeInGB,
                    volumeKmsKeyId = Core.Nothing}

-- | The ML compute instance type. 
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcInstanceType :: Lens.Lens' ResourceConfig Types.TrainingInstanceType
rcInstanceType = Lens.field @"instanceType"
{-# INLINEABLE rcInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The number of ML compute instances to use. For distributed training, provide a value greater than 1. 
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcInstanceCount :: Lens.Lens' ResourceConfig Core.Natural
rcInstanceCount = Lens.field @"instanceCount"
{-# INLINEABLE rcInstanceCount #-}
{-# DEPRECATED instanceCount "Use generic-lens or generic-optics with 'instanceCount' instead"  #-}

-- | The size of the ML storage volume that you want to provision. 
--
-- ML storage volumes store model artifacts and incremental states. Training algorithms might also use the ML storage volume for scratch space. If you want to store the training data in the ML storage volume, choose @File@ as the @TrainingInputMode@ in the algorithm specification. 
-- You must specify sufficient ML storage for your scenario. 
--
-- /Note:/ Consider using 'volumeSizeInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcVolumeSizeInGB :: Lens.Lens' ResourceConfig Core.Natural
rcVolumeSizeInGB = Lens.field @"volumeSizeInGB"
{-# INLINEABLE rcVolumeSizeInGB #-}
{-# DEPRECATED volumeSizeInGB "Use generic-lens or generic-optics with 'volumeSizeInGB' instead"  #-}

-- | The AWS KMS key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the training job.
--
-- The @VolumeKmsKeyId@ can be in any of the following formats:
--
--     * // KMS Key ID
-- @"1234abcd-12ab-34cd-56ef-1234567890ab"@ 
--
--
--     * // Amazon Resource Name (ARN) of a KMS Key
-- @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@ 
--
--
--
-- /Note:/ Consider using 'volumeKmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcVolumeKmsKeyId :: Lens.Lens' ResourceConfig (Core.Maybe Types.KmsKeyId)
rcVolumeKmsKeyId = Lens.field @"volumeKmsKeyId"
{-# INLINEABLE rcVolumeKmsKeyId #-}
{-# DEPRECATED volumeKmsKeyId "Use generic-lens or generic-optics with 'volumeKmsKeyId' instead"  #-}

instance Core.FromJSON ResourceConfig where
        toJSON ResourceConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InstanceType" Core..= instanceType),
                  Core.Just ("InstanceCount" Core..= instanceCount),
                  Core.Just ("VolumeSizeInGB" Core..= volumeSizeInGB),
                  ("VolumeKmsKeyId" Core..=) Core.<$> volumeKmsKeyId])

instance Core.FromJSON ResourceConfig where
        parseJSON
          = Core.withObject "ResourceConfig" Core.$
              \ x ->
                ResourceConfig' Core.<$>
                  (x Core..: "InstanceType") Core.<*> x Core..: "InstanceCount"
                    Core.<*> x Core..: "VolumeSizeInGB"
                    Core.<*> x Core..:? "VolumeKmsKeyId"
