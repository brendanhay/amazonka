{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TransformResources
  ( TransformResources (..)
  -- * Smart constructor
  , mkTransformResources
  -- * Lenses
  , trInstanceType
  , trInstanceCount
  , trVolumeKmsKeyId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.TransformInstanceType as Types
import qualified Network.AWS.SageMaker.Types.VolumeKmsKeyId as Types

-- | Describes the resources, including ML instance types and ML instance count, to use for transform job.
--
-- /See:/ 'mkTransformResources' smart constructor.
data TransformResources = TransformResources'
  { instanceType :: Types.TransformInstanceType
    -- ^ The ML compute instance type for the transform job. If you are using built-in algorithms to transform moderately sized datasets, we recommend using ml.m4.xlarge or @ml.m5.large@ instance types.
  , instanceCount :: Core.Natural
    -- ^ The number of ML compute instances to use in the transform job. For distributed transform jobs, specify a value greater than 1. The default value is @1@ .
  , volumeKmsKeyId :: Core.Maybe Types.VolumeKmsKeyId
    -- ^ The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt model data on the storage volume attached to the ML compute instance(s) that run the batch transform job. The @VolumeKmsKeyId@ can be any of the following formats:
--
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@ 
--
--
--     * Key ARN: @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ 
--
--
--     * Alias name: @alias/ExampleAlias@ 
--
--
--     * Alias name ARN: @arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias@ 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransformResources' value with any optional fields omitted.
mkTransformResources
    :: Types.TransformInstanceType -- ^ 'instanceType'
    -> Core.Natural -- ^ 'instanceCount'
    -> TransformResources
mkTransformResources instanceType instanceCount
  = TransformResources'{instanceType, instanceCount,
                        volumeKmsKeyId = Core.Nothing}

-- | The ML compute instance type for the transform job. If you are using built-in algorithms to transform moderately sized datasets, we recommend using ml.m4.xlarge or @ml.m5.large@ instance types.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trInstanceType :: Lens.Lens' TransformResources Types.TransformInstanceType
trInstanceType = Lens.field @"instanceType"
{-# INLINEABLE trInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The number of ML compute instances to use in the transform job. For distributed transform jobs, specify a value greater than 1. The default value is @1@ .
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trInstanceCount :: Lens.Lens' TransformResources Core.Natural
trInstanceCount = Lens.field @"instanceCount"
{-# INLINEABLE trInstanceCount #-}
{-# DEPRECATED instanceCount "Use generic-lens or generic-optics with 'instanceCount' instead"  #-}

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt model data on the storage volume attached to the ML compute instance(s) that run the batch transform job. The @VolumeKmsKeyId@ can be any of the following formats:
--
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@ 
--
--
--     * Key ARN: @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ 
--
--
--     * Alias name: @alias/ExampleAlias@ 
--
--
--     * Alias name ARN: @arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias@ 
--
--
--
-- /Note:/ Consider using 'volumeKmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trVolumeKmsKeyId :: Lens.Lens' TransformResources (Core.Maybe Types.VolumeKmsKeyId)
trVolumeKmsKeyId = Lens.field @"volumeKmsKeyId"
{-# INLINEABLE trVolumeKmsKeyId #-}
{-# DEPRECATED volumeKmsKeyId "Use generic-lens or generic-optics with 'volumeKmsKeyId' instead"  #-}

instance Core.FromJSON TransformResources where
        toJSON TransformResources{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InstanceType" Core..= instanceType),
                  Core.Just ("InstanceCount" Core..= instanceCount),
                  ("VolumeKmsKeyId" Core..=) Core.<$> volumeKmsKeyId])

instance Core.FromJSON TransformResources where
        parseJSON
          = Core.withObject "TransformResources" Core.$
              \ x ->
                TransformResources' Core.<$>
                  (x Core..: "InstanceType") Core.<*> x Core..: "InstanceCount"
                    Core.<*> x Core..:? "VolumeKmsKeyId"
