{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformResources
  ( TransformResources (..),

    -- * Smart constructor
    mkTransformResources,

    -- * Lenses
    trVolumeKMSKeyId,
    trInstanceType,
    trInstanceCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.TransformInstanceType

-- | Describes the resources, including ML instance types and ML instance count, to use for transform job.
--
-- /See:/ 'mkTransformResources' smart constructor.
data TransformResources = TransformResources'
  { volumeKMSKeyId ::
      Lude.Maybe Lude.Text,
    instanceType :: TransformInstanceType,
    instanceCount :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransformResources' with the minimum fields required to make a request.
--
-- * 'instanceCount' - The number of ML compute instances to use in the transform job. For distributed transform jobs, specify a value greater than 1. The default value is @1@ .
-- * 'instanceType' - The ML compute instance type for the transform job. If you are using built-in algorithms to transform moderately sized datasets, we recommend using ml.m4.xlarge or @ml.m5.large@ instance types.
-- * 'volumeKMSKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt model data on the storage volume attached to the ML compute instance(s) that run the batch transform job. The @VolumeKmsKeyId@ can be any of the following formats:
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
mkTransformResources ::
  -- | 'instanceType'
  TransformInstanceType ->
  -- | 'instanceCount'
  Lude.Natural ->
  TransformResources
mkTransformResources pInstanceType_ pInstanceCount_ =
  TransformResources'
    { volumeKMSKeyId = Lude.Nothing,
      instanceType = pInstanceType_,
      instanceCount = pInstanceCount_
    }

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
-- /Note:/ Consider using 'volumeKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trVolumeKMSKeyId :: Lens.Lens' TransformResources (Lude.Maybe Lude.Text)
trVolumeKMSKeyId = Lens.lens (volumeKMSKeyId :: TransformResources -> Lude.Maybe Lude.Text) (\s a -> s {volumeKMSKeyId = a} :: TransformResources)
{-# DEPRECATED trVolumeKMSKeyId "Use generic-lens or generic-optics with 'volumeKMSKeyId' instead." #-}

-- | The ML compute instance type for the transform job. If you are using built-in algorithms to transform moderately sized datasets, we recommend using ml.m4.xlarge or @ml.m5.large@ instance types.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trInstanceType :: Lens.Lens' TransformResources TransformInstanceType
trInstanceType = Lens.lens (instanceType :: TransformResources -> TransformInstanceType) (\s a -> s {instanceType = a} :: TransformResources)
{-# DEPRECATED trInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The number of ML compute instances to use in the transform job. For distributed transform jobs, specify a value greater than 1. The default value is @1@ .
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trInstanceCount :: Lens.Lens' TransformResources Lude.Natural
trInstanceCount = Lens.lens (instanceCount :: TransformResources -> Lude.Natural) (\s a -> s {instanceCount = a} :: TransformResources)
{-# DEPRECATED trInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

instance Lude.FromJSON TransformResources where
  parseJSON =
    Lude.withObject
      "TransformResources"
      ( \x ->
          TransformResources'
            Lude.<$> (x Lude..:? "VolumeKmsKeyId")
            Lude.<*> (x Lude..: "InstanceType")
            Lude.<*> (x Lude..: "InstanceCount")
      )

instance Lude.ToJSON TransformResources where
  toJSON TransformResources' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("VolumeKmsKeyId" Lude..=) Lude.<$> volumeKMSKeyId,
            Lude.Just ("InstanceType" Lude..= instanceType),
            Lude.Just ("InstanceCount" Lude..= instanceCount)
          ]
      )
