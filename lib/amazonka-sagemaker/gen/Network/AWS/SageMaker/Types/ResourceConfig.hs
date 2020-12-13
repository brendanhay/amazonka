{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ResourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ResourceConfig
  ( ResourceConfig (..),

    -- * Smart constructor
    mkResourceConfig,

    -- * Lenses
    rcInstanceCount,
    rcInstanceType,
    rcVolumeSizeInGB,
    rcVolumeKMSKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.TrainingInstanceType

-- | Describes the resources, including ML compute instances and ML storage volumes, to use for model training.
--
-- /See:/ 'mkResourceConfig' smart constructor.
data ResourceConfig = ResourceConfig'
  { -- | The number of ML compute instances to use. For distributed training, provide a value greater than 1.
    instanceCount :: Lude.Natural,
    -- | The ML compute instance type.
    instanceType :: TrainingInstanceType,
    -- | The size of the ML storage volume that you want to provision.
    --
    -- ML storage volumes store model artifacts and incremental states. Training algorithms might also use the ML storage volume for scratch space. If you want to store the training data in the ML storage volume, choose @File@ as the @TrainingInputMode@ in the algorithm specification.
    -- You must specify sufficient ML storage for your scenario.
    volumeSizeInGB :: Lude.Natural,
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
    volumeKMSKeyId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceConfig' with the minimum fields required to make a request.
--
-- * 'instanceCount' - The number of ML compute instances to use. For distributed training, provide a value greater than 1.
-- * 'instanceType' - The ML compute instance type.
-- * 'volumeSizeInGB' - The size of the ML storage volume that you want to provision.
--
-- ML storage volumes store model artifacts and incremental states. Training algorithms might also use the ML storage volume for scratch space. If you want to store the training data in the ML storage volume, choose @File@ as the @TrainingInputMode@ in the algorithm specification.
-- You must specify sufficient ML storage for your scenario.
-- * 'volumeKMSKeyId' - The AWS KMS key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the training job.
--
-- The @VolumeKmsKeyId@ can be in any of the following formats:
--
--     * // KMS Key ID
-- @"1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--     * // Amazon Resource Name (ARN) of a KMS Key
-- @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
mkResourceConfig ::
  -- | 'instanceCount'
  Lude.Natural ->
  -- | 'instanceType'
  TrainingInstanceType ->
  -- | 'volumeSizeInGB'
  Lude.Natural ->
  ResourceConfig
mkResourceConfig pInstanceCount_ pInstanceType_ pVolumeSizeInGB_ =
  ResourceConfig'
    { instanceCount = pInstanceCount_,
      instanceType = pInstanceType_,
      volumeSizeInGB = pVolumeSizeInGB_,
      volumeKMSKeyId = Lude.Nothing
    }

-- | The number of ML compute instances to use. For distributed training, provide a value greater than 1.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcInstanceCount :: Lens.Lens' ResourceConfig Lude.Natural
rcInstanceCount = Lens.lens (instanceCount :: ResourceConfig -> Lude.Natural) (\s a -> s {instanceCount = a} :: ResourceConfig)
{-# DEPRECATED rcInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The ML compute instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcInstanceType :: Lens.Lens' ResourceConfig TrainingInstanceType
rcInstanceType = Lens.lens (instanceType :: ResourceConfig -> TrainingInstanceType) (\s a -> s {instanceType = a} :: ResourceConfig)
{-# DEPRECATED rcInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The size of the ML storage volume that you want to provision.
--
-- ML storage volumes store model artifacts and incremental states. Training algorithms might also use the ML storage volume for scratch space. If you want to store the training data in the ML storage volume, choose @File@ as the @TrainingInputMode@ in the algorithm specification.
-- You must specify sufficient ML storage for your scenario.
--
-- /Note:/ Consider using 'volumeSizeInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcVolumeSizeInGB :: Lens.Lens' ResourceConfig Lude.Natural
rcVolumeSizeInGB = Lens.lens (volumeSizeInGB :: ResourceConfig -> Lude.Natural) (\s a -> s {volumeSizeInGB = a} :: ResourceConfig)
{-# DEPRECATED rcVolumeSizeInGB "Use generic-lens or generic-optics with 'volumeSizeInGB' instead." #-}

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
-- /Note:/ Consider using 'volumeKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcVolumeKMSKeyId :: Lens.Lens' ResourceConfig (Lude.Maybe Lude.Text)
rcVolumeKMSKeyId = Lens.lens (volumeKMSKeyId :: ResourceConfig -> Lude.Maybe Lude.Text) (\s a -> s {volumeKMSKeyId = a} :: ResourceConfig)
{-# DEPRECATED rcVolumeKMSKeyId "Use generic-lens or generic-optics with 'volumeKMSKeyId' instead." #-}

instance Lude.FromJSON ResourceConfig where
  parseJSON =
    Lude.withObject
      "ResourceConfig"
      ( \x ->
          ResourceConfig'
            Lude.<$> (x Lude..: "InstanceCount")
            Lude.<*> (x Lude..: "InstanceType")
            Lude.<*> (x Lude..: "VolumeSizeInGB")
            Lude.<*> (x Lude..:? "VolumeKmsKeyId")
      )

instance Lude.ToJSON ResourceConfig where
  toJSON ResourceConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("InstanceCount" Lude..= instanceCount),
            Lude.Just ("InstanceType" Lude..= instanceType),
            Lude.Just ("VolumeSizeInGB" Lude..= volumeSizeInGB),
            ("VolumeKmsKeyId" Lude..=) Lude.<$> volumeKMSKeyId
          ]
      )
