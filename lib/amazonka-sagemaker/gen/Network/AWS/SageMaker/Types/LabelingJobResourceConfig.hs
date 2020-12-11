-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobResourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobResourceConfig
  ( LabelingJobResourceConfig (..),

    -- * Smart constructor
    mkLabelingJobResourceConfig,

    -- * Lenses
    ljrcVolumeKMSKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides configuration information for labeling jobs.
--
-- /See:/ 'mkLabelingJobResourceConfig' smart constructor.
newtype LabelingJobResourceConfig = LabelingJobResourceConfig'
  { volumeKMSKeyId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LabelingJobResourceConfig' with the minimum fields required to make a request.
--
-- * 'volumeKMSKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the training job. The @VolumeKmsKeyId@ can be any of the following formats:
--
--
--     * // KMS Key ID
-- @"1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--     * // Amazon Resource Name (ARN) of a KMS Key
-- @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
mkLabelingJobResourceConfig ::
  LabelingJobResourceConfig
mkLabelingJobResourceConfig =
  LabelingJobResourceConfig' {volumeKMSKeyId = Lude.Nothing}

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the training job. The @VolumeKmsKeyId@ can be any of the following formats:
--
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
ljrcVolumeKMSKeyId :: Lens.Lens' LabelingJobResourceConfig (Lude.Maybe Lude.Text)
ljrcVolumeKMSKeyId = Lens.lens (volumeKMSKeyId :: LabelingJobResourceConfig -> Lude.Maybe Lude.Text) (\s a -> s {volumeKMSKeyId = a} :: LabelingJobResourceConfig)
{-# DEPRECATED ljrcVolumeKMSKeyId "Use generic-lens or generic-optics with 'volumeKMSKeyId' instead." #-}

instance Lude.FromJSON LabelingJobResourceConfig where
  parseJSON =
    Lude.withObject
      "LabelingJobResourceConfig"
      ( \x ->
          LabelingJobResourceConfig' Lude.<$> (x Lude..:? "VolumeKmsKeyId")
      )

instance Lude.ToJSON LabelingJobResourceConfig where
  toJSON LabelingJobResourceConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [("VolumeKmsKeyId" Lude..=) Lude.<$> volumeKMSKeyId]
      )
