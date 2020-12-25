{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    ljrcVolumeKmsKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.VolumeKmsKeyId as Types

-- | Provides configuration information for labeling jobs.
--
-- /See:/ 'mkLabelingJobResourceConfig' smart constructor.
newtype LabelingJobResourceConfig = LabelingJobResourceConfig'
  { -- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the training job. The @VolumeKmsKeyId@ can be any of the following formats:
    --
    --
    --     * // KMS Key ID
    -- @"1234abcd-12ab-34cd-56ef-1234567890ab"@
    --
    --
    --     * // Amazon Resource Name (ARN) of a KMS Key
    -- @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
    volumeKmsKeyId :: Core.Maybe Types.VolumeKmsKeyId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LabelingJobResourceConfig' value with any optional fields omitted.
mkLabelingJobResourceConfig ::
  LabelingJobResourceConfig
mkLabelingJobResourceConfig =
  LabelingJobResourceConfig' {volumeKmsKeyId = Core.Nothing}

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
-- /Note:/ Consider using 'volumeKmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrcVolumeKmsKeyId :: Lens.Lens' LabelingJobResourceConfig (Core.Maybe Types.VolumeKmsKeyId)
ljrcVolumeKmsKeyId = Lens.field @"volumeKmsKeyId"
{-# DEPRECATED ljrcVolumeKmsKeyId "Use generic-lens or generic-optics with 'volumeKmsKeyId' instead." #-}

instance Core.FromJSON LabelingJobResourceConfig where
  toJSON LabelingJobResourceConfig {..} =
    Core.object
      ( Core.catMaybes
          [("VolumeKmsKeyId" Core..=) Core.<$> volumeKmsKeyId]
      )

instance Core.FromJSON LabelingJobResourceConfig where
  parseJSON =
    Core.withObject "LabelingJobResourceConfig" Core.$
      \x ->
        LabelingJobResourceConfig' Core.<$> (x Core..:? "VolumeKmsKeyId")
