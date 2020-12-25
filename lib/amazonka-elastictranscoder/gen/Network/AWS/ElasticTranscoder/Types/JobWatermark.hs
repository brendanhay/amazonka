{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.JobWatermark
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.JobWatermark
  ( JobWatermark (..),

    -- * Smart constructor
    mkJobWatermark,

    -- * Lenses
    jwEncryption,
    jwInputKey,
    jwPresetWatermarkId,
  )
where

import qualified Network.AWS.ElasticTranscoder.Types.Encryption as Types
import qualified Network.AWS.ElasticTranscoder.Types.PresetWatermarkId as Types
import qualified Network.AWS.ElasticTranscoder.Types.WatermarkKey as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Watermarks can be in .png or .jpg format. If you want to display a watermark that is not rectangular, use the .png format, which supports transparency.
--
-- /See:/ 'mkJobWatermark' smart constructor.
data JobWatermark = JobWatermark'
  { -- | The encryption settings, if any, that you want Elastic Transcoder to apply to your watermarks.
    encryption :: Core.Maybe Types.Encryption,
    -- | The name of the .png or .jpg file that you want to use for the watermark. To determine which Amazon S3 bucket contains the specified file, Elastic Transcoder checks the pipeline specified by @Pipeline@ ; the @Input Bucket@ object in that pipeline identifies the bucket.
    --
    -- If the file name includes a prefix, for example, __logos/128x64.png__ , include the prefix in the key. If the file isn't in the specified bucket, Elastic Transcoder returns an error.
    inputKey :: Core.Maybe Types.WatermarkKey,
    -- | The ID of the watermark settings that Elastic Transcoder uses to add watermarks to the video during transcoding. The settings are in the preset specified by Preset for the current output. In that preset, the value of Watermarks Id tells Elastic Transcoder which settings to use.
    presetWatermarkId :: Core.Maybe Types.PresetWatermarkId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobWatermark' value with any optional fields omitted.
mkJobWatermark ::
  JobWatermark
mkJobWatermark =
  JobWatermark'
    { encryption = Core.Nothing,
      inputKey = Core.Nothing,
      presetWatermarkId = Core.Nothing
    }

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to your watermarks.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jwEncryption :: Lens.Lens' JobWatermark (Core.Maybe Types.Encryption)
jwEncryption = Lens.field @"encryption"
{-# DEPRECATED jwEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | The name of the .png or .jpg file that you want to use for the watermark. To determine which Amazon S3 bucket contains the specified file, Elastic Transcoder checks the pipeline specified by @Pipeline@ ; the @Input Bucket@ object in that pipeline identifies the bucket.
--
-- If the file name includes a prefix, for example, __logos/128x64.png__ , include the prefix in the key. If the file isn't in the specified bucket, Elastic Transcoder returns an error.
--
-- /Note:/ Consider using 'inputKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jwInputKey :: Lens.Lens' JobWatermark (Core.Maybe Types.WatermarkKey)
jwInputKey = Lens.field @"inputKey"
{-# DEPRECATED jwInputKey "Use generic-lens or generic-optics with 'inputKey' instead." #-}

-- | The ID of the watermark settings that Elastic Transcoder uses to add watermarks to the video during transcoding. The settings are in the preset specified by Preset for the current output. In that preset, the value of Watermarks Id tells Elastic Transcoder which settings to use.
--
-- /Note:/ Consider using 'presetWatermarkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jwPresetWatermarkId :: Lens.Lens' JobWatermark (Core.Maybe Types.PresetWatermarkId)
jwPresetWatermarkId = Lens.field @"presetWatermarkId"
{-# DEPRECATED jwPresetWatermarkId "Use generic-lens or generic-optics with 'presetWatermarkId' instead." #-}

instance Core.FromJSON JobWatermark where
  toJSON JobWatermark {..} =
    Core.object
      ( Core.catMaybes
          [ ("Encryption" Core..=) Core.<$> encryption,
            ("InputKey" Core..=) Core.<$> inputKey,
            ("PresetWatermarkId" Core..=) Core.<$> presetWatermarkId
          ]
      )

instance Core.FromJSON JobWatermark where
  parseJSON =
    Core.withObject "JobWatermark" Core.$
      \x ->
        JobWatermark'
          Core.<$> (x Core..:? "Encryption")
          Core.<*> (x Core..:? "InputKey")
          Core.<*> (x Core..:? "PresetWatermarkId")
