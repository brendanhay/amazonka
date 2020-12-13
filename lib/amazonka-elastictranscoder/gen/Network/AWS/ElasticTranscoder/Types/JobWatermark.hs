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
    jwPresetWatermarkId,
    jwInputKey,
    jwEncryption,
  )
where

import Network.AWS.ElasticTranscoder.Types.Encryption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Watermarks can be in .png or .jpg format. If you want to display a watermark that is not rectangular, use the .png format, which supports transparency.
--
-- /See:/ 'mkJobWatermark' smart constructor.
data JobWatermark = JobWatermark'
  { -- | The ID of the watermark settings that Elastic Transcoder uses to add watermarks to the video during transcoding. The settings are in the preset specified by Preset for the current output. In that preset, the value of Watermarks Id tells Elastic Transcoder which settings to use.
    presetWatermarkId :: Lude.Maybe Lude.Text,
    -- | The name of the .png or .jpg file that you want to use for the watermark. To determine which Amazon S3 bucket contains the specified file, Elastic Transcoder checks the pipeline specified by @Pipeline@ ; the @Input Bucket@ object in that pipeline identifies the bucket.
    --
    -- If the file name includes a prefix, for example, __logos/128x64.png__ , include the prefix in the key. If the file isn't in the specified bucket, Elastic Transcoder returns an error.
    inputKey :: Lude.Maybe Lude.Text,
    -- | The encryption settings, if any, that you want Elastic Transcoder to apply to your watermarks.
    encryption :: Lude.Maybe Encryption
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobWatermark' with the minimum fields required to make a request.
--
-- * 'presetWatermarkId' - The ID of the watermark settings that Elastic Transcoder uses to add watermarks to the video during transcoding. The settings are in the preset specified by Preset for the current output. In that preset, the value of Watermarks Id tells Elastic Transcoder which settings to use.
-- * 'inputKey' - The name of the .png or .jpg file that you want to use for the watermark. To determine which Amazon S3 bucket contains the specified file, Elastic Transcoder checks the pipeline specified by @Pipeline@ ; the @Input Bucket@ object in that pipeline identifies the bucket.
--
-- If the file name includes a prefix, for example, __logos/128x64.png__ , include the prefix in the key. If the file isn't in the specified bucket, Elastic Transcoder returns an error.
-- * 'encryption' - The encryption settings, if any, that you want Elastic Transcoder to apply to your watermarks.
mkJobWatermark ::
  JobWatermark
mkJobWatermark =
  JobWatermark'
    { presetWatermarkId = Lude.Nothing,
      inputKey = Lude.Nothing,
      encryption = Lude.Nothing
    }

-- | The ID of the watermark settings that Elastic Transcoder uses to add watermarks to the video during transcoding. The settings are in the preset specified by Preset for the current output. In that preset, the value of Watermarks Id tells Elastic Transcoder which settings to use.
--
-- /Note:/ Consider using 'presetWatermarkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jwPresetWatermarkId :: Lens.Lens' JobWatermark (Lude.Maybe Lude.Text)
jwPresetWatermarkId = Lens.lens (presetWatermarkId :: JobWatermark -> Lude.Maybe Lude.Text) (\s a -> s {presetWatermarkId = a} :: JobWatermark)
{-# DEPRECATED jwPresetWatermarkId "Use generic-lens or generic-optics with 'presetWatermarkId' instead." #-}

-- | The name of the .png or .jpg file that you want to use for the watermark. To determine which Amazon S3 bucket contains the specified file, Elastic Transcoder checks the pipeline specified by @Pipeline@ ; the @Input Bucket@ object in that pipeline identifies the bucket.
--
-- If the file name includes a prefix, for example, __logos/128x64.png__ , include the prefix in the key. If the file isn't in the specified bucket, Elastic Transcoder returns an error.
--
-- /Note:/ Consider using 'inputKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jwInputKey :: Lens.Lens' JobWatermark (Lude.Maybe Lude.Text)
jwInputKey = Lens.lens (inputKey :: JobWatermark -> Lude.Maybe Lude.Text) (\s a -> s {inputKey = a} :: JobWatermark)
{-# DEPRECATED jwInputKey "Use generic-lens or generic-optics with 'inputKey' instead." #-}

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to your watermarks.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jwEncryption :: Lens.Lens' JobWatermark (Lude.Maybe Encryption)
jwEncryption = Lens.lens (encryption :: JobWatermark -> Lude.Maybe Encryption) (\s a -> s {encryption = a} :: JobWatermark)
{-# DEPRECATED jwEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

instance Lude.FromJSON JobWatermark where
  parseJSON =
    Lude.withObject
      "JobWatermark"
      ( \x ->
          JobWatermark'
            Lude.<$> (x Lude..:? "PresetWatermarkId")
            Lude.<*> (x Lude..:? "InputKey")
            Lude.<*> (x Lude..:? "Encryption")
      )

instance Lude.ToJSON JobWatermark where
  toJSON JobWatermark' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PresetWatermarkId" Lude..=) Lude.<$> presetWatermarkId,
            ("InputKey" Lude..=) Lude.<$> inputKey,
            ("Encryption" Lude..=) Lude.<$> encryption
          ]
      )
