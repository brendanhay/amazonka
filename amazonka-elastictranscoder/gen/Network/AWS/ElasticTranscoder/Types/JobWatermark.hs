{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.JobWatermark
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.JobWatermark where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticTranscoder.Types.Encryption
import qualified Network.AWS.Lens as Lens

-- | Watermarks can be in .png or .jpg format. If you want to display a
-- watermark that is not rectangular, use the .png format, which supports
-- transparency.
--
-- /See:/ 'newJobWatermark' smart constructor.
data JobWatermark = JobWatermark'
  { -- | The name of the .png or .jpg file that you want to use for the
    -- watermark. To determine which Amazon S3 bucket contains the specified
    -- file, Elastic Transcoder checks the pipeline specified by @Pipeline@;
    -- the @Input Bucket@ object in that pipeline identifies the bucket.
    --
    -- If the file name includes a prefix, for example, __logos\/128x64.png__,
    -- include the prefix in the key. If the file isn\'t in the specified
    -- bucket, Elastic Transcoder returns an error.
    inputKey :: Core.Maybe Core.Text,
    -- | The encryption settings, if any, that you want Elastic Transcoder to
    -- apply to your watermarks.
    encryption :: Core.Maybe Encryption,
    -- | The ID of the watermark settings that Elastic Transcoder uses to add
    -- watermarks to the video during transcoding. The settings are in the
    -- preset specified by Preset for the current output. In that preset, the
    -- value of Watermarks Id tells Elastic Transcoder which settings to use.
    presetWatermarkId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JobWatermark' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputKey', 'jobWatermark_inputKey' - The name of the .png or .jpg file that you want to use for the
-- watermark. To determine which Amazon S3 bucket contains the specified
-- file, Elastic Transcoder checks the pipeline specified by @Pipeline@;
-- the @Input Bucket@ object in that pipeline identifies the bucket.
--
-- If the file name includes a prefix, for example, __logos\/128x64.png__,
-- include the prefix in the key. If the file isn\'t in the specified
-- bucket, Elastic Transcoder returns an error.
--
-- 'encryption', 'jobWatermark_encryption' - The encryption settings, if any, that you want Elastic Transcoder to
-- apply to your watermarks.
--
-- 'presetWatermarkId', 'jobWatermark_presetWatermarkId' - The ID of the watermark settings that Elastic Transcoder uses to add
-- watermarks to the video during transcoding. The settings are in the
-- preset specified by Preset for the current output. In that preset, the
-- value of Watermarks Id tells Elastic Transcoder which settings to use.
newJobWatermark ::
  JobWatermark
newJobWatermark =
  JobWatermark'
    { inputKey = Core.Nothing,
      encryption = Core.Nothing,
      presetWatermarkId = Core.Nothing
    }

-- | The name of the .png or .jpg file that you want to use for the
-- watermark. To determine which Amazon S3 bucket contains the specified
-- file, Elastic Transcoder checks the pipeline specified by @Pipeline@;
-- the @Input Bucket@ object in that pipeline identifies the bucket.
--
-- If the file name includes a prefix, for example, __logos\/128x64.png__,
-- include the prefix in the key. If the file isn\'t in the specified
-- bucket, Elastic Transcoder returns an error.
jobWatermark_inputKey :: Lens.Lens' JobWatermark (Core.Maybe Core.Text)
jobWatermark_inputKey = Lens.lens (\JobWatermark' {inputKey} -> inputKey) (\s@JobWatermark' {} a -> s {inputKey = a} :: JobWatermark)

-- | The encryption settings, if any, that you want Elastic Transcoder to
-- apply to your watermarks.
jobWatermark_encryption :: Lens.Lens' JobWatermark (Core.Maybe Encryption)
jobWatermark_encryption = Lens.lens (\JobWatermark' {encryption} -> encryption) (\s@JobWatermark' {} a -> s {encryption = a} :: JobWatermark)

-- | The ID of the watermark settings that Elastic Transcoder uses to add
-- watermarks to the video during transcoding. The settings are in the
-- preset specified by Preset for the current output. In that preset, the
-- value of Watermarks Id tells Elastic Transcoder which settings to use.
jobWatermark_presetWatermarkId :: Lens.Lens' JobWatermark (Core.Maybe Core.Text)
jobWatermark_presetWatermarkId = Lens.lens (\JobWatermark' {presetWatermarkId} -> presetWatermarkId) (\s@JobWatermark' {} a -> s {presetWatermarkId = a} :: JobWatermark)

instance Core.FromJSON JobWatermark where
  parseJSON =
    Core.withObject
      "JobWatermark"
      ( \x ->
          JobWatermark'
            Core.<$> (x Core..:? "InputKey")
            Core.<*> (x Core..:? "Encryption")
            Core.<*> (x Core..:? "PresetWatermarkId")
      )

instance Core.Hashable JobWatermark

instance Core.NFData JobWatermark

instance Core.ToJSON JobWatermark where
  toJSON JobWatermark' {..} =
    Core.object
      ( Core.catMaybes
          [ ("InputKey" Core..=) Core.<$> inputKey,
            ("Encryption" Core..=) Core.<$> encryption,
            ("PresetWatermarkId" Core..=)
              Core.<$> presetWatermarkId
          ]
      )
