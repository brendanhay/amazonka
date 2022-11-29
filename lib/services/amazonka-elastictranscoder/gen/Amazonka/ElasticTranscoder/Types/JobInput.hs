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
-- Module      : Amazonka.ElasticTranscoder.Types.JobInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.JobInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticTranscoder.Types.DetectedProperties
import Amazonka.ElasticTranscoder.Types.Encryption
import Amazonka.ElasticTranscoder.Types.InputCaptions
import Amazonka.ElasticTranscoder.Types.TimeSpan
import qualified Amazonka.Prelude as Prelude

-- | Information about the file that you\'re transcoding.
--
-- /See:/ 'newJobInput' smart constructor.
data JobInput = JobInput'
  { -- | The detected properties of the input file.
    detectedProperties :: Prelude.Maybe DetectedProperties,
    -- | The name of the file to transcode. Elsewhere in the body of the JSON
    -- block is the the ID of the pipeline to use for processing the job. The
    -- @InputBucket@ object in that pipeline tells Elastic Transcoder which
    -- Amazon S3 bucket to get the file from.
    --
    -- If the file name includes a prefix, such as @cooking\/lasagna.mpg@,
    -- include the prefix in the key. If the file isn\'t in the specified
    -- bucket, Elastic Transcoder returns an error.
    key :: Prelude.Maybe Prelude.Text,
    -- | The aspect ratio of the input file. If you want Elastic Transcoder to
    -- automatically detect the aspect ratio of the input file, specify @auto@.
    -- If you want to specify the aspect ratio for the output file, enter one
    -- of the following values:
    --
    -- @1:1@, @4:3@, @3:2@, @16:9@
    --
    -- If you specify a value other than @auto@, Elastic Transcoder disables
    -- automatic detection of the aspect ratio.
    aspectRatio :: Prelude.Maybe Prelude.Text,
    -- | Whether the input file is interlaced. If you want Elastic Transcoder to
    -- automatically detect whether the input file is interlaced, specify
    -- @auto@. If you want to specify whether the input file is interlaced,
    -- enter one of the following values:
    --
    -- @true@, @false@
    --
    -- If you specify a value other than @auto@, Elastic Transcoder disables
    -- automatic detection of interlacing.
    interlaced :: Prelude.Maybe Prelude.Text,
    -- | Settings for clipping an input. Each input can have different clip
    -- settings.
    timeSpan :: Prelude.Maybe TimeSpan,
    -- | The container type for the input file. If you want Elastic Transcoder to
    -- automatically detect the container type of the input file, specify
    -- @auto@. If you want to specify the container type for the input file,
    -- enter one of the following values:
    --
    -- @3gp@, @aac@, @asf@, @avi@, @divx@, @flv@, @m4a@, @mkv@, @mov@, @mp3@,
    -- @mp4@, @mpeg@, @mpeg-ps@, @mpeg-ts@, @mxf@, @ogg@, @vob@, @wav@, @webm@
    container :: Prelude.Maybe Prelude.Text,
    -- | The encryption settings, if any, that are used for decrypting your input
    -- files. If your input file is encrypted, you must specify the mode that
    -- Elastic Transcoder uses to decrypt your file.
    encryption :: Prelude.Maybe Encryption,
    -- | This value must be @auto@, which causes Elastic Transcoder to
    -- automatically detect the resolution of the input file.
    resolution :: Prelude.Maybe Prelude.Text,
    -- | You can configure Elastic Transcoder to transcode captions, or
    -- subtitles, from one format to another. All captions must be in UTF-8.
    -- Elastic Transcoder supports two types of captions:
    --
    -- -   __Embedded:__ Embedded captions are included in the same file as the
    --     audio and video. Elastic Transcoder supports only one embedded
    --     caption per language, to a maximum of 300 embedded captions per
    --     file.
    --
    --     Valid input values include: @CEA-608 (EIA-608@, first non-empty
    --     channel only), @CEA-708 (EIA-708@, first non-empty channel only),
    --     and @mov-text@
    --
    --     Valid outputs include: @mov-text@
    --
    --     Elastic Transcoder supports a maximum of one embedded format per
    --     output.
    --
    -- -   __Sidecar:__ Sidecar captions are kept in a separate metadata file
    --     from the audio and video data. Sidecar captions require a player
    --     that is capable of understanding the relationship between the video
    --     file and the sidecar file. Elastic Transcoder supports only one
    --     sidecar caption per language, to a maximum of 20 sidecar captions
    --     per file.
    --
    --     Valid input values include: @dfxp@ (first div element only),
    --     @ebu-tt@, @scc@, @smpt@, @srt@, @ttml@ (first div element only), and
    --     @webvtt@
    --
    --     Valid outputs include: @dfxp@ (first div element only), @scc@,
    --     @srt@, and @webvtt@.
    --
    -- If you want ttml or smpte-tt compatible captions, specify dfxp as your
    -- output format.
    --
    -- Elastic Transcoder does not support OCR (Optical Character Recognition),
    -- does not accept pictures as a valid input for captions, and is not
    -- available for audio-only transcoding. Elastic Transcoder does not
    -- preserve text formatting (for example, italics) during the transcoding
    -- process.
    --
    -- To remove captions or leave the captions empty, set @Captions@ to null.
    -- To pass through existing captions unchanged, set the @MergePolicy@ to
    -- @MergeRetain@, and pass in a null @CaptionSources@ array.
    --
    -- For more information on embedded files, see the Subtitles Wikipedia
    -- page.
    --
    -- For more information on sidecar files, see the Extensible Metadata
    -- Platform and Sidecar file Wikipedia pages.
    inputCaptions :: Prelude.Maybe InputCaptions,
    -- | The frame rate of the input file. If you want Elastic Transcoder to
    -- automatically detect the frame rate of the input file, specify @auto@.
    -- If you want to specify the frame rate for the input file, enter one of
    -- the following values:
    --
    -- @10@, @15@, @23.97@, @24@, @25@, @29.97@, @30@, @60@
    --
    -- If you specify a value other than @auto@, Elastic Transcoder disables
    -- automatic detection of the frame rate.
    frameRate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectedProperties', 'jobInput_detectedProperties' - The detected properties of the input file.
--
-- 'key', 'jobInput_key' - The name of the file to transcode. Elsewhere in the body of the JSON
-- block is the the ID of the pipeline to use for processing the job. The
-- @InputBucket@ object in that pipeline tells Elastic Transcoder which
-- Amazon S3 bucket to get the file from.
--
-- If the file name includes a prefix, such as @cooking\/lasagna.mpg@,
-- include the prefix in the key. If the file isn\'t in the specified
-- bucket, Elastic Transcoder returns an error.
--
-- 'aspectRatio', 'jobInput_aspectRatio' - The aspect ratio of the input file. If you want Elastic Transcoder to
-- automatically detect the aspect ratio of the input file, specify @auto@.
-- If you want to specify the aspect ratio for the output file, enter one
-- of the following values:
--
-- @1:1@, @4:3@, @3:2@, @16:9@
--
-- If you specify a value other than @auto@, Elastic Transcoder disables
-- automatic detection of the aspect ratio.
--
-- 'interlaced', 'jobInput_interlaced' - Whether the input file is interlaced. If you want Elastic Transcoder to
-- automatically detect whether the input file is interlaced, specify
-- @auto@. If you want to specify whether the input file is interlaced,
-- enter one of the following values:
--
-- @true@, @false@
--
-- If you specify a value other than @auto@, Elastic Transcoder disables
-- automatic detection of interlacing.
--
-- 'timeSpan', 'jobInput_timeSpan' - Settings for clipping an input. Each input can have different clip
-- settings.
--
-- 'container', 'jobInput_container' - The container type for the input file. If you want Elastic Transcoder to
-- automatically detect the container type of the input file, specify
-- @auto@. If you want to specify the container type for the input file,
-- enter one of the following values:
--
-- @3gp@, @aac@, @asf@, @avi@, @divx@, @flv@, @m4a@, @mkv@, @mov@, @mp3@,
-- @mp4@, @mpeg@, @mpeg-ps@, @mpeg-ts@, @mxf@, @ogg@, @vob@, @wav@, @webm@
--
-- 'encryption', 'jobInput_encryption' - The encryption settings, if any, that are used for decrypting your input
-- files. If your input file is encrypted, you must specify the mode that
-- Elastic Transcoder uses to decrypt your file.
--
-- 'resolution', 'jobInput_resolution' - This value must be @auto@, which causes Elastic Transcoder to
-- automatically detect the resolution of the input file.
--
-- 'inputCaptions', 'jobInput_inputCaptions' - You can configure Elastic Transcoder to transcode captions, or
-- subtitles, from one format to another. All captions must be in UTF-8.
-- Elastic Transcoder supports two types of captions:
--
-- -   __Embedded:__ Embedded captions are included in the same file as the
--     audio and video. Elastic Transcoder supports only one embedded
--     caption per language, to a maximum of 300 embedded captions per
--     file.
--
--     Valid input values include: @CEA-608 (EIA-608@, first non-empty
--     channel only), @CEA-708 (EIA-708@, first non-empty channel only),
--     and @mov-text@
--
--     Valid outputs include: @mov-text@
--
--     Elastic Transcoder supports a maximum of one embedded format per
--     output.
--
-- -   __Sidecar:__ Sidecar captions are kept in a separate metadata file
--     from the audio and video data. Sidecar captions require a player
--     that is capable of understanding the relationship between the video
--     file and the sidecar file. Elastic Transcoder supports only one
--     sidecar caption per language, to a maximum of 20 sidecar captions
--     per file.
--
--     Valid input values include: @dfxp@ (first div element only),
--     @ebu-tt@, @scc@, @smpt@, @srt@, @ttml@ (first div element only), and
--     @webvtt@
--
--     Valid outputs include: @dfxp@ (first div element only), @scc@,
--     @srt@, and @webvtt@.
--
-- If you want ttml or smpte-tt compatible captions, specify dfxp as your
-- output format.
--
-- Elastic Transcoder does not support OCR (Optical Character Recognition),
-- does not accept pictures as a valid input for captions, and is not
-- available for audio-only transcoding. Elastic Transcoder does not
-- preserve text formatting (for example, italics) during the transcoding
-- process.
--
-- To remove captions or leave the captions empty, set @Captions@ to null.
-- To pass through existing captions unchanged, set the @MergePolicy@ to
-- @MergeRetain@, and pass in a null @CaptionSources@ array.
--
-- For more information on embedded files, see the Subtitles Wikipedia
-- page.
--
-- For more information on sidecar files, see the Extensible Metadata
-- Platform and Sidecar file Wikipedia pages.
--
-- 'frameRate', 'jobInput_frameRate' - The frame rate of the input file. If you want Elastic Transcoder to
-- automatically detect the frame rate of the input file, specify @auto@.
-- If you want to specify the frame rate for the input file, enter one of
-- the following values:
--
-- @10@, @15@, @23.97@, @24@, @25@, @29.97@, @30@, @60@
--
-- If you specify a value other than @auto@, Elastic Transcoder disables
-- automatic detection of the frame rate.
newJobInput ::
  JobInput
newJobInput =
  JobInput'
    { detectedProperties = Prelude.Nothing,
      key = Prelude.Nothing,
      aspectRatio = Prelude.Nothing,
      interlaced = Prelude.Nothing,
      timeSpan = Prelude.Nothing,
      container = Prelude.Nothing,
      encryption = Prelude.Nothing,
      resolution = Prelude.Nothing,
      inputCaptions = Prelude.Nothing,
      frameRate = Prelude.Nothing
    }

-- | The detected properties of the input file.
jobInput_detectedProperties :: Lens.Lens' JobInput (Prelude.Maybe DetectedProperties)
jobInput_detectedProperties = Lens.lens (\JobInput' {detectedProperties} -> detectedProperties) (\s@JobInput' {} a -> s {detectedProperties = a} :: JobInput)

-- | The name of the file to transcode. Elsewhere in the body of the JSON
-- block is the the ID of the pipeline to use for processing the job. The
-- @InputBucket@ object in that pipeline tells Elastic Transcoder which
-- Amazon S3 bucket to get the file from.
--
-- If the file name includes a prefix, such as @cooking\/lasagna.mpg@,
-- include the prefix in the key. If the file isn\'t in the specified
-- bucket, Elastic Transcoder returns an error.
jobInput_key :: Lens.Lens' JobInput (Prelude.Maybe Prelude.Text)
jobInput_key = Lens.lens (\JobInput' {key} -> key) (\s@JobInput' {} a -> s {key = a} :: JobInput)

-- | The aspect ratio of the input file. If you want Elastic Transcoder to
-- automatically detect the aspect ratio of the input file, specify @auto@.
-- If you want to specify the aspect ratio for the output file, enter one
-- of the following values:
--
-- @1:1@, @4:3@, @3:2@, @16:9@
--
-- If you specify a value other than @auto@, Elastic Transcoder disables
-- automatic detection of the aspect ratio.
jobInput_aspectRatio :: Lens.Lens' JobInput (Prelude.Maybe Prelude.Text)
jobInput_aspectRatio = Lens.lens (\JobInput' {aspectRatio} -> aspectRatio) (\s@JobInput' {} a -> s {aspectRatio = a} :: JobInput)

-- | Whether the input file is interlaced. If you want Elastic Transcoder to
-- automatically detect whether the input file is interlaced, specify
-- @auto@. If you want to specify whether the input file is interlaced,
-- enter one of the following values:
--
-- @true@, @false@
--
-- If you specify a value other than @auto@, Elastic Transcoder disables
-- automatic detection of interlacing.
jobInput_interlaced :: Lens.Lens' JobInput (Prelude.Maybe Prelude.Text)
jobInput_interlaced = Lens.lens (\JobInput' {interlaced} -> interlaced) (\s@JobInput' {} a -> s {interlaced = a} :: JobInput)

-- | Settings for clipping an input. Each input can have different clip
-- settings.
jobInput_timeSpan :: Lens.Lens' JobInput (Prelude.Maybe TimeSpan)
jobInput_timeSpan = Lens.lens (\JobInput' {timeSpan} -> timeSpan) (\s@JobInput' {} a -> s {timeSpan = a} :: JobInput)

-- | The container type for the input file. If you want Elastic Transcoder to
-- automatically detect the container type of the input file, specify
-- @auto@. If you want to specify the container type for the input file,
-- enter one of the following values:
--
-- @3gp@, @aac@, @asf@, @avi@, @divx@, @flv@, @m4a@, @mkv@, @mov@, @mp3@,
-- @mp4@, @mpeg@, @mpeg-ps@, @mpeg-ts@, @mxf@, @ogg@, @vob@, @wav@, @webm@
jobInput_container :: Lens.Lens' JobInput (Prelude.Maybe Prelude.Text)
jobInput_container = Lens.lens (\JobInput' {container} -> container) (\s@JobInput' {} a -> s {container = a} :: JobInput)

-- | The encryption settings, if any, that are used for decrypting your input
-- files. If your input file is encrypted, you must specify the mode that
-- Elastic Transcoder uses to decrypt your file.
jobInput_encryption :: Lens.Lens' JobInput (Prelude.Maybe Encryption)
jobInput_encryption = Lens.lens (\JobInput' {encryption} -> encryption) (\s@JobInput' {} a -> s {encryption = a} :: JobInput)

-- | This value must be @auto@, which causes Elastic Transcoder to
-- automatically detect the resolution of the input file.
jobInput_resolution :: Lens.Lens' JobInput (Prelude.Maybe Prelude.Text)
jobInput_resolution = Lens.lens (\JobInput' {resolution} -> resolution) (\s@JobInput' {} a -> s {resolution = a} :: JobInput)

-- | You can configure Elastic Transcoder to transcode captions, or
-- subtitles, from one format to another. All captions must be in UTF-8.
-- Elastic Transcoder supports two types of captions:
--
-- -   __Embedded:__ Embedded captions are included in the same file as the
--     audio and video. Elastic Transcoder supports only one embedded
--     caption per language, to a maximum of 300 embedded captions per
--     file.
--
--     Valid input values include: @CEA-608 (EIA-608@, first non-empty
--     channel only), @CEA-708 (EIA-708@, first non-empty channel only),
--     and @mov-text@
--
--     Valid outputs include: @mov-text@
--
--     Elastic Transcoder supports a maximum of one embedded format per
--     output.
--
-- -   __Sidecar:__ Sidecar captions are kept in a separate metadata file
--     from the audio and video data. Sidecar captions require a player
--     that is capable of understanding the relationship between the video
--     file and the sidecar file. Elastic Transcoder supports only one
--     sidecar caption per language, to a maximum of 20 sidecar captions
--     per file.
--
--     Valid input values include: @dfxp@ (first div element only),
--     @ebu-tt@, @scc@, @smpt@, @srt@, @ttml@ (first div element only), and
--     @webvtt@
--
--     Valid outputs include: @dfxp@ (first div element only), @scc@,
--     @srt@, and @webvtt@.
--
-- If you want ttml or smpte-tt compatible captions, specify dfxp as your
-- output format.
--
-- Elastic Transcoder does not support OCR (Optical Character Recognition),
-- does not accept pictures as a valid input for captions, and is not
-- available for audio-only transcoding. Elastic Transcoder does not
-- preserve text formatting (for example, italics) during the transcoding
-- process.
--
-- To remove captions or leave the captions empty, set @Captions@ to null.
-- To pass through existing captions unchanged, set the @MergePolicy@ to
-- @MergeRetain@, and pass in a null @CaptionSources@ array.
--
-- For more information on embedded files, see the Subtitles Wikipedia
-- page.
--
-- For more information on sidecar files, see the Extensible Metadata
-- Platform and Sidecar file Wikipedia pages.
jobInput_inputCaptions :: Lens.Lens' JobInput (Prelude.Maybe InputCaptions)
jobInput_inputCaptions = Lens.lens (\JobInput' {inputCaptions} -> inputCaptions) (\s@JobInput' {} a -> s {inputCaptions = a} :: JobInput)

-- | The frame rate of the input file. If you want Elastic Transcoder to
-- automatically detect the frame rate of the input file, specify @auto@.
-- If you want to specify the frame rate for the input file, enter one of
-- the following values:
--
-- @10@, @15@, @23.97@, @24@, @25@, @29.97@, @30@, @60@
--
-- If you specify a value other than @auto@, Elastic Transcoder disables
-- automatic detection of the frame rate.
jobInput_frameRate :: Lens.Lens' JobInput (Prelude.Maybe Prelude.Text)
jobInput_frameRate = Lens.lens (\JobInput' {frameRate} -> frameRate) (\s@JobInput' {} a -> s {frameRate = a} :: JobInput)

instance Core.FromJSON JobInput where
  parseJSON =
    Core.withObject
      "JobInput"
      ( \x ->
          JobInput'
            Prelude.<$> (x Core..:? "DetectedProperties")
            Prelude.<*> (x Core..:? "Key")
            Prelude.<*> (x Core..:? "AspectRatio")
            Prelude.<*> (x Core..:? "Interlaced")
            Prelude.<*> (x Core..:? "TimeSpan")
            Prelude.<*> (x Core..:? "Container")
            Prelude.<*> (x Core..:? "Encryption")
            Prelude.<*> (x Core..:? "Resolution")
            Prelude.<*> (x Core..:? "InputCaptions")
            Prelude.<*> (x Core..:? "FrameRate")
      )

instance Prelude.Hashable JobInput where
  hashWithSalt _salt JobInput' {..} =
    _salt `Prelude.hashWithSalt` detectedProperties
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` aspectRatio
      `Prelude.hashWithSalt` interlaced
      `Prelude.hashWithSalt` timeSpan
      `Prelude.hashWithSalt` container
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` resolution
      `Prelude.hashWithSalt` inputCaptions
      `Prelude.hashWithSalt` frameRate

instance Prelude.NFData JobInput where
  rnf JobInput' {..} =
    Prelude.rnf detectedProperties
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf aspectRatio
      `Prelude.seq` Prelude.rnf interlaced
      `Prelude.seq` Prelude.rnf timeSpan
      `Prelude.seq` Prelude.rnf container
      `Prelude.seq` Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf resolution
      `Prelude.seq` Prelude.rnf inputCaptions
      `Prelude.seq` Prelude.rnf frameRate

instance Core.ToJSON JobInput where
  toJSON JobInput' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DetectedProperties" Core..=)
              Prelude.<$> detectedProperties,
            ("Key" Core..=) Prelude.<$> key,
            ("AspectRatio" Core..=) Prelude.<$> aspectRatio,
            ("Interlaced" Core..=) Prelude.<$> interlaced,
            ("TimeSpan" Core..=) Prelude.<$> timeSpan,
            ("Container" Core..=) Prelude.<$> container,
            ("Encryption" Core..=) Prelude.<$> encryption,
            ("Resolution" Core..=) Prelude.<$> resolution,
            ("InputCaptions" Core..=) Prelude.<$> inputCaptions,
            ("FrameRate" Core..=) Prelude.<$> frameRate
          ]
      )
