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
-- Module      : Amazonka.ElasticTranscoder.Types.JobOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.JobOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticTranscoder.Types.Captions
import Amazonka.ElasticTranscoder.Types.Clip
import Amazonka.ElasticTranscoder.Types.Encryption
import Amazonka.ElasticTranscoder.Types.JobAlbumArt
import Amazonka.ElasticTranscoder.Types.JobWatermark
import qualified Amazonka.Prelude as Prelude

-- | Outputs recommended instead.
--
-- If you specified one output for a job, information about that output. If
-- you specified multiple outputs for a job, the @Output@ object lists
-- information about the first output. This duplicates the information that
-- is listed for the first output in the @Outputs@ object.
--
-- /See:/ 'newJobOutput' smart constructor.
data JobOutput = JobOutput'
  { -- | The album art to be associated with the output file, if any.
    albumArt :: Prelude.Maybe JobAlbumArt,
    -- | If Elastic Transcoder used a preset with a @ColorSpaceConversionMode@ to
    -- transcode the output file, the @AppliedColorSpaceConversion@ parameter
    -- shows the conversion used. If no @ColorSpaceConversionMode@ was defined
    -- in the preset, this parameter is not be included in the job response.
    appliedColorSpaceConversion :: Prelude.Maybe Prelude.Text,
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
    captions :: Prelude.Maybe Captions,
    -- | You can create an output file that contains an excerpt from the input
    -- file. This excerpt, called a clip, can come from the beginning, middle,
    -- or end of the file. The Composition object contains settings for the
    -- clips that make up an output file. For the current release, you can only
    -- specify settings for a single clip per output file. The Composition
    -- object cannot be null.
    composition :: Prelude.Maybe [Clip],
    -- | Duration of the output file, in seconds.
    duration :: Prelude.Maybe Prelude.Integer,
    -- | Duration of the output file, in milliseconds.
    durationMillis :: Prelude.Maybe Prelude.Integer,
    -- | The encryption settings, if any, that you want Elastic Transcoder to
    -- apply to your output files. If you choose to use encryption, you must
    -- specify a mode to use. If you choose not to use encryption, Elastic
    -- Transcoder writes an unencrypted file to your Amazon S3 bucket.
    encryption :: Prelude.Maybe Encryption,
    -- | File size of the output file, in bytes.
    fileSize :: Prelude.Maybe Prelude.Integer,
    -- | Frame rate of the output file, in frames per second.
    frameRate :: Prelude.Maybe Prelude.Text,
    -- | Height of the output file, in pixels.
    height :: Prelude.Maybe Prelude.Int,
    -- | A sequential counter, starting with 1, that identifies an output among
    -- the outputs from the current job. In the Output syntax, this value is
    -- always 1.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name to assign to the transcoded file. Elastic Transcoder saves the
    -- file in the Amazon S3 bucket specified by the @OutputBucket@ object in
    -- the pipeline that is specified by the pipeline ID.
    key :: Prelude.Maybe Prelude.Text,
    -- | The value of the @Id@ object for the preset that you want to use for
    -- this job. The preset determines the audio, video, and thumbnail settings
    -- that Elastic Transcoder uses for transcoding. To use a preset that you
    -- created, specify the preset ID that Elastic Transcoder returned in the
    -- response when you created the preset. You can also use the Elastic
    -- Transcoder system presets, which you can get with @ListPresets@.
    presetId :: Prelude.Maybe Prelude.Text,
    -- | The number of degrees clockwise by which you want Elastic Transcoder to
    -- rotate the output relative to the input. Enter one of the following
    -- values:
    --
    -- @auto@, @0@, @90@, @180@, @270@
    --
    -- The value @auto@ generally works only if the file that you\'re
    -- transcoding contains rotation metadata.
    rotate :: Prelude.Maybe Prelude.Text,
    -- | (Outputs in Fragmented MP4 or MPEG-TS format only.
    --
    -- If you specify a preset in @PresetId@ for which the value of @Container@
    -- is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), @SegmentDuration@ is the
    -- target maximum duration of each segment in seconds. For @HLSv3@ format
    -- playlists, each media segment is stored in a separate @.ts@ file. For
    -- @HLSv4@, @MPEG-DASH@, and @Smooth@ playlists, all media segments for an
    -- output are stored in a single file. Each segment is approximately the
    -- length of the @SegmentDuration@, though individual segments might be
    -- shorter or longer.
    --
    -- The range of valid values is 1 to 60 seconds. If the duration of the
    -- video is not evenly divisible by @SegmentDuration@, the duration of the
    -- last segment is the remainder of total length\/SegmentDuration.
    --
    -- Elastic Transcoder creates an output-specific playlist for each output
    -- @HLS@ output that you specify in OutputKeys. To add an output to the
    -- master playlist for this job, include it in the @OutputKeys@ of the
    -- associated playlist.
    segmentDuration :: Prelude.Maybe Prelude.Text,
    -- | The status of one output in a job. If you specified only one output for
    -- the job, @Outputs:Status@ is always the same as @Job:Status@. If you
    -- specified more than one output:
    --
    -- -   @Job:Status@ and @Outputs:Status@ for all of the outputs is
    --     Submitted until Elastic Transcoder starts to process the first
    --     output.
    --
    -- -   When Elastic Transcoder starts to process the first output,
    --     @Outputs:Status@ for that output and @Job:Status@ both change to
    --     Progressing. For each output, the value of @Outputs:Status@ remains
    --     Submitted until Elastic Transcoder starts to process the output.
    --
    -- -   Job:Status remains Progressing until all of the outputs reach a
    --     terminal status, either Complete or Error.
    --
    -- -   When all of the outputs reach a terminal status, @Job:Status@
    --     changes to Complete only if @Outputs:Status@ for all of the outputs
    --     is @Complete@. If @Outputs:Status@ for one or more outputs is
    --     @Error@, the terminal status for @Job:Status@ is also @Error@.
    --
    -- The value of @Status@ is one of the following: @Submitted@,
    -- @Progressing@, @Complete@, @Canceled@, or @Error@.
    status :: Prelude.Maybe Prelude.Text,
    -- | Information that further explains @Status@.
    statusDetail :: Prelude.Maybe Prelude.Text,
    -- | The encryption settings, if any, that you want Elastic Transcoder to
    -- apply to your thumbnail.
    thumbnailEncryption :: Prelude.Maybe Encryption,
    -- | Whether you want Elastic Transcoder to create thumbnails for your videos
    -- and, if so, how you want Elastic Transcoder to name the files.
    --
    -- If you don\'t want Elastic Transcoder to create thumbnails, specify
    -- \"\".
    --
    -- If you do want Elastic Transcoder to create thumbnails, specify the
    -- information that you want to include in the file name for each
    -- thumbnail. You can specify the following values in any sequence:
    --
    -- -   __@{count}@ (Required)__: If you want to create thumbnails, you must
    --     include @{count}@ in the @ThumbnailPattern@ object. Wherever you
    --     specify @{count}@, Elastic Transcoder adds a five-digit sequence
    --     number (beginning with __00001__) to thumbnail file names. The
    --     number indicates where a given thumbnail appears in the sequence of
    --     thumbnails for a transcoded file.
    --
    --     If you specify a literal value and\/or @{resolution}@ but you omit
    --     @{count}@, Elastic Transcoder returns a validation error and does
    --     not create the job.
    --
    -- -   __Literal values (Optional)__: You can specify literal values
    --     anywhere in the @ThumbnailPattern@ object. For example, you can
    --     include them as a file name prefix or as a delimiter between
    --     @{resolution}@ and @{count}@.
    --
    -- -   __@{resolution}@ (Optional)__: If you want Elastic Transcoder to
    --     include the resolution in the file name, include @{resolution}@ in
    --     the @ThumbnailPattern@ object.
    --
    -- When creating thumbnails, Elastic Transcoder automatically saves the
    -- files in the format (.jpg or .png) that appears in the preset that you
    -- specified in the @PresetID@ value of @CreateJobOutput@. Elastic
    -- Transcoder also appends the applicable file name extension.
    thumbnailPattern :: Prelude.Maybe Prelude.Text,
    -- | Information about the watermarks that you want Elastic Transcoder to add
    -- to the video during transcoding. You can specify up to four watermarks
    -- for each output. Settings for each watermark must be defined in the
    -- preset that you specify in @Preset@ for the current output.
    --
    -- Watermarks are added to the output video in the sequence in which you
    -- list them in the job output—the first watermark in the list is added to
    -- the output video first, the second watermark in the list is added next,
    -- and so on. As a result, if the settings in a preset cause Elastic
    -- Transcoder to place all watermarks in the same location, the second
    -- watermark that you add covers the first one, the third one covers the
    -- second, and the fourth one covers the third.
    watermarks :: Prelude.Maybe [JobWatermark],
    -- | Specifies the width of the output file in pixels.
    width :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'albumArt', 'jobOutput_albumArt' - The album art to be associated with the output file, if any.
--
-- 'appliedColorSpaceConversion', 'jobOutput_appliedColorSpaceConversion' - If Elastic Transcoder used a preset with a @ColorSpaceConversionMode@ to
-- transcode the output file, the @AppliedColorSpaceConversion@ parameter
-- shows the conversion used. If no @ColorSpaceConversionMode@ was defined
-- in the preset, this parameter is not be included in the job response.
--
-- 'captions', 'jobOutput_captions' - You can configure Elastic Transcoder to transcode captions, or
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
-- 'composition', 'jobOutput_composition' - You can create an output file that contains an excerpt from the input
-- file. This excerpt, called a clip, can come from the beginning, middle,
-- or end of the file. The Composition object contains settings for the
-- clips that make up an output file. For the current release, you can only
-- specify settings for a single clip per output file. The Composition
-- object cannot be null.
--
-- 'duration', 'jobOutput_duration' - Duration of the output file, in seconds.
--
-- 'durationMillis', 'jobOutput_durationMillis' - Duration of the output file, in milliseconds.
--
-- 'encryption', 'jobOutput_encryption' - The encryption settings, if any, that you want Elastic Transcoder to
-- apply to your output files. If you choose to use encryption, you must
-- specify a mode to use. If you choose not to use encryption, Elastic
-- Transcoder writes an unencrypted file to your Amazon S3 bucket.
--
-- 'fileSize', 'jobOutput_fileSize' - File size of the output file, in bytes.
--
-- 'frameRate', 'jobOutput_frameRate' - Frame rate of the output file, in frames per second.
--
-- 'height', 'jobOutput_height' - Height of the output file, in pixels.
--
-- 'id', 'jobOutput_id' - A sequential counter, starting with 1, that identifies an output among
-- the outputs from the current job. In the Output syntax, this value is
-- always 1.
--
-- 'key', 'jobOutput_key' - The name to assign to the transcoded file. Elastic Transcoder saves the
-- file in the Amazon S3 bucket specified by the @OutputBucket@ object in
-- the pipeline that is specified by the pipeline ID.
--
-- 'presetId', 'jobOutput_presetId' - The value of the @Id@ object for the preset that you want to use for
-- this job. The preset determines the audio, video, and thumbnail settings
-- that Elastic Transcoder uses for transcoding. To use a preset that you
-- created, specify the preset ID that Elastic Transcoder returned in the
-- response when you created the preset. You can also use the Elastic
-- Transcoder system presets, which you can get with @ListPresets@.
--
-- 'rotate', 'jobOutput_rotate' - The number of degrees clockwise by which you want Elastic Transcoder to
-- rotate the output relative to the input. Enter one of the following
-- values:
--
-- @auto@, @0@, @90@, @180@, @270@
--
-- The value @auto@ generally works only if the file that you\'re
-- transcoding contains rotation metadata.
--
-- 'segmentDuration', 'jobOutput_segmentDuration' - (Outputs in Fragmented MP4 or MPEG-TS format only.
--
-- If you specify a preset in @PresetId@ for which the value of @Container@
-- is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), @SegmentDuration@ is the
-- target maximum duration of each segment in seconds. For @HLSv3@ format
-- playlists, each media segment is stored in a separate @.ts@ file. For
-- @HLSv4@, @MPEG-DASH@, and @Smooth@ playlists, all media segments for an
-- output are stored in a single file. Each segment is approximately the
-- length of the @SegmentDuration@, though individual segments might be
-- shorter or longer.
--
-- The range of valid values is 1 to 60 seconds. If the duration of the
-- video is not evenly divisible by @SegmentDuration@, the duration of the
-- last segment is the remainder of total length\/SegmentDuration.
--
-- Elastic Transcoder creates an output-specific playlist for each output
-- @HLS@ output that you specify in OutputKeys. To add an output to the
-- master playlist for this job, include it in the @OutputKeys@ of the
-- associated playlist.
--
-- 'status', 'jobOutput_status' - The status of one output in a job. If you specified only one output for
-- the job, @Outputs:Status@ is always the same as @Job:Status@. If you
-- specified more than one output:
--
-- -   @Job:Status@ and @Outputs:Status@ for all of the outputs is
--     Submitted until Elastic Transcoder starts to process the first
--     output.
--
-- -   When Elastic Transcoder starts to process the first output,
--     @Outputs:Status@ for that output and @Job:Status@ both change to
--     Progressing. For each output, the value of @Outputs:Status@ remains
--     Submitted until Elastic Transcoder starts to process the output.
--
-- -   Job:Status remains Progressing until all of the outputs reach a
--     terminal status, either Complete or Error.
--
-- -   When all of the outputs reach a terminal status, @Job:Status@
--     changes to Complete only if @Outputs:Status@ for all of the outputs
--     is @Complete@. If @Outputs:Status@ for one or more outputs is
--     @Error@, the terminal status for @Job:Status@ is also @Error@.
--
-- The value of @Status@ is one of the following: @Submitted@,
-- @Progressing@, @Complete@, @Canceled@, or @Error@.
--
-- 'statusDetail', 'jobOutput_statusDetail' - Information that further explains @Status@.
--
-- 'thumbnailEncryption', 'jobOutput_thumbnailEncryption' - The encryption settings, if any, that you want Elastic Transcoder to
-- apply to your thumbnail.
--
-- 'thumbnailPattern', 'jobOutput_thumbnailPattern' - Whether you want Elastic Transcoder to create thumbnails for your videos
-- and, if so, how you want Elastic Transcoder to name the files.
--
-- If you don\'t want Elastic Transcoder to create thumbnails, specify
-- \"\".
--
-- If you do want Elastic Transcoder to create thumbnails, specify the
-- information that you want to include in the file name for each
-- thumbnail. You can specify the following values in any sequence:
--
-- -   __@{count}@ (Required)__: If you want to create thumbnails, you must
--     include @{count}@ in the @ThumbnailPattern@ object. Wherever you
--     specify @{count}@, Elastic Transcoder adds a five-digit sequence
--     number (beginning with __00001__) to thumbnail file names. The
--     number indicates where a given thumbnail appears in the sequence of
--     thumbnails for a transcoded file.
--
--     If you specify a literal value and\/or @{resolution}@ but you omit
--     @{count}@, Elastic Transcoder returns a validation error and does
--     not create the job.
--
-- -   __Literal values (Optional)__: You can specify literal values
--     anywhere in the @ThumbnailPattern@ object. For example, you can
--     include them as a file name prefix or as a delimiter between
--     @{resolution}@ and @{count}@.
--
-- -   __@{resolution}@ (Optional)__: If you want Elastic Transcoder to
--     include the resolution in the file name, include @{resolution}@ in
--     the @ThumbnailPattern@ object.
--
-- When creating thumbnails, Elastic Transcoder automatically saves the
-- files in the format (.jpg or .png) that appears in the preset that you
-- specified in the @PresetID@ value of @CreateJobOutput@. Elastic
-- Transcoder also appends the applicable file name extension.
--
-- 'watermarks', 'jobOutput_watermarks' - Information about the watermarks that you want Elastic Transcoder to add
-- to the video during transcoding. You can specify up to four watermarks
-- for each output. Settings for each watermark must be defined in the
-- preset that you specify in @Preset@ for the current output.
--
-- Watermarks are added to the output video in the sequence in which you
-- list them in the job output—the first watermark in the list is added to
-- the output video first, the second watermark in the list is added next,
-- and so on. As a result, if the settings in a preset cause Elastic
-- Transcoder to place all watermarks in the same location, the second
-- watermark that you add covers the first one, the third one covers the
-- second, and the fourth one covers the third.
--
-- 'width', 'jobOutput_width' - Specifies the width of the output file in pixels.
newJobOutput ::
  JobOutput
newJobOutput =
  JobOutput'
    { albumArt = Prelude.Nothing,
      appliedColorSpaceConversion = Prelude.Nothing,
      captions = Prelude.Nothing,
      composition = Prelude.Nothing,
      duration = Prelude.Nothing,
      durationMillis = Prelude.Nothing,
      encryption = Prelude.Nothing,
      fileSize = Prelude.Nothing,
      frameRate = Prelude.Nothing,
      height = Prelude.Nothing,
      id = Prelude.Nothing,
      key = Prelude.Nothing,
      presetId = Prelude.Nothing,
      rotate = Prelude.Nothing,
      segmentDuration = Prelude.Nothing,
      status = Prelude.Nothing,
      statusDetail = Prelude.Nothing,
      thumbnailEncryption = Prelude.Nothing,
      thumbnailPattern = Prelude.Nothing,
      watermarks = Prelude.Nothing,
      width = Prelude.Nothing
    }

-- | The album art to be associated with the output file, if any.
jobOutput_albumArt :: Lens.Lens' JobOutput (Prelude.Maybe JobAlbumArt)
jobOutput_albumArt = Lens.lens (\JobOutput' {albumArt} -> albumArt) (\s@JobOutput' {} a -> s {albumArt = a} :: JobOutput)

-- | If Elastic Transcoder used a preset with a @ColorSpaceConversionMode@ to
-- transcode the output file, the @AppliedColorSpaceConversion@ parameter
-- shows the conversion used. If no @ColorSpaceConversionMode@ was defined
-- in the preset, this parameter is not be included in the job response.
jobOutput_appliedColorSpaceConversion :: Lens.Lens' JobOutput (Prelude.Maybe Prelude.Text)
jobOutput_appliedColorSpaceConversion = Lens.lens (\JobOutput' {appliedColorSpaceConversion} -> appliedColorSpaceConversion) (\s@JobOutput' {} a -> s {appliedColorSpaceConversion = a} :: JobOutput)

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
jobOutput_captions :: Lens.Lens' JobOutput (Prelude.Maybe Captions)
jobOutput_captions = Lens.lens (\JobOutput' {captions} -> captions) (\s@JobOutput' {} a -> s {captions = a} :: JobOutput)

-- | You can create an output file that contains an excerpt from the input
-- file. This excerpt, called a clip, can come from the beginning, middle,
-- or end of the file. The Composition object contains settings for the
-- clips that make up an output file. For the current release, you can only
-- specify settings for a single clip per output file. The Composition
-- object cannot be null.
jobOutput_composition :: Lens.Lens' JobOutput (Prelude.Maybe [Clip])
jobOutput_composition = Lens.lens (\JobOutput' {composition} -> composition) (\s@JobOutput' {} a -> s {composition = a} :: JobOutput) Prelude.. Lens.mapping Lens.coerced

-- | Duration of the output file, in seconds.
jobOutput_duration :: Lens.Lens' JobOutput (Prelude.Maybe Prelude.Integer)
jobOutput_duration = Lens.lens (\JobOutput' {duration} -> duration) (\s@JobOutput' {} a -> s {duration = a} :: JobOutput)

-- | Duration of the output file, in milliseconds.
jobOutput_durationMillis :: Lens.Lens' JobOutput (Prelude.Maybe Prelude.Integer)
jobOutput_durationMillis = Lens.lens (\JobOutput' {durationMillis} -> durationMillis) (\s@JobOutput' {} a -> s {durationMillis = a} :: JobOutput)

-- | The encryption settings, if any, that you want Elastic Transcoder to
-- apply to your output files. If you choose to use encryption, you must
-- specify a mode to use. If you choose not to use encryption, Elastic
-- Transcoder writes an unencrypted file to your Amazon S3 bucket.
jobOutput_encryption :: Lens.Lens' JobOutput (Prelude.Maybe Encryption)
jobOutput_encryption = Lens.lens (\JobOutput' {encryption} -> encryption) (\s@JobOutput' {} a -> s {encryption = a} :: JobOutput)

-- | File size of the output file, in bytes.
jobOutput_fileSize :: Lens.Lens' JobOutput (Prelude.Maybe Prelude.Integer)
jobOutput_fileSize = Lens.lens (\JobOutput' {fileSize} -> fileSize) (\s@JobOutput' {} a -> s {fileSize = a} :: JobOutput)

-- | Frame rate of the output file, in frames per second.
jobOutput_frameRate :: Lens.Lens' JobOutput (Prelude.Maybe Prelude.Text)
jobOutput_frameRate = Lens.lens (\JobOutput' {frameRate} -> frameRate) (\s@JobOutput' {} a -> s {frameRate = a} :: JobOutput)

-- | Height of the output file, in pixels.
jobOutput_height :: Lens.Lens' JobOutput (Prelude.Maybe Prelude.Int)
jobOutput_height = Lens.lens (\JobOutput' {height} -> height) (\s@JobOutput' {} a -> s {height = a} :: JobOutput)

-- | A sequential counter, starting with 1, that identifies an output among
-- the outputs from the current job. In the Output syntax, this value is
-- always 1.
jobOutput_id :: Lens.Lens' JobOutput (Prelude.Maybe Prelude.Text)
jobOutput_id = Lens.lens (\JobOutput' {id} -> id) (\s@JobOutput' {} a -> s {id = a} :: JobOutput)

-- | The name to assign to the transcoded file. Elastic Transcoder saves the
-- file in the Amazon S3 bucket specified by the @OutputBucket@ object in
-- the pipeline that is specified by the pipeline ID.
jobOutput_key :: Lens.Lens' JobOutput (Prelude.Maybe Prelude.Text)
jobOutput_key = Lens.lens (\JobOutput' {key} -> key) (\s@JobOutput' {} a -> s {key = a} :: JobOutput)

-- | The value of the @Id@ object for the preset that you want to use for
-- this job. The preset determines the audio, video, and thumbnail settings
-- that Elastic Transcoder uses for transcoding. To use a preset that you
-- created, specify the preset ID that Elastic Transcoder returned in the
-- response when you created the preset. You can also use the Elastic
-- Transcoder system presets, which you can get with @ListPresets@.
jobOutput_presetId :: Lens.Lens' JobOutput (Prelude.Maybe Prelude.Text)
jobOutput_presetId = Lens.lens (\JobOutput' {presetId} -> presetId) (\s@JobOutput' {} a -> s {presetId = a} :: JobOutput)

-- | The number of degrees clockwise by which you want Elastic Transcoder to
-- rotate the output relative to the input. Enter one of the following
-- values:
--
-- @auto@, @0@, @90@, @180@, @270@
--
-- The value @auto@ generally works only if the file that you\'re
-- transcoding contains rotation metadata.
jobOutput_rotate :: Lens.Lens' JobOutput (Prelude.Maybe Prelude.Text)
jobOutput_rotate = Lens.lens (\JobOutput' {rotate} -> rotate) (\s@JobOutput' {} a -> s {rotate = a} :: JobOutput)

-- | (Outputs in Fragmented MP4 or MPEG-TS format only.
--
-- If you specify a preset in @PresetId@ for which the value of @Container@
-- is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), @SegmentDuration@ is the
-- target maximum duration of each segment in seconds. For @HLSv3@ format
-- playlists, each media segment is stored in a separate @.ts@ file. For
-- @HLSv4@, @MPEG-DASH@, and @Smooth@ playlists, all media segments for an
-- output are stored in a single file. Each segment is approximately the
-- length of the @SegmentDuration@, though individual segments might be
-- shorter or longer.
--
-- The range of valid values is 1 to 60 seconds. If the duration of the
-- video is not evenly divisible by @SegmentDuration@, the duration of the
-- last segment is the remainder of total length\/SegmentDuration.
--
-- Elastic Transcoder creates an output-specific playlist for each output
-- @HLS@ output that you specify in OutputKeys. To add an output to the
-- master playlist for this job, include it in the @OutputKeys@ of the
-- associated playlist.
jobOutput_segmentDuration :: Lens.Lens' JobOutput (Prelude.Maybe Prelude.Text)
jobOutput_segmentDuration = Lens.lens (\JobOutput' {segmentDuration} -> segmentDuration) (\s@JobOutput' {} a -> s {segmentDuration = a} :: JobOutput)

-- | The status of one output in a job. If you specified only one output for
-- the job, @Outputs:Status@ is always the same as @Job:Status@. If you
-- specified more than one output:
--
-- -   @Job:Status@ and @Outputs:Status@ for all of the outputs is
--     Submitted until Elastic Transcoder starts to process the first
--     output.
--
-- -   When Elastic Transcoder starts to process the first output,
--     @Outputs:Status@ for that output and @Job:Status@ both change to
--     Progressing. For each output, the value of @Outputs:Status@ remains
--     Submitted until Elastic Transcoder starts to process the output.
--
-- -   Job:Status remains Progressing until all of the outputs reach a
--     terminal status, either Complete or Error.
--
-- -   When all of the outputs reach a terminal status, @Job:Status@
--     changes to Complete only if @Outputs:Status@ for all of the outputs
--     is @Complete@. If @Outputs:Status@ for one or more outputs is
--     @Error@, the terminal status for @Job:Status@ is also @Error@.
--
-- The value of @Status@ is one of the following: @Submitted@,
-- @Progressing@, @Complete@, @Canceled@, or @Error@.
jobOutput_status :: Lens.Lens' JobOutput (Prelude.Maybe Prelude.Text)
jobOutput_status = Lens.lens (\JobOutput' {status} -> status) (\s@JobOutput' {} a -> s {status = a} :: JobOutput)

-- | Information that further explains @Status@.
jobOutput_statusDetail :: Lens.Lens' JobOutput (Prelude.Maybe Prelude.Text)
jobOutput_statusDetail = Lens.lens (\JobOutput' {statusDetail} -> statusDetail) (\s@JobOutput' {} a -> s {statusDetail = a} :: JobOutput)

-- | The encryption settings, if any, that you want Elastic Transcoder to
-- apply to your thumbnail.
jobOutput_thumbnailEncryption :: Lens.Lens' JobOutput (Prelude.Maybe Encryption)
jobOutput_thumbnailEncryption = Lens.lens (\JobOutput' {thumbnailEncryption} -> thumbnailEncryption) (\s@JobOutput' {} a -> s {thumbnailEncryption = a} :: JobOutput)

-- | Whether you want Elastic Transcoder to create thumbnails for your videos
-- and, if so, how you want Elastic Transcoder to name the files.
--
-- If you don\'t want Elastic Transcoder to create thumbnails, specify
-- \"\".
--
-- If you do want Elastic Transcoder to create thumbnails, specify the
-- information that you want to include in the file name for each
-- thumbnail. You can specify the following values in any sequence:
--
-- -   __@{count}@ (Required)__: If you want to create thumbnails, you must
--     include @{count}@ in the @ThumbnailPattern@ object. Wherever you
--     specify @{count}@, Elastic Transcoder adds a five-digit sequence
--     number (beginning with __00001__) to thumbnail file names. The
--     number indicates where a given thumbnail appears in the sequence of
--     thumbnails for a transcoded file.
--
--     If you specify a literal value and\/or @{resolution}@ but you omit
--     @{count}@, Elastic Transcoder returns a validation error and does
--     not create the job.
--
-- -   __Literal values (Optional)__: You can specify literal values
--     anywhere in the @ThumbnailPattern@ object. For example, you can
--     include them as a file name prefix or as a delimiter between
--     @{resolution}@ and @{count}@.
--
-- -   __@{resolution}@ (Optional)__: If you want Elastic Transcoder to
--     include the resolution in the file name, include @{resolution}@ in
--     the @ThumbnailPattern@ object.
--
-- When creating thumbnails, Elastic Transcoder automatically saves the
-- files in the format (.jpg or .png) that appears in the preset that you
-- specified in the @PresetID@ value of @CreateJobOutput@. Elastic
-- Transcoder also appends the applicable file name extension.
jobOutput_thumbnailPattern :: Lens.Lens' JobOutput (Prelude.Maybe Prelude.Text)
jobOutput_thumbnailPattern = Lens.lens (\JobOutput' {thumbnailPattern} -> thumbnailPattern) (\s@JobOutput' {} a -> s {thumbnailPattern = a} :: JobOutput)

-- | Information about the watermarks that you want Elastic Transcoder to add
-- to the video during transcoding. You can specify up to four watermarks
-- for each output. Settings for each watermark must be defined in the
-- preset that you specify in @Preset@ for the current output.
--
-- Watermarks are added to the output video in the sequence in which you
-- list them in the job output—the first watermark in the list is added to
-- the output video first, the second watermark in the list is added next,
-- and so on. As a result, if the settings in a preset cause Elastic
-- Transcoder to place all watermarks in the same location, the second
-- watermark that you add covers the first one, the third one covers the
-- second, and the fourth one covers the third.
jobOutput_watermarks :: Lens.Lens' JobOutput (Prelude.Maybe [JobWatermark])
jobOutput_watermarks = Lens.lens (\JobOutput' {watermarks} -> watermarks) (\s@JobOutput' {} a -> s {watermarks = a} :: JobOutput) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the width of the output file in pixels.
jobOutput_width :: Lens.Lens' JobOutput (Prelude.Maybe Prelude.Int)
jobOutput_width = Lens.lens (\JobOutput' {width} -> width) (\s@JobOutput' {} a -> s {width = a} :: JobOutput)

instance Data.FromJSON JobOutput where
  parseJSON =
    Data.withObject
      "JobOutput"
      ( \x ->
          JobOutput'
            Prelude.<$> (x Data..:? "AlbumArt")
            Prelude.<*> (x Data..:? "AppliedColorSpaceConversion")
            Prelude.<*> (x Data..:? "Captions")
            Prelude.<*> (x Data..:? "Composition" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Duration")
            Prelude.<*> (x Data..:? "DurationMillis")
            Prelude.<*> (x Data..:? "Encryption")
            Prelude.<*> (x Data..:? "FileSize")
            Prelude.<*> (x Data..:? "FrameRate")
            Prelude.<*> (x Data..:? "Height")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Key")
            Prelude.<*> (x Data..:? "PresetId")
            Prelude.<*> (x Data..:? "Rotate")
            Prelude.<*> (x Data..:? "SegmentDuration")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusDetail")
            Prelude.<*> (x Data..:? "ThumbnailEncryption")
            Prelude.<*> (x Data..:? "ThumbnailPattern")
            Prelude.<*> (x Data..:? "Watermarks" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Width")
      )

instance Prelude.Hashable JobOutput where
  hashWithSalt _salt JobOutput' {..} =
    _salt
      `Prelude.hashWithSalt` albumArt
      `Prelude.hashWithSalt` appliedColorSpaceConversion
      `Prelude.hashWithSalt` captions
      `Prelude.hashWithSalt` composition
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` durationMillis
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` fileSize
      `Prelude.hashWithSalt` frameRate
      `Prelude.hashWithSalt` height
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` presetId
      `Prelude.hashWithSalt` rotate
      `Prelude.hashWithSalt` segmentDuration
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusDetail
      `Prelude.hashWithSalt` thumbnailEncryption
      `Prelude.hashWithSalt` thumbnailPattern
      `Prelude.hashWithSalt` watermarks
      `Prelude.hashWithSalt` width

instance Prelude.NFData JobOutput where
  rnf JobOutput' {..} =
    Prelude.rnf albumArt
      `Prelude.seq` Prelude.rnf appliedColorSpaceConversion
      `Prelude.seq` Prelude.rnf captions
      `Prelude.seq` Prelude.rnf composition
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf durationMillis
      `Prelude.seq` Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf fileSize
      `Prelude.seq` Prelude.rnf frameRate
      `Prelude.seq` Prelude.rnf height
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf presetId
      `Prelude.seq` Prelude.rnf rotate
      `Prelude.seq` Prelude.rnf segmentDuration
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusDetail
      `Prelude.seq` Prelude.rnf thumbnailEncryption
      `Prelude.seq` Prelude.rnf thumbnailPattern
      `Prelude.seq` Prelude.rnf watermarks
      `Prelude.seq` Prelude.rnf width
