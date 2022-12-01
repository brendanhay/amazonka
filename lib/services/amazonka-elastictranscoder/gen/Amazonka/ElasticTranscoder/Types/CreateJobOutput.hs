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
-- Module      : Amazonka.ElasticTranscoder.Types.CreateJobOutput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.CreateJobOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticTranscoder.Types.Captions
import Amazonka.ElasticTranscoder.Types.Clip
import Amazonka.ElasticTranscoder.Types.Encryption
import Amazonka.ElasticTranscoder.Types.JobAlbumArt
import Amazonka.ElasticTranscoder.Types.JobWatermark
import qualified Amazonka.Prelude as Prelude

-- | The @CreateJobOutput@ structure.
--
-- /See:/ 'newCreateJobOutput' smart constructor.
data CreateJobOutput = CreateJobOutput'
  { -- | The name to assign to the transcoded file. Elastic Transcoder saves the
    -- file in the Amazon S3 bucket specified by the @OutputBucket@ object in
    -- the pipeline that is specified by the pipeline ID. If a file with the
    -- specified name already exists in the output bucket, the job fails.
    key :: Prelude.Maybe Prelude.Text,
    -- | Information about the watermarks that you want Elastic Transcoder to add
    -- to the video during transcoding. You can specify up to four watermarks
    -- for each output. Settings for each watermark must be defined in the
    -- preset for the current output.
    watermarks :: Prelude.Maybe [JobWatermark],
    -- | You can create an output file that contains an excerpt from the input
    -- file. This excerpt, called a clip, can come from the beginning, middle,
    -- or end of the file. The Composition object contains settings for the
    -- clips that make up an output file. For the current release, you can only
    -- specify settings for a single clip per output file. The Composition
    -- object cannot be null.
    composition :: Prelude.Maybe [Clip],
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
    -- | Information about the album art that you want Elastic Transcoder to add
    -- to the file during transcoding. You can specify up to twenty album
    -- artworks for each output. Settings for each artwork must be defined in
    -- the job for the current output.
    albumArt :: Prelude.Maybe JobAlbumArt,
    -- | The @Id@ of the preset to use for this job. The preset determines the
    -- audio, video, and thumbnail settings that Elastic Transcoder uses for
    -- transcoding.
    presetId :: Prelude.Maybe Prelude.Text,
    -- | You can specify encryption settings for any output files that you want
    -- to use for a transcoding job. This includes the output file and any
    -- watermarks, thumbnails, album art, or captions that you want to use. You
    -- must specify encryption settings for each file individually.
    encryption :: Prelude.Maybe Encryption,
    -- | The number of degrees clockwise by which you want Elastic Transcoder to
    -- rotate the output relative to the input. Enter one of the following
    -- values: @auto@, @0@, @90@, @180@, @270@. The value @auto@ generally
    -- works only if the file that you\'re transcoding contains rotation
    -- metadata.
    rotate :: Prelude.Maybe Prelude.Text,
    -- | (Outputs in Fragmented MP4 or MPEG-TS format only.
    --
    -- If you specify a preset in @PresetId@ for which the value of @Container@
    -- is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), @SegmentDuration@ is the
    -- target maximum duration of each segment in seconds. For @HLSv3@ format
    -- playlists, each media segment is stored in a separate @.ts@ file. For
    -- @HLSv4@ and @Smooth@ playlists, all media segments for an output are
    -- stored in a single file. Each segment is approximately the length of the
    -- @SegmentDuration@, though individual segments might be shorter or
    -- longer.
    --
    -- The range of valid values is 1 to 60 seconds. If the duration of the
    -- video is not evenly divisible by @SegmentDuration@, the duration of the
    -- last segment is the remainder of total length\/SegmentDuration.
    --
    -- Elastic Transcoder creates an output-specific playlist for each output
    -- @HLS@ output that you specify in OutputKeys. To add an output to the
    -- master playlist for this job, include it in the @OutputKeys@ of the
    -- associated playlist.
    segmentDuration :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateJobOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'createJobOutput_key' - The name to assign to the transcoded file. Elastic Transcoder saves the
-- file in the Amazon S3 bucket specified by the @OutputBucket@ object in
-- the pipeline that is specified by the pipeline ID. If a file with the
-- specified name already exists in the output bucket, the job fails.
--
-- 'watermarks', 'createJobOutput_watermarks' - Information about the watermarks that you want Elastic Transcoder to add
-- to the video during transcoding. You can specify up to four watermarks
-- for each output. Settings for each watermark must be defined in the
-- preset for the current output.
--
-- 'composition', 'createJobOutput_composition' - You can create an output file that contains an excerpt from the input
-- file. This excerpt, called a clip, can come from the beginning, middle,
-- or end of the file. The Composition object contains settings for the
-- clips that make up an output file. For the current release, you can only
-- specify settings for a single clip per output file. The Composition
-- object cannot be null.
--
-- 'thumbnailEncryption', 'createJobOutput_thumbnailEncryption' - The encryption settings, if any, that you want Elastic Transcoder to
-- apply to your thumbnail.
--
-- 'thumbnailPattern', 'createJobOutput_thumbnailPattern' - Whether you want Elastic Transcoder to create thumbnails for your videos
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
-- 'captions', 'createJobOutput_captions' - You can configure Elastic Transcoder to transcode captions, or
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
-- 'albumArt', 'createJobOutput_albumArt' - Information about the album art that you want Elastic Transcoder to add
-- to the file during transcoding. You can specify up to twenty album
-- artworks for each output. Settings for each artwork must be defined in
-- the job for the current output.
--
-- 'presetId', 'createJobOutput_presetId' - The @Id@ of the preset to use for this job. The preset determines the
-- audio, video, and thumbnail settings that Elastic Transcoder uses for
-- transcoding.
--
-- 'encryption', 'createJobOutput_encryption' - You can specify encryption settings for any output files that you want
-- to use for a transcoding job. This includes the output file and any
-- watermarks, thumbnails, album art, or captions that you want to use. You
-- must specify encryption settings for each file individually.
--
-- 'rotate', 'createJobOutput_rotate' - The number of degrees clockwise by which you want Elastic Transcoder to
-- rotate the output relative to the input. Enter one of the following
-- values: @auto@, @0@, @90@, @180@, @270@. The value @auto@ generally
-- works only if the file that you\'re transcoding contains rotation
-- metadata.
--
-- 'segmentDuration', 'createJobOutput_segmentDuration' - (Outputs in Fragmented MP4 or MPEG-TS format only.
--
-- If you specify a preset in @PresetId@ for which the value of @Container@
-- is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), @SegmentDuration@ is the
-- target maximum duration of each segment in seconds. For @HLSv3@ format
-- playlists, each media segment is stored in a separate @.ts@ file. For
-- @HLSv4@ and @Smooth@ playlists, all media segments for an output are
-- stored in a single file. Each segment is approximately the length of the
-- @SegmentDuration@, though individual segments might be shorter or
-- longer.
--
-- The range of valid values is 1 to 60 seconds. If the duration of the
-- video is not evenly divisible by @SegmentDuration@, the duration of the
-- last segment is the remainder of total length\/SegmentDuration.
--
-- Elastic Transcoder creates an output-specific playlist for each output
-- @HLS@ output that you specify in OutputKeys. To add an output to the
-- master playlist for this job, include it in the @OutputKeys@ of the
-- associated playlist.
newCreateJobOutput ::
  CreateJobOutput
newCreateJobOutput =
  CreateJobOutput'
    { key = Prelude.Nothing,
      watermarks = Prelude.Nothing,
      composition = Prelude.Nothing,
      thumbnailEncryption = Prelude.Nothing,
      thumbnailPattern = Prelude.Nothing,
      captions = Prelude.Nothing,
      albumArt = Prelude.Nothing,
      presetId = Prelude.Nothing,
      encryption = Prelude.Nothing,
      rotate = Prelude.Nothing,
      segmentDuration = Prelude.Nothing
    }

-- | The name to assign to the transcoded file. Elastic Transcoder saves the
-- file in the Amazon S3 bucket specified by the @OutputBucket@ object in
-- the pipeline that is specified by the pipeline ID. If a file with the
-- specified name already exists in the output bucket, the job fails.
createJobOutput_key :: Lens.Lens' CreateJobOutput (Prelude.Maybe Prelude.Text)
createJobOutput_key = Lens.lens (\CreateJobOutput' {key} -> key) (\s@CreateJobOutput' {} a -> s {key = a} :: CreateJobOutput)

-- | Information about the watermarks that you want Elastic Transcoder to add
-- to the video during transcoding. You can specify up to four watermarks
-- for each output. Settings for each watermark must be defined in the
-- preset for the current output.
createJobOutput_watermarks :: Lens.Lens' CreateJobOutput (Prelude.Maybe [JobWatermark])
createJobOutput_watermarks = Lens.lens (\CreateJobOutput' {watermarks} -> watermarks) (\s@CreateJobOutput' {} a -> s {watermarks = a} :: CreateJobOutput) Prelude.. Lens.mapping Lens.coerced

-- | You can create an output file that contains an excerpt from the input
-- file. This excerpt, called a clip, can come from the beginning, middle,
-- or end of the file. The Composition object contains settings for the
-- clips that make up an output file. For the current release, you can only
-- specify settings for a single clip per output file. The Composition
-- object cannot be null.
createJobOutput_composition :: Lens.Lens' CreateJobOutput (Prelude.Maybe [Clip])
createJobOutput_composition = Lens.lens (\CreateJobOutput' {composition} -> composition) (\s@CreateJobOutput' {} a -> s {composition = a} :: CreateJobOutput) Prelude.. Lens.mapping Lens.coerced

-- | The encryption settings, if any, that you want Elastic Transcoder to
-- apply to your thumbnail.
createJobOutput_thumbnailEncryption :: Lens.Lens' CreateJobOutput (Prelude.Maybe Encryption)
createJobOutput_thumbnailEncryption = Lens.lens (\CreateJobOutput' {thumbnailEncryption} -> thumbnailEncryption) (\s@CreateJobOutput' {} a -> s {thumbnailEncryption = a} :: CreateJobOutput)

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
createJobOutput_thumbnailPattern :: Lens.Lens' CreateJobOutput (Prelude.Maybe Prelude.Text)
createJobOutput_thumbnailPattern = Lens.lens (\CreateJobOutput' {thumbnailPattern} -> thumbnailPattern) (\s@CreateJobOutput' {} a -> s {thumbnailPattern = a} :: CreateJobOutput)

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
createJobOutput_captions :: Lens.Lens' CreateJobOutput (Prelude.Maybe Captions)
createJobOutput_captions = Lens.lens (\CreateJobOutput' {captions} -> captions) (\s@CreateJobOutput' {} a -> s {captions = a} :: CreateJobOutput)

-- | Information about the album art that you want Elastic Transcoder to add
-- to the file during transcoding. You can specify up to twenty album
-- artworks for each output. Settings for each artwork must be defined in
-- the job for the current output.
createJobOutput_albumArt :: Lens.Lens' CreateJobOutput (Prelude.Maybe JobAlbumArt)
createJobOutput_albumArt = Lens.lens (\CreateJobOutput' {albumArt} -> albumArt) (\s@CreateJobOutput' {} a -> s {albumArt = a} :: CreateJobOutput)

-- | The @Id@ of the preset to use for this job. The preset determines the
-- audio, video, and thumbnail settings that Elastic Transcoder uses for
-- transcoding.
createJobOutput_presetId :: Lens.Lens' CreateJobOutput (Prelude.Maybe Prelude.Text)
createJobOutput_presetId = Lens.lens (\CreateJobOutput' {presetId} -> presetId) (\s@CreateJobOutput' {} a -> s {presetId = a} :: CreateJobOutput)

-- | You can specify encryption settings for any output files that you want
-- to use for a transcoding job. This includes the output file and any
-- watermarks, thumbnails, album art, or captions that you want to use. You
-- must specify encryption settings for each file individually.
createJobOutput_encryption :: Lens.Lens' CreateJobOutput (Prelude.Maybe Encryption)
createJobOutput_encryption = Lens.lens (\CreateJobOutput' {encryption} -> encryption) (\s@CreateJobOutput' {} a -> s {encryption = a} :: CreateJobOutput)

-- | The number of degrees clockwise by which you want Elastic Transcoder to
-- rotate the output relative to the input. Enter one of the following
-- values: @auto@, @0@, @90@, @180@, @270@. The value @auto@ generally
-- works only if the file that you\'re transcoding contains rotation
-- metadata.
createJobOutput_rotate :: Lens.Lens' CreateJobOutput (Prelude.Maybe Prelude.Text)
createJobOutput_rotate = Lens.lens (\CreateJobOutput' {rotate} -> rotate) (\s@CreateJobOutput' {} a -> s {rotate = a} :: CreateJobOutput)

-- | (Outputs in Fragmented MP4 or MPEG-TS format only.
--
-- If you specify a preset in @PresetId@ for which the value of @Container@
-- is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), @SegmentDuration@ is the
-- target maximum duration of each segment in seconds. For @HLSv3@ format
-- playlists, each media segment is stored in a separate @.ts@ file. For
-- @HLSv4@ and @Smooth@ playlists, all media segments for an output are
-- stored in a single file. Each segment is approximately the length of the
-- @SegmentDuration@, though individual segments might be shorter or
-- longer.
--
-- The range of valid values is 1 to 60 seconds. If the duration of the
-- video is not evenly divisible by @SegmentDuration@, the duration of the
-- last segment is the remainder of total length\/SegmentDuration.
--
-- Elastic Transcoder creates an output-specific playlist for each output
-- @HLS@ output that you specify in OutputKeys. To add an output to the
-- master playlist for this job, include it in the @OutputKeys@ of the
-- associated playlist.
createJobOutput_segmentDuration :: Lens.Lens' CreateJobOutput (Prelude.Maybe Prelude.Text)
createJobOutput_segmentDuration = Lens.lens (\CreateJobOutput' {segmentDuration} -> segmentDuration) (\s@CreateJobOutput' {} a -> s {segmentDuration = a} :: CreateJobOutput)

instance Prelude.Hashable CreateJobOutput where
  hashWithSalt _salt CreateJobOutput' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` watermarks
      `Prelude.hashWithSalt` composition
      `Prelude.hashWithSalt` thumbnailEncryption
      `Prelude.hashWithSalt` thumbnailPattern
      `Prelude.hashWithSalt` captions
      `Prelude.hashWithSalt` albumArt
      `Prelude.hashWithSalt` presetId
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` rotate
      `Prelude.hashWithSalt` segmentDuration

instance Prelude.NFData CreateJobOutput where
  rnf CreateJobOutput' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf watermarks
      `Prelude.seq` Prelude.rnf composition
      `Prelude.seq` Prelude.rnf thumbnailEncryption
      `Prelude.seq` Prelude.rnf thumbnailPattern
      `Prelude.seq` Prelude.rnf captions
      `Prelude.seq` Prelude.rnf albumArt
      `Prelude.seq` Prelude.rnf presetId
      `Prelude.seq` Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf rotate
      `Prelude.seq` Prelude.rnf segmentDuration

instance Core.ToJSON CreateJobOutput where
  toJSON CreateJobOutput' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Key" Core..=) Prelude.<$> key,
            ("Watermarks" Core..=) Prelude.<$> watermarks,
            ("Composition" Core..=) Prelude.<$> composition,
            ("ThumbnailEncryption" Core..=)
              Prelude.<$> thumbnailEncryption,
            ("ThumbnailPattern" Core..=)
              Prelude.<$> thumbnailPattern,
            ("Captions" Core..=) Prelude.<$> captions,
            ("AlbumArt" Core..=) Prelude.<$> albumArt,
            ("PresetId" Core..=) Prelude.<$> presetId,
            ("Encryption" Core..=) Prelude.<$> encryption,
            ("Rotate" Core..=) Prelude.<$> rotate,
            ("SegmentDuration" Core..=)
              Prelude.<$> segmentDuration
          ]
      )
