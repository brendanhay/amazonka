{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.JobOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.JobOutput
  ( JobOutput (..),

    -- * Smart constructor
    mkJobOutput,

    -- * Lenses
    joAppliedColorSpaceConversion,
    joThumbnailPattern,
    joStatus,
    joHeight,
    joFrameRate,
    joCaptions,
    joPresetId,
    joComposition,
    joAlbumArt,
    joFileSize,
    joWatermarks,
    joWidth,
    joEncryption,
    joKey,
    joStatusDetail,
    joId,
    joSegmentDuration,
    joDurationMillis,
    joThumbnailEncryption,
    joDuration,
    joRotate,
  )
where

import Network.AWS.ElasticTranscoder.Types.Captions
import Network.AWS.ElasticTranscoder.Types.Clip
import Network.AWS.ElasticTranscoder.Types.Encryption
import Network.AWS.ElasticTranscoder.Types.JobAlbumArt
import Network.AWS.ElasticTranscoder.Types.JobWatermark
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /Important:/ Outputs recommended instead.
--
-- If you specified one output for a job, information about that output. If you specified multiple outputs for a job, the @Output@ object lists information about the first output. This duplicates the information that is listed for the first output in the @Outputs@ object.
--
-- /See:/ 'mkJobOutput' smart constructor.
data JobOutput = JobOutput'
  { -- | If Elastic Transcoder used a preset with a @ColorSpaceConversionMode@ to transcode the output file, the @AppliedColorSpaceConversion@ parameter shows the conversion used. If no @ColorSpaceConversionMode@ was defined in the preset, this parameter is not be included in the job response.
    appliedColorSpaceConversion :: Lude.Maybe Lude.Text,
    -- | Whether you want Elastic Transcoder to create thumbnails for your videos and, if so, how you want Elastic Transcoder to name the files.
    --
    -- If you don't want Elastic Transcoder to create thumbnails, specify "".
    -- If you do want Elastic Transcoder to create thumbnails, specify the information that you want to include in the file name for each thumbnail. You can specify the following values in any sequence:
    --
    --     * __@{count}@ (Required)__ : If you want to create thumbnails, you must include @{count}@ in the @ThumbnailPattern@ object. Wherever you specify @{count}@ , Elastic Transcoder adds a five-digit sequence number (beginning with __00001__ ) to thumbnail file names. The number indicates where a given thumbnail appears in the sequence of thumbnails for a transcoded file.
    -- /Important:/ If you specify a literal value and/or @{resolution}@ but you omit @{count}@ , Elastic Transcoder returns a validation error and does not create the job.
    --
    --
    --     * __Literal values (Optional)__ : You can specify literal values anywhere in the @ThumbnailPattern@ object. For example, you can include them as a file name prefix or as a delimiter between @{resolution}@ and @{count}@ .
    --
    --
    --     * __@{resolution}@ (Optional)__ : If you want Elastic Transcoder to include the resolution in the file name, include @{resolution}@ in the @ThumbnailPattern@ object.
    --
    --
    -- When creating thumbnails, Elastic Transcoder automatically saves the files in the format (.jpg or .png) that appears in the preset that you specified in the @PresetID@ value of @CreateJobOutput@ . Elastic Transcoder also appends the applicable file name extension.
    thumbnailPattern :: Lude.Maybe Lude.Text,
    -- | The status of one output in a job. If you specified only one output for the job, @Outputs:Status@ is always the same as @Job:Status@ . If you specified more than one output:
    --
    --
    --     * @Job:Status@ and @Outputs:Status@ for all of the outputs is Submitted until Elastic Transcoder starts to process the first output.
    --
    --
    --     * When Elastic Transcoder starts to process the first output, @Outputs:Status@ for that output and @Job:Status@ both change to Progressing. For each output, the value of @Outputs:Status@ remains Submitted until Elastic Transcoder starts to process the output.
    --
    --
    --     * Job:Status remains Progressing until all of the outputs reach a terminal status, either Complete or Error.
    --
    --
    --     * When all of the outputs reach a terminal status, @Job:Status@ changes to Complete only if @Outputs:Status@ for all of the outputs is @Complete@ . If @Outputs:Status@ for one or more outputs is @Error@ , the terminal status for @Job:Status@ is also @Error@ .
    --
    --
    -- The value of @Status@ is one of the following: @Submitted@ , @Progressing@ , @Complete@ , @Canceled@ , or @Error@ .
    status :: Lude.Maybe Lude.Text,
    -- | Height of the output file, in pixels.
    height :: Lude.Maybe Lude.Int,
    -- | Frame rate of the output file, in frames per second.
    frameRate :: Lude.Maybe Lude.Text,
    -- | You can configure Elastic Transcoder to transcode captions, or subtitles, from one format to another. All captions must be in UTF-8. Elastic Transcoder supports two types of captions:
    --
    --
    --     * __Embedded:__ Embedded captions are included in the same file as the audio and video. Elastic Transcoder supports only one embedded caption per language, to a maximum of 300 embedded captions per file.
    -- Valid input values include: @CEA-608 (EIA-608@ , first non-empty channel only), @CEA-708 (EIA-708@ , first non-empty channel only), and @mov-text@
    -- Valid outputs include: @mov-text@
    -- Elastic Transcoder supports a maximum of one embedded format per output.
    --
    --
    --     * __Sidecar:__ Sidecar captions are kept in a separate metadata file from the audio and video data. Sidecar captions require a player that is capable of understanding the relationship between the video file and the sidecar file. Elastic Transcoder supports only one sidecar caption per language, to a maximum of 20 sidecar captions per file.
    -- Valid input values include: @dfxp@ (first div element only), @ebu-tt@ , @scc@ , @smpt@ , @srt@ , @ttml@ (first div element only), and @webvtt@
    -- Valid outputs include: @dfxp@ (first div element only), @scc@ , @srt@ , and @webvtt@ .
    --
    --
    -- If you want ttml or smpte-tt compatible captions, specify dfxp as your output format.
    -- Elastic Transcoder does not support OCR (Optical Character Recognition), does not accept pictures as a valid input for captions, and is not available for audio-only transcoding. Elastic Transcoder does not preserve text formatting (for example, italics) during the transcoding process.
    -- To remove captions or leave the captions empty, set @Captions@ to null. To pass through existing captions unchanged, set the @MergePolicy@ to @MergeRetain@ , and pass in a null @CaptionSources@ array.
    -- For more information on embedded files, see the Subtitles Wikipedia page.
    -- For more information on sidecar files, see the Extensible Metadata Platform and Sidecar file Wikipedia pages.
    captions :: Lude.Maybe Captions,
    -- | The value of the @Id@ object for the preset that you want to use for this job. The preset determines the audio, video, and thumbnail settings that Elastic Transcoder uses for transcoding. To use a preset that you created, specify the preset ID that Elastic Transcoder returned in the response when you created the preset. You can also use the Elastic Transcoder system presets, which you can get with @ListPresets@ .
    presetId :: Lude.Maybe Lude.Text,
    -- | You can create an output file that contains an excerpt from the input file. This excerpt, called a clip, can come from the beginning, middle, or end of the file. The Composition object contains settings for the clips that make up an output file. For the current release, you can only specify settings for a single clip per output file. The Composition object cannot be null.
    composition :: Lude.Maybe [Clip],
    -- | The album art to be associated with the output file, if any.
    albumArt :: Lude.Maybe JobAlbumArt,
    -- | File size of the output file, in bytes.
    fileSize :: Lude.Maybe Lude.Integer,
    -- | Information about the watermarks that you want Elastic Transcoder to add to the video during transcoding. You can specify up to four watermarks for each output. Settings for each watermark must be defined in the preset that you specify in @Preset@ for the current output.
    --
    -- Watermarks are added to the output video in the sequence in which you list them in the job output—the first watermark in the list is added to the output video first, the second watermark in the list is added next, and so on. As a result, if the settings in a preset cause Elastic Transcoder to place all watermarks in the same location, the second watermark that you add covers the first one, the third one covers the second, and the fourth one covers the third.
    watermarks :: Lude.Maybe [JobWatermark],
    -- | Specifies the width of the output file in pixels.
    width :: Lude.Maybe Lude.Int,
    -- | The encryption settings, if any, that you want Elastic Transcoder to apply to your output files. If you choose to use encryption, you must specify a mode to use. If you choose not to use encryption, Elastic Transcoder writes an unencrypted file to your Amazon S3 bucket.
    encryption :: Lude.Maybe Encryption,
    -- | The name to assign to the transcoded file. Elastic Transcoder saves the file in the Amazon S3 bucket specified by the @OutputBucket@ object in the pipeline that is specified by the pipeline ID.
    key :: Lude.Maybe Lude.Text,
    -- | Information that further explains @Status@ .
    statusDetail :: Lude.Maybe Lude.Text,
    -- | A sequential counter, starting with 1, that identifies an output among the outputs from the current job. In the Output syntax, this value is always 1.
    id :: Lude.Maybe Lude.Text,
    -- | /Important:/ (Outputs in Fragmented MP4 or MPEG-TS format only.
    --
    -- If you specify a preset in @PresetId@ for which the value of @Container@ is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), @SegmentDuration@ is the target maximum duration of each segment in seconds. For @HLSv3@ format playlists, each media segment is stored in a separate @.ts@ file. For @HLSv4@ , @MPEG-DASH@ , and @Smooth@ playlists, all media segments for an output are stored in a single file. Each segment is approximately the length of the @SegmentDuration@ , though individual segments might be shorter or longer.
    -- The range of valid values is 1 to 60 seconds. If the duration of the video is not evenly divisible by @SegmentDuration@ , the duration of the last segment is the remainder of total length/SegmentDuration.
    -- Elastic Transcoder creates an output-specific playlist for each output @HLS@ output that you specify in OutputKeys. To add an output to the master playlist for this job, include it in the @OutputKeys@ of the associated playlist.
    segmentDuration :: Lude.Maybe Lude.Text,
    -- | Duration of the output file, in milliseconds.
    durationMillis :: Lude.Maybe Lude.Integer,
    -- | The encryption settings, if any, that you want Elastic Transcoder to apply to your thumbnail.
    thumbnailEncryption :: Lude.Maybe Encryption,
    -- | Duration of the output file, in seconds.
    duration :: Lude.Maybe Lude.Integer,
    -- | The number of degrees clockwise by which you want Elastic Transcoder to rotate the output relative to the input. Enter one of the following values:
    --
    -- @auto@ , @0@ , @90@ , @180@ , @270@
    -- The value @auto@ generally works only if the file that you're transcoding contains rotation metadata.
    rotate :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobOutput' with the minimum fields required to make a request.
--
-- * 'appliedColorSpaceConversion' - If Elastic Transcoder used a preset with a @ColorSpaceConversionMode@ to transcode the output file, the @AppliedColorSpaceConversion@ parameter shows the conversion used. If no @ColorSpaceConversionMode@ was defined in the preset, this parameter is not be included in the job response.
-- * 'thumbnailPattern' - Whether you want Elastic Transcoder to create thumbnails for your videos and, if so, how you want Elastic Transcoder to name the files.
--
-- If you don't want Elastic Transcoder to create thumbnails, specify "".
-- If you do want Elastic Transcoder to create thumbnails, specify the information that you want to include in the file name for each thumbnail. You can specify the following values in any sequence:
--
--     * __@{count}@ (Required)__ : If you want to create thumbnails, you must include @{count}@ in the @ThumbnailPattern@ object. Wherever you specify @{count}@ , Elastic Transcoder adds a five-digit sequence number (beginning with __00001__ ) to thumbnail file names. The number indicates where a given thumbnail appears in the sequence of thumbnails for a transcoded file.
-- /Important:/ If you specify a literal value and/or @{resolution}@ but you omit @{count}@ , Elastic Transcoder returns a validation error and does not create the job.
--
--
--     * __Literal values (Optional)__ : You can specify literal values anywhere in the @ThumbnailPattern@ object. For example, you can include them as a file name prefix or as a delimiter between @{resolution}@ and @{count}@ .
--
--
--     * __@{resolution}@ (Optional)__ : If you want Elastic Transcoder to include the resolution in the file name, include @{resolution}@ in the @ThumbnailPattern@ object.
--
--
-- When creating thumbnails, Elastic Transcoder automatically saves the files in the format (.jpg or .png) that appears in the preset that you specified in the @PresetID@ value of @CreateJobOutput@ . Elastic Transcoder also appends the applicable file name extension.
-- * 'status' - The status of one output in a job. If you specified only one output for the job, @Outputs:Status@ is always the same as @Job:Status@ . If you specified more than one output:
--
--
--     * @Job:Status@ and @Outputs:Status@ for all of the outputs is Submitted until Elastic Transcoder starts to process the first output.
--
--
--     * When Elastic Transcoder starts to process the first output, @Outputs:Status@ for that output and @Job:Status@ both change to Progressing. For each output, the value of @Outputs:Status@ remains Submitted until Elastic Transcoder starts to process the output.
--
--
--     * Job:Status remains Progressing until all of the outputs reach a terminal status, either Complete or Error.
--
--
--     * When all of the outputs reach a terminal status, @Job:Status@ changes to Complete only if @Outputs:Status@ for all of the outputs is @Complete@ . If @Outputs:Status@ for one or more outputs is @Error@ , the terminal status for @Job:Status@ is also @Error@ .
--
--
-- The value of @Status@ is one of the following: @Submitted@ , @Progressing@ , @Complete@ , @Canceled@ , or @Error@ .
-- * 'height' - Height of the output file, in pixels.
-- * 'frameRate' - Frame rate of the output file, in frames per second.
-- * 'captions' - You can configure Elastic Transcoder to transcode captions, or subtitles, from one format to another. All captions must be in UTF-8. Elastic Transcoder supports two types of captions:
--
--
--     * __Embedded:__ Embedded captions are included in the same file as the audio and video. Elastic Transcoder supports only one embedded caption per language, to a maximum of 300 embedded captions per file.
-- Valid input values include: @CEA-608 (EIA-608@ , first non-empty channel only), @CEA-708 (EIA-708@ , first non-empty channel only), and @mov-text@
-- Valid outputs include: @mov-text@
-- Elastic Transcoder supports a maximum of one embedded format per output.
--
--
--     * __Sidecar:__ Sidecar captions are kept in a separate metadata file from the audio and video data. Sidecar captions require a player that is capable of understanding the relationship between the video file and the sidecar file. Elastic Transcoder supports only one sidecar caption per language, to a maximum of 20 sidecar captions per file.
-- Valid input values include: @dfxp@ (first div element only), @ebu-tt@ , @scc@ , @smpt@ , @srt@ , @ttml@ (first div element only), and @webvtt@
-- Valid outputs include: @dfxp@ (first div element only), @scc@ , @srt@ , and @webvtt@ .
--
--
-- If you want ttml or smpte-tt compatible captions, specify dfxp as your output format.
-- Elastic Transcoder does not support OCR (Optical Character Recognition), does not accept pictures as a valid input for captions, and is not available for audio-only transcoding. Elastic Transcoder does not preserve text formatting (for example, italics) during the transcoding process.
-- To remove captions or leave the captions empty, set @Captions@ to null. To pass through existing captions unchanged, set the @MergePolicy@ to @MergeRetain@ , and pass in a null @CaptionSources@ array.
-- For more information on embedded files, see the Subtitles Wikipedia page.
-- For more information on sidecar files, see the Extensible Metadata Platform and Sidecar file Wikipedia pages.
-- * 'presetId' - The value of the @Id@ object for the preset that you want to use for this job. The preset determines the audio, video, and thumbnail settings that Elastic Transcoder uses for transcoding. To use a preset that you created, specify the preset ID that Elastic Transcoder returned in the response when you created the preset. You can also use the Elastic Transcoder system presets, which you can get with @ListPresets@ .
-- * 'composition' - You can create an output file that contains an excerpt from the input file. This excerpt, called a clip, can come from the beginning, middle, or end of the file. The Composition object contains settings for the clips that make up an output file. For the current release, you can only specify settings for a single clip per output file. The Composition object cannot be null.
-- * 'albumArt' - The album art to be associated with the output file, if any.
-- * 'fileSize' - File size of the output file, in bytes.
-- * 'watermarks' - Information about the watermarks that you want Elastic Transcoder to add to the video during transcoding. You can specify up to four watermarks for each output. Settings for each watermark must be defined in the preset that you specify in @Preset@ for the current output.
--
-- Watermarks are added to the output video in the sequence in which you list them in the job output—the first watermark in the list is added to the output video first, the second watermark in the list is added next, and so on. As a result, if the settings in a preset cause Elastic Transcoder to place all watermarks in the same location, the second watermark that you add covers the first one, the third one covers the second, and the fourth one covers the third.
-- * 'width' - Specifies the width of the output file in pixels.
-- * 'encryption' - The encryption settings, if any, that you want Elastic Transcoder to apply to your output files. If you choose to use encryption, you must specify a mode to use. If you choose not to use encryption, Elastic Transcoder writes an unencrypted file to your Amazon S3 bucket.
-- * 'key' - The name to assign to the transcoded file. Elastic Transcoder saves the file in the Amazon S3 bucket specified by the @OutputBucket@ object in the pipeline that is specified by the pipeline ID.
-- * 'statusDetail' - Information that further explains @Status@ .
-- * 'id' - A sequential counter, starting with 1, that identifies an output among the outputs from the current job. In the Output syntax, this value is always 1.
-- * 'segmentDuration' - /Important:/ (Outputs in Fragmented MP4 or MPEG-TS format only.
--
-- If you specify a preset in @PresetId@ for which the value of @Container@ is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), @SegmentDuration@ is the target maximum duration of each segment in seconds. For @HLSv3@ format playlists, each media segment is stored in a separate @.ts@ file. For @HLSv4@ , @MPEG-DASH@ , and @Smooth@ playlists, all media segments for an output are stored in a single file. Each segment is approximately the length of the @SegmentDuration@ , though individual segments might be shorter or longer.
-- The range of valid values is 1 to 60 seconds. If the duration of the video is not evenly divisible by @SegmentDuration@ , the duration of the last segment is the remainder of total length/SegmentDuration.
-- Elastic Transcoder creates an output-specific playlist for each output @HLS@ output that you specify in OutputKeys. To add an output to the master playlist for this job, include it in the @OutputKeys@ of the associated playlist.
-- * 'durationMillis' - Duration of the output file, in milliseconds.
-- * 'thumbnailEncryption' - The encryption settings, if any, that you want Elastic Transcoder to apply to your thumbnail.
-- * 'duration' - Duration of the output file, in seconds.
-- * 'rotate' - The number of degrees clockwise by which you want Elastic Transcoder to rotate the output relative to the input. Enter one of the following values:
--
-- @auto@ , @0@ , @90@ , @180@ , @270@
-- The value @auto@ generally works only if the file that you're transcoding contains rotation metadata.
mkJobOutput ::
  JobOutput
mkJobOutput =
  JobOutput'
    { appliedColorSpaceConversion = Lude.Nothing,
      thumbnailPattern = Lude.Nothing,
      status = Lude.Nothing,
      height = Lude.Nothing,
      frameRate = Lude.Nothing,
      captions = Lude.Nothing,
      presetId = Lude.Nothing,
      composition = Lude.Nothing,
      albumArt = Lude.Nothing,
      fileSize = Lude.Nothing,
      watermarks = Lude.Nothing,
      width = Lude.Nothing,
      encryption = Lude.Nothing,
      key = Lude.Nothing,
      statusDetail = Lude.Nothing,
      id = Lude.Nothing,
      segmentDuration = Lude.Nothing,
      durationMillis = Lude.Nothing,
      thumbnailEncryption = Lude.Nothing,
      duration = Lude.Nothing,
      rotate = Lude.Nothing
    }

-- | If Elastic Transcoder used a preset with a @ColorSpaceConversionMode@ to transcode the output file, the @AppliedColorSpaceConversion@ parameter shows the conversion used. If no @ColorSpaceConversionMode@ was defined in the preset, this parameter is not be included in the job response.
--
-- /Note:/ Consider using 'appliedColorSpaceConversion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joAppliedColorSpaceConversion :: Lens.Lens' JobOutput (Lude.Maybe Lude.Text)
joAppliedColorSpaceConversion = Lens.lens (appliedColorSpaceConversion :: JobOutput -> Lude.Maybe Lude.Text) (\s a -> s {appliedColorSpaceConversion = a} :: JobOutput)
{-# DEPRECATED joAppliedColorSpaceConversion "Use generic-lens or generic-optics with 'appliedColorSpaceConversion' instead." #-}

-- | Whether you want Elastic Transcoder to create thumbnails for your videos and, if so, how you want Elastic Transcoder to name the files.
--
-- If you don't want Elastic Transcoder to create thumbnails, specify "".
-- If you do want Elastic Transcoder to create thumbnails, specify the information that you want to include in the file name for each thumbnail. You can specify the following values in any sequence:
--
--     * __@{count}@ (Required)__ : If you want to create thumbnails, you must include @{count}@ in the @ThumbnailPattern@ object. Wherever you specify @{count}@ , Elastic Transcoder adds a five-digit sequence number (beginning with __00001__ ) to thumbnail file names. The number indicates where a given thumbnail appears in the sequence of thumbnails for a transcoded file.
-- /Important:/ If you specify a literal value and/or @{resolution}@ but you omit @{count}@ , Elastic Transcoder returns a validation error and does not create the job.
--
--
--     * __Literal values (Optional)__ : You can specify literal values anywhere in the @ThumbnailPattern@ object. For example, you can include them as a file name prefix or as a delimiter between @{resolution}@ and @{count}@ .
--
--
--     * __@{resolution}@ (Optional)__ : If you want Elastic Transcoder to include the resolution in the file name, include @{resolution}@ in the @ThumbnailPattern@ object.
--
--
-- When creating thumbnails, Elastic Transcoder automatically saves the files in the format (.jpg or .png) that appears in the preset that you specified in the @PresetID@ value of @CreateJobOutput@ . Elastic Transcoder also appends the applicable file name extension.
--
-- /Note:/ Consider using 'thumbnailPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joThumbnailPattern :: Lens.Lens' JobOutput (Lude.Maybe Lude.Text)
joThumbnailPattern = Lens.lens (thumbnailPattern :: JobOutput -> Lude.Maybe Lude.Text) (\s a -> s {thumbnailPattern = a} :: JobOutput)
{-# DEPRECATED joThumbnailPattern "Use generic-lens or generic-optics with 'thumbnailPattern' instead." #-}

-- | The status of one output in a job. If you specified only one output for the job, @Outputs:Status@ is always the same as @Job:Status@ . If you specified more than one output:
--
--
--     * @Job:Status@ and @Outputs:Status@ for all of the outputs is Submitted until Elastic Transcoder starts to process the first output.
--
--
--     * When Elastic Transcoder starts to process the first output, @Outputs:Status@ for that output and @Job:Status@ both change to Progressing. For each output, the value of @Outputs:Status@ remains Submitted until Elastic Transcoder starts to process the output.
--
--
--     * Job:Status remains Progressing until all of the outputs reach a terminal status, either Complete or Error.
--
--
--     * When all of the outputs reach a terminal status, @Job:Status@ changes to Complete only if @Outputs:Status@ for all of the outputs is @Complete@ . If @Outputs:Status@ for one or more outputs is @Error@ , the terminal status for @Job:Status@ is also @Error@ .
--
--
-- The value of @Status@ is one of the following: @Submitted@ , @Progressing@ , @Complete@ , @Canceled@ , or @Error@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joStatus :: Lens.Lens' JobOutput (Lude.Maybe Lude.Text)
joStatus = Lens.lens (status :: JobOutput -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: JobOutput)
{-# DEPRECATED joStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Height of the output file, in pixels.
--
-- /Note:/ Consider using 'height' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joHeight :: Lens.Lens' JobOutput (Lude.Maybe Lude.Int)
joHeight = Lens.lens (height :: JobOutput -> Lude.Maybe Lude.Int) (\s a -> s {height = a} :: JobOutput)
{-# DEPRECATED joHeight "Use generic-lens or generic-optics with 'height' instead." #-}

-- | Frame rate of the output file, in frames per second.
--
-- /Note:/ Consider using 'frameRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joFrameRate :: Lens.Lens' JobOutput (Lude.Maybe Lude.Text)
joFrameRate = Lens.lens (frameRate :: JobOutput -> Lude.Maybe Lude.Text) (\s a -> s {frameRate = a} :: JobOutput)
{-# DEPRECATED joFrameRate "Use generic-lens or generic-optics with 'frameRate' instead." #-}

-- | You can configure Elastic Transcoder to transcode captions, or subtitles, from one format to another. All captions must be in UTF-8. Elastic Transcoder supports two types of captions:
--
--
--     * __Embedded:__ Embedded captions are included in the same file as the audio and video. Elastic Transcoder supports only one embedded caption per language, to a maximum of 300 embedded captions per file.
-- Valid input values include: @CEA-608 (EIA-608@ , first non-empty channel only), @CEA-708 (EIA-708@ , first non-empty channel only), and @mov-text@
-- Valid outputs include: @mov-text@
-- Elastic Transcoder supports a maximum of one embedded format per output.
--
--
--     * __Sidecar:__ Sidecar captions are kept in a separate metadata file from the audio and video data. Sidecar captions require a player that is capable of understanding the relationship between the video file and the sidecar file. Elastic Transcoder supports only one sidecar caption per language, to a maximum of 20 sidecar captions per file.
-- Valid input values include: @dfxp@ (first div element only), @ebu-tt@ , @scc@ , @smpt@ , @srt@ , @ttml@ (first div element only), and @webvtt@
-- Valid outputs include: @dfxp@ (first div element only), @scc@ , @srt@ , and @webvtt@ .
--
--
-- If you want ttml or smpte-tt compatible captions, specify dfxp as your output format.
-- Elastic Transcoder does not support OCR (Optical Character Recognition), does not accept pictures as a valid input for captions, and is not available for audio-only transcoding. Elastic Transcoder does not preserve text formatting (for example, italics) during the transcoding process.
-- To remove captions or leave the captions empty, set @Captions@ to null. To pass through existing captions unchanged, set the @MergePolicy@ to @MergeRetain@ , and pass in a null @CaptionSources@ array.
-- For more information on embedded files, see the Subtitles Wikipedia page.
-- For more information on sidecar files, see the Extensible Metadata Platform and Sidecar file Wikipedia pages.
--
-- /Note:/ Consider using 'captions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joCaptions :: Lens.Lens' JobOutput (Lude.Maybe Captions)
joCaptions = Lens.lens (captions :: JobOutput -> Lude.Maybe Captions) (\s a -> s {captions = a} :: JobOutput)
{-# DEPRECATED joCaptions "Use generic-lens or generic-optics with 'captions' instead." #-}

-- | The value of the @Id@ object for the preset that you want to use for this job. The preset determines the audio, video, and thumbnail settings that Elastic Transcoder uses for transcoding. To use a preset that you created, specify the preset ID that Elastic Transcoder returned in the response when you created the preset. You can also use the Elastic Transcoder system presets, which you can get with @ListPresets@ .
--
-- /Note:/ Consider using 'presetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joPresetId :: Lens.Lens' JobOutput (Lude.Maybe Lude.Text)
joPresetId = Lens.lens (presetId :: JobOutput -> Lude.Maybe Lude.Text) (\s a -> s {presetId = a} :: JobOutput)
{-# DEPRECATED joPresetId "Use generic-lens or generic-optics with 'presetId' instead." #-}

-- | You can create an output file that contains an excerpt from the input file. This excerpt, called a clip, can come from the beginning, middle, or end of the file. The Composition object contains settings for the clips that make up an output file. For the current release, you can only specify settings for a single clip per output file. The Composition object cannot be null.
--
-- /Note:/ Consider using 'composition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joComposition :: Lens.Lens' JobOutput (Lude.Maybe [Clip])
joComposition = Lens.lens (composition :: JobOutput -> Lude.Maybe [Clip]) (\s a -> s {composition = a} :: JobOutput)
{-# DEPRECATED joComposition "Use generic-lens or generic-optics with 'composition' instead." #-}

-- | The album art to be associated with the output file, if any.
--
-- /Note:/ Consider using 'albumArt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joAlbumArt :: Lens.Lens' JobOutput (Lude.Maybe JobAlbumArt)
joAlbumArt = Lens.lens (albumArt :: JobOutput -> Lude.Maybe JobAlbumArt) (\s a -> s {albumArt = a} :: JobOutput)
{-# DEPRECATED joAlbumArt "Use generic-lens or generic-optics with 'albumArt' instead." #-}

-- | File size of the output file, in bytes.
--
-- /Note:/ Consider using 'fileSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joFileSize :: Lens.Lens' JobOutput (Lude.Maybe Lude.Integer)
joFileSize = Lens.lens (fileSize :: JobOutput -> Lude.Maybe Lude.Integer) (\s a -> s {fileSize = a} :: JobOutput)
{-# DEPRECATED joFileSize "Use generic-lens or generic-optics with 'fileSize' instead." #-}

-- | Information about the watermarks that you want Elastic Transcoder to add to the video during transcoding. You can specify up to four watermarks for each output. Settings for each watermark must be defined in the preset that you specify in @Preset@ for the current output.
--
-- Watermarks are added to the output video in the sequence in which you list them in the job output—the first watermark in the list is added to the output video first, the second watermark in the list is added next, and so on. As a result, if the settings in a preset cause Elastic Transcoder to place all watermarks in the same location, the second watermark that you add covers the first one, the third one covers the second, and the fourth one covers the third.
--
-- /Note:/ Consider using 'watermarks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joWatermarks :: Lens.Lens' JobOutput (Lude.Maybe [JobWatermark])
joWatermarks = Lens.lens (watermarks :: JobOutput -> Lude.Maybe [JobWatermark]) (\s a -> s {watermarks = a} :: JobOutput)
{-# DEPRECATED joWatermarks "Use generic-lens or generic-optics with 'watermarks' instead." #-}

-- | Specifies the width of the output file in pixels.
--
-- /Note:/ Consider using 'width' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joWidth :: Lens.Lens' JobOutput (Lude.Maybe Lude.Int)
joWidth = Lens.lens (width :: JobOutput -> Lude.Maybe Lude.Int) (\s a -> s {width = a} :: JobOutput)
{-# DEPRECATED joWidth "Use generic-lens or generic-optics with 'width' instead." #-}

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to your output files. If you choose to use encryption, you must specify a mode to use. If you choose not to use encryption, Elastic Transcoder writes an unencrypted file to your Amazon S3 bucket.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joEncryption :: Lens.Lens' JobOutput (Lude.Maybe Encryption)
joEncryption = Lens.lens (encryption :: JobOutput -> Lude.Maybe Encryption) (\s a -> s {encryption = a} :: JobOutput)
{-# DEPRECATED joEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | The name to assign to the transcoded file. Elastic Transcoder saves the file in the Amazon S3 bucket specified by the @OutputBucket@ object in the pipeline that is specified by the pipeline ID.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joKey :: Lens.Lens' JobOutput (Lude.Maybe Lude.Text)
joKey = Lens.lens (key :: JobOutput -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: JobOutput)
{-# DEPRECATED joKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Information that further explains @Status@ .
--
-- /Note:/ Consider using 'statusDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joStatusDetail :: Lens.Lens' JobOutput (Lude.Maybe Lude.Text)
joStatusDetail = Lens.lens (statusDetail :: JobOutput -> Lude.Maybe Lude.Text) (\s a -> s {statusDetail = a} :: JobOutput)
{-# DEPRECATED joStatusDetail "Use generic-lens or generic-optics with 'statusDetail' instead." #-}

-- | A sequential counter, starting with 1, that identifies an output among the outputs from the current job. In the Output syntax, this value is always 1.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joId :: Lens.Lens' JobOutput (Lude.Maybe Lude.Text)
joId = Lens.lens (id :: JobOutput -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: JobOutput)
{-# DEPRECATED joId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | /Important:/ (Outputs in Fragmented MP4 or MPEG-TS format only.
--
-- If you specify a preset in @PresetId@ for which the value of @Container@ is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), @SegmentDuration@ is the target maximum duration of each segment in seconds. For @HLSv3@ format playlists, each media segment is stored in a separate @.ts@ file. For @HLSv4@ , @MPEG-DASH@ , and @Smooth@ playlists, all media segments for an output are stored in a single file. Each segment is approximately the length of the @SegmentDuration@ , though individual segments might be shorter or longer.
-- The range of valid values is 1 to 60 seconds. If the duration of the video is not evenly divisible by @SegmentDuration@ , the duration of the last segment is the remainder of total length/SegmentDuration.
-- Elastic Transcoder creates an output-specific playlist for each output @HLS@ output that you specify in OutputKeys. To add an output to the master playlist for this job, include it in the @OutputKeys@ of the associated playlist.
--
-- /Note:/ Consider using 'segmentDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joSegmentDuration :: Lens.Lens' JobOutput (Lude.Maybe Lude.Text)
joSegmentDuration = Lens.lens (segmentDuration :: JobOutput -> Lude.Maybe Lude.Text) (\s a -> s {segmentDuration = a} :: JobOutput)
{-# DEPRECATED joSegmentDuration "Use generic-lens or generic-optics with 'segmentDuration' instead." #-}

-- | Duration of the output file, in milliseconds.
--
-- /Note:/ Consider using 'durationMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joDurationMillis :: Lens.Lens' JobOutput (Lude.Maybe Lude.Integer)
joDurationMillis = Lens.lens (durationMillis :: JobOutput -> Lude.Maybe Lude.Integer) (\s a -> s {durationMillis = a} :: JobOutput)
{-# DEPRECATED joDurationMillis "Use generic-lens or generic-optics with 'durationMillis' instead." #-}

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to your thumbnail.
--
-- /Note:/ Consider using 'thumbnailEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joThumbnailEncryption :: Lens.Lens' JobOutput (Lude.Maybe Encryption)
joThumbnailEncryption = Lens.lens (thumbnailEncryption :: JobOutput -> Lude.Maybe Encryption) (\s a -> s {thumbnailEncryption = a} :: JobOutput)
{-# DEPRECATED joThumbnailEncryption "Use generic-lens or generic-optics with 'thumbnailEncryption' instead." #-}

-- | Duration of the output file, in seconds.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joDuration :: Lens.Lens' JobOutput (Lude.Maybe Lude.Integer)
joDuration = Lens.lens (duration :: JobOutput -> Lude.Maybe Lude.Integer) (\s a -> s {duration = a} :: JobOutput)
{-# DEPRECATED joDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The number of degrees clockwise by which you want Elastic Transcoder to rotate the output relative to the input. Enter one of the following values:
--
-- @auto@ , @0@ , @90@ , @180@ , @270@
-- The value @auto@ generally works only if the file that you're transcoding contains rotation metadata.
--
-- /Note:/ Consider using 'rotate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joRotate :: Lens.Lens' JobOutput (Lude.Maybe Lude.Text)
joRotate = Lens.lens (rotate :: JobOutput -> Lude.Maybe Lude.Text) (\s a -> s {rotate = a} :: JobOutput)
{-# DEPRECATED joRotate "Use generic-lens or generic-optics with 'rotate' instead." #-}

instance Lude.FromJSON JobOutput where
  parseJSON =
    Lude.withObject
      "JobOutput"
      ( \x ->
          JobOutput'
            Lude.<$> (x Lude..:? "AppliedColorSpaceConversion")
            Lude.<*> (x Lude..:? "ThumbnailPattern")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "Height")
            Lude.<*> (x Lude..:? "FrameRate")
            Lude.<*> (x Lude..:? "Captions")
            Lude.<*> (x Lude..:? "PresetId")
            Lude.<*> (x Lude..:? "Composition" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AlbumArt")
            Lude.<*> (x Lude..:? "FileSize")
            Lude.<*> (x Lude..:? "Watermarks" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Width")
            Lude.<*> (x Lude..:? "Encryption")
            Lude.<*> (x Lude..:? "Key")
            Lude.<*> (x Lude..:? "StatusDetail")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "SegmentDuration")
            Lude.<*> (x Lude..:? "DurationMillis")
            Lude.<*> (x Lude..:? "ThumbnailEncryption")
            Lude.<*> (x Lude..:? "Duration")
            Lude.<*> (x Lude..:? "Rotate")
      )
