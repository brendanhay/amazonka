{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.JobOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Types.JobOutput
  ( JobOutput (..)
  -- * Smart constructor
  , mkJobOutput
  -- * Lenses
  , joAlbumArt
  , joAppliedColorSpaceConversion
  , joCaptions
  , joComposition
  , joDuration
  , joDurationMillis
  , joEncryption
  , joFileSize
  , joFrameRate
  , joHeight
  , joId
  , joKey
  , joPresetId
  , joRotate
  , joSegmentDuration
  , joStatus
  , joStatusDetail
  , joThumbnailEncryption
  , joThumbnailPattern
  , joWatermarks
  , joWidth
  ) where

import qualified Network.AWS.ElasticTranscoder.Types.Captions as Types
import qualified Network.AWS.ElasticTranscoder.Types.Clip as Types
import qualified Network.AWS.ElasticTranscoder.Types.Encryption as Types
import qualified Network.AWS.ElasticTranscoder.Types.FrameRate as Types
import qualified Network.AWS.ElasticTranscoder.Types.JobAlbumArt as Types
import qualified Network.AWS.ElasticTranscoder.Types.JobWatermark as Types
import qualified Network.AWS.ElasticTranscoder.Types.Key as Types
import qualified Network.AWS.ElasticTranscoder.Types.PresetId as Types
import qualified Network.AWS.ElasticTranscoder.Types.Rotate as Types
import qualified Network.AWS.ElasticTranscoder.Types.SegmentDuration as Types
import qualified Network.AWS.ElasticTranscoder.Types.Status as Types
import qualified Network.AWS.ElasticTranscoder.Types.StatusDetail as Types
import qualified Network.AWS.ElasticTranscoder.Types.ThumbnailPattern as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /Important:/ Outputs recommended instead.
--
-- If you specified one output for a job, information about that output. If you specified multiple outputs for a job, the @Output@ object lists information about the first output. This duplicates the information that is listed for the first output in the @Outputs@ object.
--
-- /See:/ 'mkJobOutput' smart constructor.
data JobOutput = JobOutput'
  { albumArt :: Core.Maybe Types.JobAlbumArt
    -- ^ The album art to be associated with the output file, if any.
  , appliedColorSpaceConversion :: Core.Maybe Core.Text
    -- ^ If Elastic Transcoder used a preset with a @ColorSpaceConversionMode@ to transcode the output file, the @AppliedColorSpaceConversion@ parameter shows the conversion used. If no @ColorSpaceConversionMode@ was defined in the preset, this parameter is not be included in the job response.
  , captions :: Core.Maybe Types.Captions
    -- ^ You can configure Elastic Transcoder to transcode captions, or subtitles, from one format to another. All captions must be in UTF-8. Elastic Transcoder supports two types of captions:
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
  , composition :: Core.Maybe [Types.Clip]
    -- ^ You can create an output file that contains an excerpt from the input file. This excerpt, called a clip, can come from the beginning, middle, or end of the file. The Composition object contains settings for the clips that make up an output file. For the current release, you can only specify settings for a single clip per output file. The Composition object cannot be null.
  , duration :: Core.Maybe Core.Integer
    -- ^ Duration of the output file, in seconds.
  , durationMillis :: Core.Maybe Core.Integer
    -- ^ Duration of the output file, in milliseconds.
  , encryption :: Core.Maybe Types.Encryption
    -- ^ The encryption settings, if any, that you want Elastic Transcoder to apply to your output files. If you choose to use encryption, you must specify a mode to use. If you choose not to use encryption, Elastic Transcoder writes an unencrypted file to your Amazon S3 bucket.
  , fileSize :: Core.Maybe Core.Integer
    -- ^ File size of the output file, in bytes.
  , frameRate :: Core.Maybe Types.FrameRate
    -- ^ Frame rate of the output file, in frames per second.
  , height :: Core.Maybe Core.Int
    -- ^ Height of the output file, in pixels.
  , id :: Core.Maybe Core.Text
    -- ^ A sequential counter, starting with 1, that identifies an output among the outputs from the current job. In the Output syntax, this value is always 1.
  , key :: Core.Maybe Types.Key
    -- ^ The name to assign to the transcoded file. Elastic Transcoder saves the file in the Amazon S3 bucket specified by the @OutputBucket@ object in the pipeline that is specified by the pipeline ID.
  , presetId :: Core.Maybe Types.PresetId
    -- ^ The value of the @Id@ object for the preset that you want to use for this job. The preset determines the audio, video, and thumbnail settings that Elastic Transcoder uses for transcoding. To use a preset that you created, specify the preset ID that Elastic Transcoder returned in the response when you created the preset. You can also use the Elastic Transcoder system presets, which you can get with @ListPresets@ .
  , rotate :: Core.Maybe Types.Rotate
    -- ^ The number of degrees clockwise by which you want Elastic Transcoder to rotate the output relative to the input. Enter one of the following values:
--
-- @auto@ , @0@ , @90@ , @180@ , @270@ 
-- The value @auto@ generally works only if the file that you're transcoding contains rotation metadata.
  , segmentDuration :: Core.Maybe Types.SegmentDuration
    -- ^ /Important:/ (Outputs in Fragmented MP4 or MPEG-TS format only.
--
-- If you specify a preset in @PresetId@ for which the value of @Container@ is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), @SegmentDuration@ is the target maximum duration of each segment in seconds. For @HLSv3@ format playlists, each media segment is stored in a separate @.ts@ file. For @HLSv4@ , @MPEG-DASH@ , and @Smooth@ playlists, all media segments for an output are stored in a single file. Each segment is approximately the length of the @SegmentDuration@ , though individual segments might be shorter or longer.
-- The range of valid values is 1 to 60 seconds. If the duration of the video is not evenly divisible by @SegmentDuration@ , the duration of the last segment is the remainder of total length/SegmentDuration.
-- Elastic Transcoder creates an output-specific playlist for each output @HLS@ output that you specify in OutputKeys. To add an output to the master playlist for this job, include it in the @OutputKeys@ of the associated playlist.
  , status :: Core.Maybe Types.Status
    -- ^ The status of one output in a job. If you specified only one output for the job, @Outputs:Status@ is always the same as @Job:Status@ . If you specified more than one output: 
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
  , statusDetail :: Core.Maybe Types.StatusDetail
    -- ^ Information that further explains @Status@ .
  , thumbnailEncryption :: Core.Maybe Types.Encryption
    -- ^ The encryption settings, if any, that you want Elastic Transcoder to apply to your thumbnail.
  , thumbnailPattern :: Core.Maybe Types.ThumbnailPattern
    -- ^ Whether you want Elastic Transcoder to create thumbnails for your videos and, if so, how you want Elastic Transcoder to name the files.
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
  , watermarks :: Core.Maybe [Types.JobWatermark]
    -- ^ Information about the watermarks that you want Elastic Transcoder to add to the video during transcoding. You can specify up to four watermarks for each output. Settings for each watermark must be defined in the preset that you specify in @Preset@ for the current output.
--
-- Watermarks are added to the output video in the sequence in which you list them in the job output—the first watermark in the list is added to the output video first, the second watermark in the list is added next, and so on. As a result, if the settings in a preset cause Elastic Transcoder to place all watermarks in the same location, the second watermark that you add covers the first one, the third one covers the second, and the fourth one covers the third.
  , width :: Core.Maybe Core.Int
    -- ^ Specifies the width of the output file in pixels.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobOutput' value with any optional fields omitted.
mkJobOutput
    :: JobOutput
mkJobOutput
  = JobOutput'{albumArt = Core.Nothing,
               appliedColorSpaceConversion = Core.Nothing,
               captions = Core.Nothing, composition = Core.Nothing,
               duration = Core.Nothing, durationMillis = Core.Nothing,
               encryption = Core.Nothing, fileSize = Core.Nothing,
               frameRate = Core.Nothing, height = Core.Nothing, id = Core.Nothing,
               key = Core.Nothing, presetId = Core.Nothing, rotate = Core.Nothing,
               segmentDuration = Core.Nothing, status = Core.Nothing,
               statusDetail = Core.Nothing, thumbnailEncryption = Core.Nothing,
               thumbnailPattern = Core.Nothing, watermarks = Core.Nothing,
               width = Core.Nothing}

-- | The album art to be associated with the output file, if any.
--
-- /Note:/ Consider using 'albumArt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joAlbumArt :: Lens.Lens' JobOutput (Core.Maybe Types.JobAlbumArt)
joAlbumArt = Lens.field @"albumArt"
{-# INLINEABLE joAlbumArt #-}
{-# DEPRECATED albumArt "Use generic-lens or generic-optics with 'albumArt' instead"  #-}

-- | If Elastic Transcoder used a preset with a @ColorSpaceConversionMode@ to transcode the output file, the @AppliedColorSpaceConversion@ parameter shows the conversion used. If no @ColorSpaceConversionMode@ was defined in the preset, this parameter is not be included in the job response.
--
-- /Note:/ Consider using 'appliedColorSpaceConversion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joAppliedColorSpaceConversion :: Lens.Lens' JobOutput (Core.Maybe Core.Text)
joAppliedColorSpaceConversion = Lens.field @"appliedColorSpaceConversion"
{-# INLINEABLE joAppliedColorSpaceConversion #-}
{-# DEPRECATED appliedColorSpaceConversion "Use generic-lens or generic-optics with 'appliedColorSpaceConversion' instead"  #-}

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
joCaptions :: Lens.Lens' JobOutput (Core.Maybe Types.Captions)
joCaptions = Lens.field @"captions"
{-# INLINEABLE joCaptions #-}
{-# DEPRECATED captions "Use generic-lens or generic-optics with 'captions' instead"  #-}

-- | You can create an output file that contains an excerpt from the input file. This excerpt, called a clip, can come from the beginning, middle, or end of the file. The Composition object contains settings for the clips that make up an output file. For the current release, you can only specify settings for a single clip per output file. The Composition object cannot be null.
--
-- /Note:/ Consider using 'composition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joComposition :: Lens.Lens' JobOutput (Core.Maybe [Types.Clip])
joComposition = Lens.field @"composition"
{-# INLINEABLE joComposition #-}
{-# DEPRECATED composition "Use generic-lens or generic-optics with 'composition' instead"  #-}

-- | Duration of the output file, in seconds.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joDuration :: Lens.Lens' JobOutput (Core.Maybe Core.Integer)
joDuration = Lens.field @"duration"
{-# INLINEABLE joDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | Duration of the output file, in milliseconds.
--
-- /Note:/ Consider using 'durationMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joDurationMillis :: Lens.Lens' JobOutput (Core.Maybe Core.Integer)
joDurationMillis = Lens.field @"durationMillis"
{-# INLINEABLE joDurationMillis #-}
{-# DEPRECATED durationMillis "Use generic-lens or generic-optics with 'durationMillis' instead"  #-}

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to your output files. If you choose to use encryption, you must specify a mode to use. If you choose not to use encryption, Elastic Transcoder writes an unencrypted file to your Amazon S3 bucket.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joEncryption :: Lens.Lens' JobOutput (Core.Maybe Types.Encryption)
joEncryption = Lens.field @"encryption"
{-# INLINEABLE joEncryption #-}
{-# DEPRECATED encryption "Use generic-lens or generic-optics with 'encryption' instead"  #-}

-- | File size of the output file, in bytes.
--
-- /Note:/ Consider using 'fileSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joFileSize :: Lens.Lens' JobOutput (Core.Maybe Core.Integer)
joFileSize = Lens.field @"fileSize"
{-# INLINEABLE joFileSize #-}
{-# DEPRECATED fileSize "Use generic-lens or generic-optics with 'fileSize' instead"  #-}

-- | Frame rate of the output file, in frames per second.
--
-- /Note:/ Consider using 'frameRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joFrameRate :: Lens.Lens' JobOutput (Core.Maybe Types.FrameRate)
joFrameRate = Lens.field @"frameRate"
{-# INLINEABLE joFrameRate #-}
{-# DEPRECATED frameRate "Use generic-lens or generic-optics with 'frameRate' instead"  #-}

-- | Height of the output file, in pixels.
--
-- /Note:/ Consider using 'height' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joHeight :: Lens.Lens' JobOutput (Core.Maybe Core.Int)
joHeight = Lens.field @"height"
{-# INLINEABLE joHeight #-}
{-# DEPRECATED height "Use generic-lens or generic-optics with 'height' instead"  #-}

-- | A sequential counter, starting with 1, that identifies an output among the outputs from the current job. In the Output syntax, this value is always 1.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joId :: Lens.Lens' JobOutput (Core.Maybe Core.Text)
joId = Lens.field @"id"
{-# INLINEABLE joId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name to assign to the transcoded file. Elastic Transcoder saves the file in the Amazon S3 bucket specified by the @OutputBucket@ object in the pipeline that is specified by the pipeline ID.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joKey :: Lens.Lens' JobOutput (Core.Maybe Types.Key)
joKey = Lens.field @"key"
{-# INLINEABLE joKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The value of the @Id@ object for the preset that you want to use for this job. The preset determines the audio, video, and thumbnail settings that Elastic Transcoder uses for transcoding. To use a preset that you created, specify the preset ID that Elastic Transcoder returned in the response when you created the preset. You can also use the Elastic Transcoder system presets, which you can get with @ListPresets@ .
--
-- /Note:/ Consider using 'presetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joPresetId :: Lens.Lens' JobOutput (Core.Maybe Types.PresetId)
joPresetId = Lens.field @"presetId"
{-# INLINEABLE joPresetId #-}
{-# DEPRECATED presetId "Use generic-lens or generic-optics with 'presetId' instead"  #-}

-- | The number of degrees clockwise by which you want Elastic Transcoder to rotate the output relative to the input. Enter one of the following values:
--
-- @auto@ , @0@ , @90@ , @180@ , @270@ 
-- The value @auto@ generally works only if the file that you're transcoding contains rotation metadata.
--
-- /Note:/ Consider using 'rotate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joRotate :: Lens.Lens' JobOutput (Core.Maybe Types.Rotate)
joRotate = Lens.field @"rotate"
{-# INLINEABLE joRotate #-}
{-# DEPRECATED rotate "Use generic-lens or generic-optics with 'rotate' instead"  #-}

-- | /Important:/ (Outputs in Fragmented MP4 or MPEG-TS format only.
--
-- If you specify a preset in @PresetId@ for which the value of @Container@ is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), @SegmentDuration@ is the target maximum duration of each segment in seconds. For @HLSv3@ format playlists, each media segment is stored in a separate @.ts@ file. For @HLSv4@ , @MPEG-DASH@ , and @Smooth@ playlists, all media segments for an output are stored in a single file. Each segment is approximately the length of the @SegmentDuration@ , though individual segments might be shorter or longer.
-- The range of valid values is 1 to 60 seconds. If the duration of the video is not evenly divisible by @SegmentDuration@ , the duration of the last segment is the remainder of total length/SegmentDuration.
-- Elastic Transcoder creates an output-specific playlist for each output @HLS@ output that you specify in OutputKeys. To add an output to the master playlist for this job, include it in the @OutputKeys@ of the associated playlist.
--
-- /Note:/ Consider using 'segmentDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joSegmentDuration :: Lens.Lens' JobOutput (Core.Maybe Types.SegmentDuration)
joSegmentDuration = Lens.field @"segmentDuration"
{-# INLINEABLE joSegmentDuration #-}
{-# DEPRECATED segmentDuration "Use generic-lens or generic-optics with 'segmentDuration' instead"  #-}

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
joStatus :: Lens.Lens' JobOutput (Core.Maybe Types.Status)
joStatus = Lens.field @"status"
{-# INLINEABLE joStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Information that further explains @Status@ .
--
-- /Note:/ Consider using 'statusDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joStatusDetail :: Lens.Lens' JobOutput (Core.Maybe Types.StatusDetail)
joStatusDetail = Lens.field @"statusDetail"
{-# INLINEABLE joStatusDetail #-}
{-# DEPRECATED statusDetail "Use generic-lens or generic-optics with 'statusDetail' instead"  #-}

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to your thumbnail.
--
-- /Note:/ Consider using 'thumbnailEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joThumbnailEncryption :: Lens.Lens' JobOutput (Core.Maybe Types.Encryption)
joThumbnailEncryption = Lens.field @"thumbnailEncryption"
{-# INLINEABLE joThumbnailEncryption #-}
{-# DEPRECATED thumbnailEncryption "Use generic-lens or generic-optics with 'thumbnailEncryption' instead"  #-}

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
joThumbnailPattern :: Lens.Lens' JobOutput (Core.Maybe Types.ThumbnailPattern)
joThumbnailPattern = Lens.field @"thumbnailPattern"
{-# INLINEABLE joThumbnailPattern #-}
{-# DEPRECATED thumbnailPattern "Use generic-lens or generic-optics with 'thumbnailPattern' instead"  #-}

-- | Information about the watermarks that you want Elastic Transcoder to add to the video during transcoding. You can specify up to four watermarks for each output. Settings for each watermark must be defined in the preset that you specify in @Preset@ for the current output.
--
-- Watermarks are added to the output video in the sequence in which you list them in the job output—the first watermark in the list is added to the output video first, the second watermark in the list is added next, and so on. As a result, if the settings in a preset cause Elastic Transcoder to place all watermarks in the same location, the second watermark that you add covers the first one, the third one covers the second, and the fourth one covers the third.
--
-- /Note:/ Consider using 'watermarks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joWatermarks :: Lens.Lens' JobOutput (Core.Maybe [Types.JobWatermark])
joWatermarks = Lens.field @"watermarks"
{-# INLINEABLE joWatermarks #-}
{-# DEPRECATED watermarks "Use generic-lens or generic-optics with 'watermarks' instead"  #-}

-- | Specifies the width of the output file in pixels.
--
-- /Note:/ Consider using 'width' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
joWidth :: Lens.Lens' JobOutput (Core.Maybe Core.Int)
joWidth = Lens.field @"width"
{-# INLINEABLE joWidth #-}
{-# DEPRECATED width "Use generic-lens or generic-optics with 'width' instead"  #-}

instance Core.FromJSON JobOutput where
        parseJSON
          = Core.withObject "JobOutput" Core.$
              \ x ->
                JobOutput' Core.<$>
                  (x Core..:? "AlbumArt") Core.<*>
                    x Core..:? "AppliedColorSpaceConversion"
                    Core.<*> x Core..:? "Captions"
                    Core.<*> x Core..:? "Composition"
                    Core.<*> x Core..:? "Duration"
                    Core.<*> x Core..:? "DurationMillis"
                    Core.<*> x Core..:? "Encryption"
                    Core.<*> x Core..:? "FileSize"
                    Core.<*> x Core..:? "FrameRate"
                    Core.<*> x Core..:? "Height"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "Key"
                    Core.<*> x Core..:? "PresetId"
                    Core.<*> x Core..:? "Rotate"
                    Core.<*> x Core..:? "SegmentDuration"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "StatusDetail"
                    Core.<*> x Core..:? "ThumbnailEncryption"
                    Core.<*> x Core..:? "ThumbnailPattern"
                    Core.<*> x Core..:? "Watermarks"
                    Core.<*> x Core..:? "Width"
