{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.CreateJobOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.CreateJobOutput
  ( CreateJobOutput (..),

    -- * Smart constructor
    mkCreateJobOutput,

    -- * Lenses
    cjoAlbumArt,
    cjoCaptions,
    cjoComposition,
    cjoEncryption,
    cjoKey,
    cjoPresetId,
    cjoRotate,
    cjoSegmentDuration,
    cjoThumbnailEncryption,
    cjoThumbnailPattern,
    cjoWatermarks,
  )
where

import qualified Network.AWS.ElasticTranscoder.Types.Captions as Types
import qualified Network.AWS.ElasticTranscoder.Types.Clip as Types
import qualified Network.AWS.ElasticTranscoder.Types.Encryption as Types
import qualified Network.AWS.ElasticTranscoder.Types.JobAlbumArt as Types
import qualified Network.AWS.ElasticTranscoder.Types.JobWatermark as Types
import qualified Network.AWS.ElasticTranscoder.Types.Key as Types
import qualified Network.AWS.ElasticTranscoder.Types.PresetId as Types
import qualified Network.AWS.ElasticTranscoder.Types.Rotate as Types
import qualified Network.AWS.ElasticTranscoder.Types.SegmentDuration as Types
import qualified Network.AWS.ElasticTranscoder.Types.ThumbnailPattern as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The @CreateJobOutput@ structure.
--
-- /See:/ 'mkCreateJobOutput' smart constructor.
data CreateJobOutput = CreateJobOutput'
  { -- | Information about the album art that you want Elastic Transcoder to add to the file during transcoding. You can specify up to twenty album artworks for each output. Settings for each artwork must be defined in the job for the current output.
    albumArt :: Core.Maybe Types.JobAlbumArt,
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
    captions :: Core.Maybe Types.Captions,
    -- | You can create an output file that contains an excerpt from the input file. This excerpt, called a clip, can come from the beginning, middle, or end of the file. The Composition object contains settings for the clips that make up an output file. For the current release, you can only specify settings for a single clip per output file. The Composition object cannot be null.
    composition :: Core.Maybe [Types.Clip],
    -- | You can specify encryption settings for any output files that you want to use for a transcoding job. This includes the output file and any watermarks, thumbnails, album art, or captions that you want to use. You must specify encryption settings for each file individually.
    encryption :: Core.Maybe Types.Encryption,
    -- | The name to assign to the transcoded file. Elastic Transcoder saves the file in the Amazon S3 bucket specified by the @OutputBucket@ object in the pipeline that is specified by the pipeline ID. If a file with the specified name already exists in the output bucket, the job fails.
    key :: Core.Maybe Types.Key,
    -- | The @Id@ of the preset to use for this job. The preset determines the audio, video, and thumbnail settings that Elastic Transcoder uses for transcoding.
    presetId :: Core.Maybe Types.PresetId,
    -- | The number of degrees clockwise by which you want Elastic Transcoder to rotate the output relative to the input. Enter one of the following values: @auto@ , @0@ , @90@ , @180@ , @270@ . The value @auto@ generally works only if the file that you're transcoding contains rotation metadata.
    rotate :: Core.Maybe Types.Rotate,
    -- | /Important:/ (Outputs in Fragmented MP4 or MPEG-TS format only.
    --
    -- If you specify a preset in @PresetId@ for which the value of @Container@ is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), @SegmentDuration@ is the target maximum duration of each segment in seconds. For @HLSv3@ format playlists, each media segment is stored in a separate @.ts@ file. For @HLSv4@ and @Smooth@ playlists, all media segments for an output are stored in a single file. Each segment is approximately the length of the @SegmentDuration@ , though individual segments might be shorter or longer.
    -- The range of valid values is 1 to 60 seconds. If the duration of the video is not evenly divisible by @SegmentDuration@ , the duration of the last segment is the remainder of total length/SegmentDuration.
    -- Elastic Transcoder creates an output-specific playlist for each output @HLS@ output that you specify in OutputKeys. To add an output to the master playlist for this job, include it in the @OutputKeys@ of the associated playlist.
    segmentDuration :: Core.Maybe Types.SegmentDuration,
    -- | The encryption settings, if any, that you want Elastic Transcoder to apply to your thumbnail.
    thumbnailEncryption :: Core.Maybe Types.Encryption,
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
    thumbnailPattern :: Core.Maybe Types.ThumbnailPattern,
    -- | Information about the watermarks that you want Elastic Transcoder to add to the video during transcoding. You can specify up to four watermarks for each output. Settings for each watermark must be defined in the preset for the current output.
    watermarks :: Core.Maybe [Types.JobWatermark]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateJobOutput' value with any optional fields omitted.
mkCreateJobOutput ::
  CreateJobOutput
mkCreateJobOutput =
  CreateJobOutput'
    { albumArt = Core.Nothing,
      captions = Core.Nothing,
      composition = Core.Nothing,
      encryption = Core.Nothing,
      key = Core.Nothing,
      presetId = Core.Nothing,
      rotate = Core.Nothing,
      segmentDuration = Core.Nothing,
      thumbnailEncryption = Core.Nothing,
      thumbnailPattern = Core.Nothing,
      watermarks = Core.Nothing
    }

-- | Information about the album art that you want Elastic Transcoder to add to the file during transcoding. You can specify up to twenty album artworks for each output. Settings for each artwork must be defined in the job for the current output.
--
-- /Note:/ Consider using 'albumArt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjoAlbumArt :: Lens.Lens' CreateJobOutput (Core.Maybe Types.JobAlbumArt)
cjoAlbumArt = Lens.field @"albumArt"
{-# DEPRECATED cjoAlbumArt "Use generic-lens or generic-optics with 'albumArt' instead." #-}

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
cjoCaptions :: Lens.Lens' CreateJobOutput (Core.Maybe Types.Captions)
cjoCaptions = Lens.field @"captions"
{-# DEPRECATED cjoCaptions "Use generic-lens or generic-optics with 'captions' instead." #-}

-- | You can create an output file that contains an excerpt from the input file. This excerpt, called a clip, can come from the beginning, middle, or end of the file. The Composition object contains settings for the clips that make up an output file. For the current release, you can only specify settings for a single clip per output file. The Composition object cannot be null.
--
-- /Note:/ Consider using 'composition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjoComposition :: Lens.Lens' CreateJobOutput (Core.Maybe [Types.Clip])
cjoComposition = Lens.field @"composition"
{-# DEPRECATED cjoComposition "Use generic-lens or generic-optics with 'composition' instead." #-}

-- | You can specify encryption settings for any output files that you want to use for a transcoding job. This includes the output file and any watermarks, thumbnails, album art, or captions that you want to use. You must specify encryption settings for each file individually.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjoEncryption :: Lens.Lens' CreateJobOutput (Core.Maybe Types.Encryption)
cjoEncryption = Lens.field @"encryption"
{-# DEPRECATED cjoEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | The name to assign to the transcoded file. Elastic Transcoder saves the file in the Amazon S3 bucket specified by the @OutputBucket@ object in the pipeline that is specified by the pipeline ID. If a file with the specified name already exists in the output bucket, the job fails.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjoKey :: Lens.Lens' CreateJobOutput (Core.Maybe Types.Key)
cjoKey = Lens.field @"key"
{-# DEPRECATED cjoKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The @Id@ of the preset to use for this job. The preset determines the audio, video, and thumbnail settings that Elastic Transcoder uses for transcoding.
--
-- /Note:/ Consider using 'presetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjoPresetId :: Lens.Lens' CreateJobOutput (Core.Maybe Types.PresetId)
cjoPresetId = Lens.field @"presetId"
{-# DEPRECATED cjoPresetId "Use generic-lens or generic-optics with 'presetId' instead." #-}

-- | The number of degrees clockwise by which you want Elastic Transcoder to rotate the output relative to the input. Enter one of the following values: @auto@ , @0@ , @90@ , @180@ , @270@ . The value @auto@ generally works only if the file that you're transcoding contains rotation metadata.
--
-- /Note:/ Consider using 'rotate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjoRotate :: Lens.Lens' CreateJobOutput (Core.Maybe Types.Rotate)
cjoRotate = Lens.field @"rotate"
{-# DEPRECATED cjoRotate "Use generic-lens or generic-optics with 'rotate' instead." #-}

-- | /Important:/ (Outputs in Fragmented MP4 or MPEG-TS format only.
--
-- If you specify a preset in @PresetId@ for which the value of @Container@ is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), @SegmentDuration@ is the target maximum duration of each segment in seconds. For @HLSv3@ format playlists, each media segment is stored in a separate @.ts@ file. For @HLSv4@ and @Smooth@ playlists, all media segments for an output are stored in a single file. Each segment is approximately the length of the @SegmentDuration@ , though individual segments might be shorter or longer.
-- The range of valid values is 1 to 60 seconds. If the duration of the video is not evenly divisible by @SegmentDuration@ , the duration of the last segment is the remainder of total length/SegmentDuration.
-- Elastic Transcoder creates an output-specific playlist for each output @HLS@ output that you specify in OutputKeys. To add an output to the master playlist for this job, include it in the @OutputKeys@ of the associated playlist.
--
-- /Note:/ Consider using 'segmentDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjoSegmentDuration :: Lens.Lens' CreateJobOutput (Core.Maybe Types.SegmentDuration)
cjoSegmentDuration = Lens.field @"segmentDuration"
{-# DEPRECATED cjoSegmentDuration "Use generic-lens or generic-optics with 'segmentDuration' instead." #-}

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to your thumbnail.
--
-- /Note:/ Consider using 'thumbnailEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjoThumbnailEncryption :: Lens.Lens' CreateJobOutput (Core.Maybe Types.Encryption)
cjoThumbnailEncryption = Lens.field @"thumbnailEncryption"
{-# DEPRECATED cjoThumbnailEncryption "Use generic-lens or generic-optics with 'thumbnailEncryption' instead." #-}

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
cjoThumbnailPattern :: Lens.Lens' CreateJobOutput (Core.Maybe Types.ThumbnailPattern)
cjoThumbnailPattern = Lens.field @"thumbnailPattern"
{-# DEPRECATED cjoThumbnailPattern "Use generic-lens or generic-optics with 'thumbnailPattern' instead." #-}

-- | Information about the watermarks that you want Elastic Transcoder to add to the video during transcoding. You can specify up to four watermarks for each output. Settings for each watermark must be defined in the preset for the current output.
--
-- /Note:/ Consider using 'watermarks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjoWatermarks :: Lens.Lens' CreateJobOutput (Core.Maybe [Types.JobWatermark])
cjoWatermarks = Lens.field @"watermarks"
{-# DEPRECATED cjoWatermarks "Use generic-lens or generic-optics with 'watermarks' instead." #-}

instance Core.FromJSON CreateJobOutput where
  toJSON CreateJobOutput {..} =
    Core.object
      ( Core.catMaybes
          [ ("AlbumArt" Core..=) Core.<$> albumArt,
            ("Captions" Core..=) Core.<$> captions,
            ("Composition" Core..=) Core.<$> composition,
            ("Encryption" Core..=) Core.<$> encryption,
            ("Key" Core..=) Core.<$> key,
            ("PresetId" Core..=) Core.<$> presetId,
            ("Rotate" Core..=) Core.<$> rotate,
            ("SegmentDuration" Core..=) Core.<$> segmentDuration,
            ("ThumbnailEncryption" Core..=) Core.<$> thumbnailEncryption,
            ("ThumbnailPattern" Core..=) Core.<$> thumbnailPattern,
            ("Watermarks" Core..=) Core.<$> watermarks
          ]
      )
