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
    cjoThumbnailPattern,
    cjoCaptions,
    cjoPresetId,
    cjoComposition,
    cjoAlbumArt,
    cjoWatermarks,
    cjoEncryption,
    cjoKey,
    cjoSegmentDuration,
    cjoThumbnailEncryption,
    cjoRotate,
  )
where

import Network.AWS.ElasticTranscoder.Types.Captions
import Network.AWS.ElasticTranscoder.Types.Clip
import Network.AWS.ElasticTranscoder.Types.Encryption
import Network.AWS.ElasticTranscoder.Types.JobAlbumArt
import Network.AWS.ElasticTranscoder.Types.JobWatermark
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The @CreateJobOutput@ structure.
--
-- /See:/ 'mkCreateJobOutput' smart constructor.
data CreateJobOutput = CreateJobOutput'
  { thumbnailPattern ::
      Lude.Maybe Lude.Text,
    captions :: Lude.Maybe Captions,
    presetId :: Lude.Maybe Lude.Text,
    composition :: Lude.Maybe [Clip],
    albumArt :: Lude.Maybe JobAlbumArt,
    watermarks :: Lude.Maybe [JobWatermark],
    encryption :: Lude.Maybe Encryption,
    key :: Lude.Maybe Lude.Text,
    segmentDuration :: Lude.Maybe Lude.Text,
    thumbnailEncryption :: Lude.Maybe Encryption,
    rotate :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateJobOutput' with the minimum fields required to make a request.
--
-- * 'albumArt' - Information about the album art that you want Elastic Transcoder to add to the file during transcoding. You can specify up to twenty album artworks for each output. Settings for each artwork must be defined in the job for the current output.
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
-- * 'composition' - You can create an output file that contains an excerpt from the input file. This excerpt, called a clip, can come from the beginning, middle, or end of the file. The Composition object contains settings for the clips that make up an output file. For the current release, you can only specify settings for a single clip per output file. The Composition object cannot be null.
-- * 'encryption' - You can specify encryption settings for any output files that you want to use for a transcoding job. This includes the output file and any watermarks, thumbnails, album art, or captions that you want to use. You must specify encryption settings for each file individually.
-- * 'key' - The name to assign to the transcoded file. Elastic Transcoder saves the file in the Amazon S3 bucket specified by the @OutputBucket@ object in the pipeline that is specified by the pipeline ID. If a file with the specified name already exists in the output bucket, the job fails.
-- * 'presetId' - The @Id@ of the preset to use for this job. The preset determines the audio, video, and thumbnail settings that Elastic Transcoder uses for transcoding.
-- * 'rotate' - The number of degrees clockwise by which you want Elastic Transcoder to rotate the output relative to the input. Enter one of the following values: @auto@ , @0@ , @90@ , @180@ , @270@ . The value @auto@ generally works only if the file that you're transcoding contains rotation metadata.
-- * 'segmentDuration' - /Important:/ (Outputs in Fragmented MP4 or MPEG-TS format only.
--
-- If you specify a preset in @PresetId@ for which the value of @Container@ is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), @SegmentDuration@ is the target maximum duration of each segment in seconds. For @HLSv3@ format playlists, each media segment is stored in a separate @.ts@ file. For @HLSv4@ and @Smooth@ playlists, all media segments for an output are stored in a single file. Each segment is approximately the length of the @SegmentDuration@ , though individual segments might be shorter or longer.
-- The range of valid values is 1 to 60 seconds. If the duration of the video is not evenly divisible by @SegmentDuration@ , the duration of the last segment is the remainder of total length/SegmentDuration.
-- Elastic Transcoder creates an output-specific playlist for each output @HLS@ output that you specify in OutputKeys. To add an output to the master playlist for this job, include it in the @OutputKeys@ of the associated playlist.
-- * 'thumbnailEncryption' - The encryption settings, if any, that you want Elastic Transcoder to apply to your thumbnail.
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
-- * 'watermarks' - Information about the watermarks that you want Elastic Transcoder to add to the video during transcoding. You can specify up to four watermarks for each output. Settings for each watermark must be defined in the preset for the current output.
mkCreateJobOutput ::
  CreateJobOutput
mkCreateJobOutput =
  CreateJobOutput'
    { thumbnailPattern = Lude.Nothing,
      captions = Lude.Nothing,
      presetId = Lude.Nothing,
      composition = Lude.Nothing,
      albumArt = Lude.Nothing,
      watermarks = Lude.Nothing,
      encryption = Lude.Nothing,
      key = Lude.Nothing,
      segmentDuration = Lude.Nothing,
      thumbnailEncryption = Lude.Nothing,
      rotate = Lude.Nothing
    }

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
cjoThumbnailPattern :: Lens.Lens' CreateJobOutput (Lude.Maybe Lude.Text)
cjoThumbnailPattern = Lens.lens (thumbnailPattern :: CreateJobOutput -> Lude.Maybe Lude.Text) (\s a -> s {thumbnailPattern = a} :: CreateJobOutput)
{-# DEPRECATED cjoThumbnailPattern "Use generic-lens or generic-optics with 'thumbnailPattern' instead." #-}

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
cjoCaptions :: Lens.Lens' CreateJobOutput (Lude.Maybe Captions)
cjoCaptions = Lens.lens (captions :: CreateJobOutput -> Lude.Maybe Captions) (\s a -> s {captions = a} :: CreateJobOutput)
{-# DEPRECATED cjoCaptions "Use generic-lens or generic-optics with 'captions' instead." #-}

-- | The @Id@ of the preset to use for this job. The preset determines the audio, video, and thumbnail settings that Elastic Transcoder uses for transcoding.
--
-- /Note:/ Consider using 'presetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjoPresetId :: Lens.Lens' CreateJobOutput (Lude.Maybe Lude.Text)
cjoPresetId = Lens.lens (presetId :: CreateJobOutput -> Lude.Maybe Lude.Text) (\s a -> s {presetId = a} :: CreateJobOutput)
{-# DEPRECATED cjoPresetId "Use generic-lens or generic-optics with 'presetId' instead." #-}

-- | You can create an output file that contains an excerpt from the input file. This excerpt, called a clip, can come from the beginning, middle, or end of the file. The Composition object contains settings for the clips that make up an output file. For the current release, you can only specify settings for a single clip per output file. The Composition object cannot be null.
--
-- /Note:/ Consider using 'composition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjoComposition :: Lens.Lens' CreateJobOutput (Lude.Maybe [Clip])
cjoComposition = Lens.lens (composition :: CreateJobOutput -> Lude.Maybe [Clip]) (\s a -> s {composition = a} :: CreateJobOutput)
{-# DEPRECATED cjoComposition "Use generic-lens or generic-optics with 'composition' instead." #-}

-- | Information about the album art that you want Elastic Transcoder to add to the file during transcoding. You can specify up to twenty album artworks for each output. Settings for each artwork must be defined in the job for the current output.
--
-- /Note:/ Consider using 'albumArt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjoAlbumArt :: Lens.Lens' CreateJobOutput (Lude.Maybe JobAlbumArt)
cjoAlbumArt = Lens.lens (albumArt :: CreateJobOutput -> Lude.Maybe JobAlbumArt) (\s a -> s {albumArt = a} :: CreateJobOutput)
{-# DEPRECATED cjoAlbumArt "Use generic-lens or generic-optics with 'albumArt' instead." #-}

-- | Information about the watermarks that you want Elastic Transcoder to add to the video during transcoding. You can specify up to four watermarks for each output. Settings for each watermark must be defined in the preset for the current output.
--
-- /Note:/ Consider using 'watermarks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjoWatermarks :: Lens.Lens' CreateJobOutput (Lude.Maybe [JobWatermark])
cjoWatermarks = Lens.lens (watermarks :: CreateJobOutput -> Lude.Maybe [JobWatermark]) (\s a -> s {watermarks = a} :: CreateJobOutput)
{-# DEPRECATED cjoWatermarks "Use generic-lens or generic-optics with 'watermarks' instead." #-}

-- | You can specify encryption settings for any output files that you want to use for a transcoding job. This includes the output file and any watermarks, thumbnails, album art, or captions that you want to use. You must specify encryption settings for each file individually.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjoEncryption :: Lens.Lens' CreateJobOutput (Lude.Maybe Encryption)
cjoEncryption = Lens.lens (encryption :: CreateJobOutput -> Lude.Maybe Encryption) (\s a -> s {encryption = a} :: CreateJobOutput)
{-# DEPRECATED cjoEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | The name to assign to the transcoded file. Elastic Transcoder saves the file in the Amazon S3 bucket specified by the @OutputBucket@ object in the pipeline that is specified by the pipeline ID. If a file with the specified name already exists in the output bucket, the job fails.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjoKey :: Lens.Lens' CreateJobOutput (Lude.Maybe Lude.Text)
cjoKey = Lens.lens (key :: CreateJobOutput -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: CreateJobOutput)
{-# DEPRECATED cjoKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | /Important:/ (Outputs in Fragmented MP4 or MPEG-TS format only.
--
-- If you specify a preset in @PresetId@ for which the value of @Container@ is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), @SegmentDuration@ is the target maximum duration of each segment in seconds. For @HLSv3@ format playlists, each media segment is stored in a separate @.ts@ file. For @HLSv4@ and @Smooth@ playlists, all media segments for an output are stored in a single file. Each segment is approximately the length of the @SegmentDuration@ , though individual segments might be shorter or longer.
-- The range of valid values is 1 to 60 seconds. If the duration of the video is not evenly divisible by @SegmentDuration@ , the duration of the last segment is the remainder of total length/SegmentDuration.
-- Elastic Transcoder creates an output-specific playlist for each output @HLS@ output that you specify in OutputKeys. To add an output to the master playlist for this job, include it in the @OutputKeys@ of the associated playlist.
--
-- /Note:/ Consider using 'segmentDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjoSegmentDuration :: Lens.Lens' CreateJobOutput (Lude.Maybe Lude.Text)
cjoSegmentDuration = Lens.lens (segmentDuration :: CreateJobOutput -> Lude.Maybe Lude.Text) (\s a -> s {segmentDuration = a} :: CreateJobOutput)
{-# DEPRECATED cjoSegmentDuration "Use generic-lens or generic-optics with 'segmentDuration' instead." #-}

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to your thumbnail.
--
-- /Note:/ Consider using 'thumbnailEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjoThumbnailEncryption :: Lens.Lens' CreateJobOutput (Lude.Maybe Encryption)
cjoThumbnailEncryption = Lens.lens (thumbnailEncryption :: CreateJobOutput -> Lude.Maybe Encryption) (\s a -> s {thumbnailEncryption = a} :: CreateJobOutput)
{-# DEPRECATED cjoThumbnailEncryption "Use generic-lens or generic-optics with 'thumbnailEncryption' instead." #-}

-- | The number of degrees clockwise by which you want Elastic Transcoder to rotate the output relative to the input. Enter one of the following values: @auto@ , @0@ , @90@ , @180@ , @270@ . The value @auto@ generally works only if the file that you're transcoding contains rotation metadata.
--
-- /Note:/ Consider using 'rotate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjoRotate :: Lens.Lens' CreateJobOutput (Lude.Maybe Lude.Text)
cjoRotate = Lens.lens (rotate :: CreateJobOutput -> Lude.Maybe Lude.Text) (\s a -> s {rotate = a} :: CreateJobOutput)
{-# DEPRECATED cjoRotate "Use generic-lens or generic-optics with 'rotate' instead." #-}

instance Lude.ToJSON CreateJobOutput where
  toJSON CreateJobOutput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ThumbnailPattern" Lude..=) Lude.<$> thumbnailPattern,
            ("Captions" Lude..=) Lude.<$> captions,
            ("PresetId" Lude..=) Lude.<$> presetId,
            ("Composition" Lude..=) Lude.<$> composition,
            ("AlbumArt" Lude..=) Lude.<$> albumArt,
            ("Watermarks" Lude..=) Lude.<$> watermarks,
            ("Encryption" Lude..=) Lude.<$> encryption,
            ("Key" Lude..=) Lude.<$> key,
            ("SegmentDuration" Lude..=) Lude.<$> segmentDuration,
            ("ThumbnailEncryption" Lude..=) Lude.<$> thumbnailEncryption,
            ("Rotate" Lude..=) Lude.<$> rotate
          ]
      )
