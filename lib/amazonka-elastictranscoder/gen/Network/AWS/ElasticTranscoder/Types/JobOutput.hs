{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.JobOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.JobOutput where

import Network.AWS.ElasticTranscoder.Types.Captions
import Network.AWS.ElasticTranscoder.Types.Clip
import Network.AWS.ElasticTranscoder.Types.Encryption
import Network.AWS.ElasticTranscoder.Types.JobAlbumArt
import Network.AWS.ElasticTranscoder.Types.JobWatermark
import Network.AWS.Lens
import Network.AWS.Prelude

-- | /Important:/ Outputs recommended instead.
--
--
-- If you specified one output for a job, information about that output. If you specified multiple outputs for a job, the @Output@ object lists information about the first output. This duplicates the information that is listed for the first output in the @Outputs@ object.
--
--
-- /See:/ 'jobOutput' smart constructor.
data JobOutput = JobOutput'
  { _joAppliedColorSpaceConversion ::
      !(Maybe Text),
    _joThumbnailPattern :: !(Maybe Text),
    _joStatus :: !(Maybe Text),
    _joHeight :: !(Maybe Int),
    _joFrameRate :: !(Maybe Text),
    _joCaptions :: !(Maybe Captions),
    _joPresetId :: !(Maybe Text),
    _joComposition :: !(Maybe [Clip]),
    _joAlbumArt :: !(Maybe JobAlbumArt),
    _joFileSize :: !(Maybe Integer),
    _joWatermarks :: !(Maybe [JobWatermark]),
    _joWidth :: !(Maybe Int),
    _joEncryption :: !(Maybe Encryption),
    _joKey :: !(Maybe Text),
    _joStatusDetail :: !(Maybe Text),
    _joId :: !(Maybe Text),
    _joSegmentDuration :: !(Maybe Text),
    _joDurationMillis :: !(Maybe Integer),
    _joThumbnailEncryption :: !(Maybe Encryption),
    _joDuration :: !(Maybe Integer),
    _joRotate :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'joAppliedColorSpaceConversion' - If Elastic Transcoder used a preset with a @ColorSpaceConversionMode@ to transcode the output file, the @AppliedColorSpaceConversion@ parameter shows the conversion used. If no @ColorSpaceConversionMode@ was defined in the preset, this parameter is not be included in the job response.
--
-- * 'joThumbnailPattern' - Whether you want Elastic Transcoder to create thumbnails for your videos and, if so, how you want Elastic Transcoder to name the files. If you don't want Elastic Transcoder to create thumbnails, specify "". If you do want Elastic Transcoder to create thumbnails, specify the information that you want to include in the file name for each thumbnail. You can specify the following values in any sequence:     * __@{count}@ (Required)__ : If you want to create thumbnails, you must include @{count}@ in the @ThumbnailPattern@ object. Wherever you specify @{count}@ , Elastic Transcoder adds a five-digit sequence number (beginning with __00001__ ) to thumbnail file names. The number indicates where a given thumbnail appears in the sequence of thumbnails for a transcoded file.  /Important:/ If you specify a literal value and/or @{resolution}@ but you omit @{count}@ , Elastic Transcoder returns a validation error and does not create the job.     * __Literal values (Optional)__ : You can specify literal values anywhere in the @ThumbnailPattern@ object. For example, you can include them as a file name prefix or as a delimiter between @{resolution}@ and @{count}@ .      * __@{resolution}@ (Optional)__ : If you want Elastic Transcoder to include the resolution in the file name, include @{resolution}@ in the @ThumbnailPattern@ object.  When creating thumbnails, Elastic Transcoder automatically saves the files in the format (.jpg or .png) that appears in the preset that you specified in the @PresetID@ value of @CreateJobOutput@ . Elastic Transcoder also appends the applicable file name extension.
--
-- * 'joStatus' - The status of one output in a job. If you specified only one output for the job, @Outputs:Status@ is always the same as @Job:Status@ . If you specified more than one output:      * @Job:Status@ and @Outputs:Status@ for all of the outputs is Submitted until Elastic Transcoder starts to process the first output.     * When Elastic Transcoder starts to process the first output, @Outputs:Status@ for that output and @Job:Status@ both change to Progressing. For each output, the value of @Outputs:Status@ remains Submitted until Elastic Transcoder starts to process the output.     * Job:Status remains Progressing until all of the outputs reach a terminal status, either Complete or Error.     * When all of the outputs reach a terminal status, @Job:Status@ changes to Complete only if @Outputs:Status@ for all of the outputs is @Complete@ . If @Outputs:Status@ for one or more outputs is @Error@ , the terminal status for @Job:Status@ is also @Error@ . The value of @Status@ is one of the following: @Submitted@ , @Progressing@ , @Complete@ , @Canceled@ , or @Error@ .
--
-- * 'joHeight' - Height of the output file, in pixels.
--
-- * 'joFrameRate' - Frame rate of the output file, in frames per second.
--
-- * 'joCaptions' - You can configure Elastic Transcoder to transcode captions, or subtitles, from one format to another. All captions must be in UTF-8. Elastic Transcoder supports two types of captions:     * __Embedded:__ Embedded captions are included in the same file as the audio and video. Elastic Transcoder supports only one embedded caption per language, to a maximum of 300 embedded captions per file. Valid input values include: @CEA-608 (EIA-608@ , first non-empty channel only), @CEA-708 (EIA-708@ , first non-empty channel only), and @mov-text@  Valid outputs include: @mov-text@  Elastic Transcoder supports a maximum of one embedded format per output.     * __Sidecar:__ Sidecar captions are kept in a separate metadata file from the audio and video data. Sidecar captions require a player that is capable of understanding the relationship between the video file and the sidecar file. Elastic Transcoder supports only one sidecar caption per language, to a maximum of 20 sidecar captions per file. Valid input values include: @dfxp@ (first div element only), @ebu-tt@ , @scc@ , @smpt@ , @srt@ , @ttml@ (first div element only), and @webvtt@  Valid outputs include: @dfxp@ (first div element only), @scc@ , @srt@ , and @webvtt@ . If you want ttml or smpte-tt compatible captions, specify dfxp as your output format. Elastic Transcoder does not support OCR (Optical Character Recognition), does not accept pictures as a valid input for captions, and is not available for audio-only transcoding. Elastic Transcoder does not preserve text formatting (for example, italics) during the transcoding process. To remove captions or leave the captions empty, set @Captions@ to null. To pass through existing captions unchanged, set the @MergePolicy@ to @MergeRetain@ , and pass in a null @CaptionSources@ array. For more information on embedded files, see the Subtitles Wikipedia page. For more information on sidecar files, see the Extensible Metadata Platform and Sidecar file Wikipedia pages.
--
-- * 'joPresetId' - The value of the @Id@ object for the preset that you want to use for this job. The preset determines the audio, video, and thumbnail settings that Elastic Transcoder uses for transcoding. To use a preset that you created, specify the preset ID that Elastic Transcoder returned in the response when you created the preset. You can also use the Elastic Transcoder system presets, which you can get with @ListPresets@ .
--
-- * 'joComposition' - You can create an output file that contains an excerpt from the input file. This excerpt, called a clip, can come from the beginning, middle, or end of the file. The Composition object contains settings for the clips that make up an output file. For the current release, you can only specify settings for a single clip per output file. The Composition object cannot be null.
--
-- * 'joAlbumArt' - The album art to be associated with the output file, if any.
--
-- * 'joFileSize' - File size of the output file, in bytes.
--
-- * 'joWatermarks' - Information about the watermarks that you want Elastic Transcoder to add to the video during transcoding. You can specify up to four watermarks for each output. Settings for each watermark must be defined in the preset that you specify in @Preset@ for the current output. Watermarks are added to the output video in the sequence in which you list them in the job output—the first watermark in the list is added to the output video first, the second watermark in the list is added next, and so on. As a result, if the settings in a preset cause Elastic Transcoder to place all watermarks in the same location, the second watermark that you add covers the first one, the third one covers the second, and the fourth one covers the third.
--
-- * 'joWidth' - Specifies the width of the output file in pixels.
--
-- * 'joEncryption' - The encryption settings, if any, that you want Elastic Transcoder to apply to your output files. If you choose to use encryption, you must specify a mode to use. If you choose not to use encryption, Elastic Transcoder writes an unencrypted file to your Amazon S3 bucket.
--
-- * 'joKey' - The name to assign to the transcoded file. Elastic Transcoder saves the file in the Amazon S3 bucket specified by the @OutputBucket@ object in the pipeline that is specified by the pipeline ID.
--
-- * 'joStatusDetail' - Information that further explains @Status@ .
--
-- * 'joId' - A sequential counter, starting with 1, that identifies an output among the outputs from the current job. In the Output syntax, this value is always 1.
--
-- * 'joSegmentDuration' - /Important:/ (Outputs in Fragmented MP4 or MPEG-TS format only. If you specify a preset in @PresetId@ for which the value of @Container@ is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), @SegmentDuration@ is the target maximum duration of each segment in seconds. For @HLSv3@ format playlists, each media segment is stored in a separate @.ts@ file. For @HLSv4@ , @MPEG-DASH@ , and @Smooth@ playlists, all media segments for an output are stored in a single file. Each segment is approximately the length of the @SegmentDuration@ , though individual segments might be shorter or longer. The range of valid values is 1 to 60 seconds. If the duration of the video is not evenly divisible by @SegmentDuration@ , the duration of the last segment is the remainder of total length/SegmentDuration. Elastic Transcoder creates an output-specific playlist for each output @HLS@ output that you specify in OutputKeys. To add an output to the master playlist for this job, include it in the @OutputKeys@ of the associated playlist.
--
-- * 'joDurationMillis' - Duration of the output file, in milliseconds.
--
-- * 'joThumbnailEncryption' - The encryption settings, if any, that you want Elastic Transcoder to apply to your thumbnail.
--
-- * 'joDuration' - Duration of the output file, in seconds.
--
-- * 'joRotate' - The number of degrees clockwise by which you want Elastic Transcoder to rotate the output relative to the input. Enter one of the following values: @auto@ , @0@ , @90@ , @180@ , @270@  The value @auto@ generally works only if the file that you're transcoding contains rotation metadata.
jobOutput ::
  JobOutput
jobOutput =
  JobOutput'
    { _joAppliedColorSpaceConversion = Nothing,
      _joThumbnailPattern = Nothing,
      _joStatus = Nothing,
      _joHeight = Nothing,
      _joFrameRate = Nothing,
      _joCaptions = Nothing,
      _joPresetId = Nothing,
      _joComposition = Nothing,
      _joAlbumArt = Nothing,
      _joFileSize = Nothing,
      _joWatermarks = Nothing,
      _joWidth = Nothing,
      _joEncryption = Nothing,
      _joKey = Nothing,
      _joStatusDetail = Nothing,
      _joId = Nothing,
      _joSegmentDuration = Nothing,
      _joDurationMillis = Nothing,
      _joThumbnailEncryption = Nothing,
      _joDuration = Nothing,
      _joRotate = Nothing
    }

-- | If Elastic Transcoder used a preset with a @ColorSpaceConversionMode@ to transcode the output file, the @AppliedColorSpaceConversion@ parameter shows the conversion used. If no @ColorSpaceConversionMode@ was defined in the preset, this parameter is not be included in the job response.
joAppliedColorSpaceConversion :: Lens' JobOutput (Maybe Text)
joAppliedColorSpaceConversion = lens _joAppliedColorSpaceConversion (\s a -> s {_joAppliedColorSpaceConversion = a})

-- | Whether you want Elastic Transcoder to create thumbnails for your videos and, if so, how you want Elastic Transcoder to name the files. If you don't want Elastic Transcoder to create thumbnails, specify "". If you do want Elastic Transcoder to create thumbnails, specify the information that you want to include in the file name for each thumbnail. You can specify the following values in any sequence:     * __@{count}@ (Required)__ : If you want to create thumbnails, you must include @{count}@ in the @ThumbnailPattern@ object. Wherever you specify @{count}@ , Elastic Transcoder adds a five-digit sequence number (beginning with __00001__ ) to thumbnail file names. The number indicates where a given thumbnail appears in the sequence of thumbnails for a transcoded file.  /Important:/ If you specify a literal value and/or @{resolution}@ but you omit @{count}@ , Elastic Transcoder returns a validation error and does not create the job.     * __Literal values (Optional)__ : You can specify literal values anywhere in the @ThumbnailPattern@ object. For example, you can include them as a file name prefix or as a delimiter between @{resolution}@ and @{count}@ .      * __@{resolution}@ (Optional)__ : If you want Elastic Transcoder to include the resolution in the file name, include @{resolution}@ in the @ThumbnailPattern@ object.  When creating thumbnails, Elastic Transcoder automatically saves the files in the format (.jpg or .png) that appears in the preset that you specified in the @PresetID@ value of @CreateJobOutput@ . Elastic Transcoder also appends the applicable file name extension.
joThumbnailPattern :: Lens' JobOutput (Maybe Text)
joThumbnailPattern = lens _joThumbnailPattern (\s a -> s {_joThumbnailPattern = a})

-- | The status of one output in a job. If you specified only one output for the job, @Outputs:Status@ is always the same as @Job:Status@ . If you specified more than one output:      * @Job:Status@ and @Outputs:Status@ for all of the outputs is Submitted until Elastic Transcoder starts to process the first output.     * When Elastic Transcoder starts to process the first output, @Outputs:Status@ for that output and @Job:Status@ both change to Progressing. For each output, the value of @Outputs:Status@ remains Submitted until Elastic Transcoder starts to process the output.     * Job:Status remains Progressing until all of the outputs reach a terminal status, either Complete or Error.     * When all of the outputs reach a terminal status, @Job:Status@ changes to Complete only if @Outputs:Status@ for all of the outputs is @Complete@ . If @Outputs:Status@ for one or more outputs is @Error@ , the terminal status for @Job:Status@ is also @Error@ . The value of @Status@ is one of the following: @Submitted@ , @Progressing@ , @Complete@ , @Canceled@ , or @Error@ .
joStatus :: Lens' JobOutput (Maybe Text)
joStatus = lens _joStatus (\s a -> s {_joStatus = a})

-- | Height of the output file, in pixels.
joHeight :: Lens' JobOutput (Maybe Int)
joHeight = lens _joHeight (\s a -> s {_joHeight = a})

-- | Frame rate of the output file, in frames per second.
joFrameRate :: Lens' JobOutput (Maybe Text)
joFrameRate = lens _joFrameRate (\s a -> s {_joFrameRate = a})

-- | You can configure Elastic Transcoder to transcode captions, or subtitles, from one format to another. All captions must be in UTF-8. Elastic Transcoder supports two types of captions:     * __Embedded:__ Embedded captions are included in the same file as the audio and video. Elastic Transcoder supports only one embedded caption per language, to a maximum of 300 embedded captions per file. Valid input values include: @CEA-608 (EIA-608@ , first non-empty channel only), @CEA-708 (EIA-708@ , first non-empty channel only), and @mov-text@  Valid outputs include: @mov-text@  Elastic Transcoder supports a maximum of one embedded format per output.     * __Sidecar:__ Sidecar captions are kept in a separate metadata file from the audio and video data. Sidecar captions require a player that is capable of understanding the relationship between the video file and the sidecar file. Elastic Transcoder supports only one sidecar caption per language, to a maximum of 20 sidecar captions per file. Valid input values include: @dfxp@ (first div element only), @ebu-tt@ , @scc@ , @smpt@ , @srt@ , @ttml@ (first div element only), and @webvtt@  Valid outputs include: @dfxp@ (first div element only), @scc@ , @srt@ , and @webvtt@ . If you want ttml or smpte-tt compatible captions, specify dfxp as your output format. Elastic Transcoder does not support OCR (Optical Character Recognition), does not accept pictures as a valid input for captions, and is not available for audio-only transcoding. Elastic Transcoder does not preserve text formatting (for example, italics) during the transcoding process. To remove captions or leave the captions empty, set @Captions@ to null. To pass through existing captions unchanged, set the @MergePolicy@ to @MergeRetain@ , and pass in a null @CaptionSources@ array. For more information on embedded files, see the Subtitles Wikipedia page. For more information on sidecar files, see the Extensible Metadata Platform and Sidecar file Wikipedia pages.
joCaptions :: Lens' JobOutput (Maybe Captions)
joCaptions = lens _joCaptions (\s a -> s {_joCaptions = a})

-- | The value of the @Id@ object for the preset that you want to use for this job. The preset determines the audio, video, and thumbnail settings that Elastic Transcoder uses for transcoding. To use a preset that you created, specify the preset ID that Elastic Transcoder returned in the response when you created the preset. You can also use the Elastic Transcoder system presets, which you can get with @ListPresets@ .
joPresetId :: Lens' JobOutput (Maybe Text)
joPresetId = lens _joPresetId (\s a -> s {_joPresetId = a})

-- | You can create an output file that contains an excerpt from the input file. This excerpt, called a clip, can come from the beginning, middle, or end of the file. The Composition object contains settings for the clips that make up an output file. For the current release, you can only specify settings for a single clip per output file. The Composition object cannot be null.
joComposition :: Lens' JobOutput [Clip]
joComposition = lens _joComposition (\s a -> s {_joComposition = a}) . _Default . _Coerce

-- | The album art to be associated with the output file, if any.
joAlbumArt :: Lens' JobOutput (Maybe JobAlbumArt)
joAlbumArt = lens _joAlbumArt (\s a -> s {_joAlbumArt = a})

-- | File size of the output file, in bytes.
joFileSize :: Lens' JobOutput (Maybe Integer)
joFileSize = lens _joFileSize (\s a -> s {_joFileSize = a})

-- | Information about the watermarks that you want Elastic Transcoder to add to the video during transcoding. You can specify up to four watermarks for each output. Settings for each watermark must be defined in the preset that you specify in @Preset@ for the current output. Watermarks are added to the output video in the sequence in which you list them in the job output—the first watermark in the list is added to the output video first, the second watermark in the list is added next, and so on. As a result, if the settings in a preset cause Elastic Transcoder to place all watermarks in the same location, the second watermark that you add covers the first one, the third one covers the second, and the fourth one covers the third.
joWatermarks :: Lens' JobOutput [JobWatermark]
joWatermarks = lens _joWatermarks (\s a -> s {_joWatermarks = a}) . _Default . _Coerce

-- | Specifies the width of the output file in pixels.
joWidth :: Lens' JobOutput (Maybe Int)
joWidth = lens _joWidth (\s a -> s {_joWidth = a})

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to your output files. If you choose to use encryption, you must specify a mode to use. If you choose not to use encryption, Elastic Transcoder writes an unencrypted file to your Amazon S3 bucket.
joEncryption :: Lens' JobOutput (Maybe Encryption)
joEncryption = lens _joEncryption (\s a -> s {_joEncryption = a})

-- | The name to assign to the transcoded file. Elastic Transcoder saves the file in the Amazon S3 bucket specified by the @OutputBucket@ object in the pipeline that is specified by the pipeline ID.
joKey :: Lens' JobOutput (Maybe Text)
joKey = lens _joKey (\s a -> s {_joKey = a})

-- | Information that further explains @Status@ .
joStatusDetail :: Lens' JobOutput (Maybe Text)
joStatusDetail = lens _joStatusDetail (\s a -> s {_joStatusDetail = a})

-- | A sequential counter, starting with 1, that identifies an output among the outputs from the current job. In the Output syntax, this value is always 1.
joId :: Lens' JobOutput (Maybe Text)
joId = lens _joId (\s a -> s {_joId = a})

-- | /Important:/ (Outputs in Fragmented MP4 or MPEG-TS format only. If you specify a preset in @PresetId@ for which the value of @Container@ is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), @SegmentDuration@ is the target maximum duration of each segment in seconds. For @HLSv3@ format playlists, each media segment is stored in a separate @.ts@ file. For @HLSv4@ , @MPEG-DASH@ , and @Smooth@ playlists, all media segments for an output are stored in a single file. Each segment is approximately the length of the @SegmentDuration@ , though individual segments might be shorter or longer. The range of valid values is 1 to 60 seconds. If the duration of the video is not evenly divisible by @SegmentDuration@ , the duration of the last segment is the remainder of total length/SegmentDuration. Elastic Transcoder creates an output-specific playlist for each output @HLS@ output that you specify in OutputKeys. To add an output to the master playlist for this job, include it in the @OutputKeys@ of the associated playlist.
joSegmentDuration :: Lens' JobOutput (Maybe Text)
joSegmentDuration = lens _joSegmentDuration (\s a -> s {_joSegmentDuration = a})

-- | Duration of the output file, in milliseconds.
joDurationMillis :: Lens' JobOutput (Maybe Integer)
joDurationMillis = lens _joDurationMillis (\s a -> s {_joDurationMillis = a})

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to your thumbnail.
joThumbnailEncryption :: Lens' JobOutput (Maybe Encryption)
joThumbnailEncryption = lens _joThumbnailEncryption (\s a -> s {_joThumbnailEncryption = a})

-- | Duration of the output file, in seconds.
joDuration :: Lens' JobOutput (Maybe Integer)
joDuration = lens _joDuration (\s a -> s {_joDuration = a})

-- | The number of degrees clockwise by which you want Elastic Transcoder to rotate the output relative to the input. Enter one of the following values: @auto@ , @0@ , @90@ , @180@ , @270@  The value @auto@ generally works only if the file that you're transcoding contains rotation metadata.
joRotate :: Lens' JobOutput (Maybe Text)
joRotate = lens _joRotate (\s a -> s {_joRotate = a})

instance FromJSON JobOutput where
  parseJSON =
    withObject
      "JobOutput"
      ( \x ->
          JobOutput'
            <$> (x .:? "AppliedColorSpaceConversion")
            <*> (x .:? "ThumbnailPattern")
            <*> (x .:? "Status")
            <*> (x .:? "Height")
            <*> (x .:? "FrameRate")
            <*> (x .:? "Captions")
            <*> (x .:? "PresetId")
            <*> (x .:? "Composition" .!= mempty)
            <*> (x .:? "AlbumArt")
            <*> (x .:? "FileSize")
            <*> (x .:? "Watermarks" .!= mempty)
            <*> (x .:? "Width")
            <*> (x .:? "Encryption")
            <*> (x .:? "Key")
            <*> (x .:? "StatusDetail")
            <*> (x .:? "Id")
            <*> (x .:? "SegmentDuration")
            <*> (x .:? "DurationMillis")
            <*> (x .:? "ThumbnailEncryption")
            <*> (x .:? "Duration")
            <*> (x .:? "Rotate")
      )

instance Hashable JobOutput

instance NFData JobOutput
