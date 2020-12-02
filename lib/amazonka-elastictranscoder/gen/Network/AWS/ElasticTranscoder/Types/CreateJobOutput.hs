{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.CreateJobOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.CreateJobOutput where

import Network.AWS.ElasticTranscoder.Types.Captions
import Network.AWS.ElasticTranscoder.Types.Clip
import Network.AWS.ElasticTranscoder.Types.Encryption
import Network.AWS.ElasticTranscoder.Types.JobAlbumArt
import Network.AWS.ElasticTranscoder.Types.JobWatermark
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The @CreateJobOutput@ structure.
--
--
--
-- /See:/ 'createJobOutput' smart constructor.
data CreateJobOutput = CreateJobOutput'
  { _cjoThumbnailPattern ::
      !(Maybe Text),
    _cjoCaptions :: !(Maybe Captions),
    _cjoPresetId :: !(Maybe Text),
    _cjoComposition :: !(Maybe [Clip]),
    _cjoAlbumArt :: !(Maybe JobAlbumArt),
    _cjoWatermarks :: !(Maybe [JobWatermark]),
    _cjoEncryption :: !(Maybe Encryption),
    _cjoKey :: !(Maybe Text),
    _cjoSegmentDuration :: !(Maybe Text),
    _cjoThumbnailEncryption :: !(Maybe Encryption),
    _cjoRotate :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateJobOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjoThumbnailPattern' - Whether you want Elastic Transcoder to create thumbnails for your videos and, if so, how you want Elastic Transcoder to name the files. If you don't want Elastic Transcoder to create thumbnails, specify "". If you do want Elastic Transcoder to create thumbnails, specify the information that you want to include in the file name for each thumbnail. You can specify the following values in any sequence:     * __@{count}@ (Required)__ : If you want to create thumbnails, you must include @{count}@ in the @ThumbnailPattern@ object. Wherever you specify @{count}@ , Elastic Transcoder adds a five-digit sequence number (beginning with __00001__ ) to thumbnail file names. The number indicates where a given thumbnail appears in the sequence of thumbnails for a transcoded file.  /Important:/ If you specify a literal value and/or @{resolution}@ but you omit @{count}@ , Elastic Transcoder returns a validation error and does not create the job.     * __Literal values (Optional)__ : You can specify literal values anywhere in the @ThumbnailPattern@ object. For example, you can include them as a file name prefix or as a delimiter between @{resolution}@ and @{count}@ .      * __@{resolution}@ (Optional)__ : If you want Elastic Transcoder to include the resolution in the file name, include @{resolution}@ in the @ThumbnailPattern@ object.  When creating thumbnails, Elastic Transcoder automatically saves the files in the format (.jpg or .png) that appears in the preset that you specified in the @PresetID@ value of @CreateJobOutput@ . Elastic Transcoder also appends the applicable file name extension.
--
-- * 'cjoCaptions' - You can configure Elastic Transcoder to transcode captions, or subtitles, from one format to another. All captions must be in UTF-8. Elastic Transcoder supports two types of captions:     * __Embedded:__ Embedded captions are included in the same file as the audio and video. Elastic Transcoder supports only one embedded caption per language, to a maximum of 300 embedded captions per file. Valid input values include: @CEA-608 (EIA-608@ , first non-empty channel only), @CEA-708 (EIA-708@ , first non-empty channel only), and @mov-text@  Valid outputs include: @mov-text@  Elastic Transcoder supports a maximum of one embedded format per output.     * __Sidecar:__ Sidecar captions are kept in a separate metadata file from the audio and video data. Sidecar captions require a player that is capable of understanding the relationship between the video file and the sidecar file. Elastic Transcoder supports only one sidecar caption per language, to a maximum of 20 sidecar captions per file. Valid input values include: @dfxp@ (first div element only), @ebu-tt@ , @scc@ , @smpt@ , @srt@ , @ttml@ (first div element only), and @webvtt@  Valid outputs include: @dfxp@ (first div element only), @scc@ , @srt@ , and @webvtt@ . If you want ttml or smpte-tt compatible captions, specify dfxp as your output format. Elastic Transcoder does not support OCR (Optical Character Recognition), does not accept pictures as a valid input for captions, and is not available for audio-only transcoding. Elastic Transcoder does not preserve text formatting (for example, italics) during the transcoding process. To remove captions or leave the captions empty, set @Captions@ to null. To pass through existing captions unchanged, set the @MergePolicy@ to @MergeRetain@ , and pass in a null @CaptionSources@ array. For more information on embedded files, see the Subtitles Wikipedia page. For more information on sidecar files, see the Extensible Metadata Platform and Sidecar file Wikipedia pages.
--
-- * 'cjoPresetId' - The @Id@ of the preset to use for this job. The preset determines the audio, video, and thumbnail settings that Elastic Transcoder uses for transcoding.
--
-- * 'cjoComposition' - You can create an output file that contains an excerpt from the input file. This excerpt, called a clip, can come from the beginning, middle, or end of the file. The Composition object contains settings for the clips that make up an output file. For the current release, you can only specify settings for a single clip per output file. The Composition object cannot be null.
--
-- * 'cjoAlbumArt' - Information about the album art that you want Elastic Transcoder to add to the file during transcoding. You can specify up to twenty album artworks for each output. Settings for each artwork must be defined in the job for the current output.
--
-- * 'cjoWatermarks' - Information about the watermarks that you want Elastic Transcoder to add to the video during transcoding. You can specify up to four watermarks for each output. Settings for each watermark must be defined in the preset for the current output.
--
-- * 'cjoEncryption' - You can specify encryption settings for any output files that you want to use for a transcoding job. This includes the output file and any watermarks, thumbnails, album art, or captions that you want to use. You must specify encryption settings for each file individually.
--
-- * 'cjoKey' - The name to assign to the transcoded file. Elastic Transcoder saves the file in the Amazon S3 bucket specified by the @OutputBucket@ object in the pipeline that is specified by the pipeline ID. If a file with the specified name already exists in the output bucket, the job fails.
--
-- * 'cjoSegmentDuration' - /Important:/ (Outputs in Fragmented MP4 or MPEG-TS format only. If you specify a preset in @PresetId@ for which the value of @Container@ is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), @SegmentDuration@ is the target maximum duration of each segment in seconds. For @HLSv3@ format playlists, each media segment is stored in a separate @.ts@ file. For @HLSv4@ and @Smooth@ playlists, all media segments for an output are stored in a single file. Each segment is approximately the length of the @SegmentDuration@ , though individual segments might be shorter or longer. The range of valid values is 1 to 60 seconds. If the duration of the video is not evenly divisible by @SegmentDuration@ , the duration of the last segment is the remainder of total length/SegmentDuration. Elastic Transcoder creates an output-specific playlist for each output @HLS@ output that you specify in OutputKeys. To add an output to the master playlist for this job, include it in the @OutputKeys@ of the associated playlist.
--
-- * 'cjoThumbnailEncryption' - The encryption settings, if any, that you want Elastic Transcoder to apply to your thumbnail.
--
-- * 'cjoRotate' - The number of degrees clockwise by which you want Elastic Transcoder to rotate the output relative to the input. Enter one of the following values: @auto@ , @0@ , @90@ , @180@ , @270@ . The value @auto@ generally works only if the file that you're transcoding contains rotation metadata.
createJobOutput ::
  CreateJobOutput
createJobOutput =
  CreateJobOutput'
    { _cjoThumbnailPattern = Nothing,
      _cjoCaptions = Nothing,
      _cjoPresetId = Nothing,
      _cjoComposition = Nothing,
      _cjoAlbumArt = Nothing,
      _cjoWatermarks = Nothing,
      _cjoEncryption = Nothing,
      _cjoKey = Nothing,
      _cjoSegmentDuration = Nothing,
      _cjoThumbnailEncryption = Nothing,
      _cjoRotate = Nothing
    }

-- | Whether you want Elastic Transcoder to create thumbnails for your videos and, if so, how you want Elastic Transcoder to name the files. If you don't want Elastic Transcoder to create thumbnails, specify "". If you do want Elastic Transcoder to create thumbnails, specify the information that you want to include in the file name for each thumbnail. You can specify the following values in any sequence:     * __@{count}@ (Required)__ : If you want to create thumbnails, you must include @{count}@ in the @ThumbnailPattern@ object. Wherever you specify @{count}@ , Elastic Transcoder adds a five-digit sequence number (beginning with __00001__ ) to thumbnail file names. The number indicates where a given thumbnail appears in the sequence of thumbnails for a transcoded file.  /Important:/ If you specify a literal value and/or @{resolution}@ but you omit @{count}@ , Elastic Transcoder returns a validation error and does not create the job.     * __Literal values (Optional)__ : You can specify literal values anywhere in the @ThumbnailPattern@ object. For example, you can include them as a file name prefix or as a delimiter between @{resolution}@ and @{count}@ .      * __@{resolution}@ (Optional)__ : If you want Elastic Transcoder to include the resolution in the file name, include @{resolution}@ in the @ThumbnailPattern@ object.  When creating thumbnails, Elastic Transcoder automatically saves the files in the format (.jpg or .png) that appears in the preset that you specified in the @PresetID@ value of @CreateJobOutput@ . Elastic Transcoder also appends the applicable file name extension.
cjoThumbnailPattern :: Lens' CreateJobOutput (Maybe Text)
cjoThumbnailPattern = lens _cjoThumbnailPattern (\s a -> s {_cjoThumbnailPattern = a})

-- | You can configure Elastic Transcoder to transcode captions, or subtitles, from one format to another. All captions must be in UTF-8. Elastic Transcoder supports two types of captions:     * __Embedded:__ Embedded captions are included in the same file as the audio and video. Elastic Transcoder supports only one embedded caption per language, to a maximum of 300 embedded captions per file. Valid input values include: @CEA-608 (EIA-608@ , first non-empty channel only), @CEA-708 (EIA-708@ , first non-empty channel only), and @mov-text@  Valid outputs include: @mov-text@  Elastic Transcoder supports a maximum of one embedded format per output.     * __Sidecar:__ Sidecar captions are kept in a separate metadata file from the audio and video data. Sidecar captions require a player that is capable of understanding the relationship between the video file and the sidecar file. Elastic Transcoder supports only one sidecar caption per language, to a maximum of 20 sidecar captions per file. Valid input values include: @dfxp@ (first div element only), @ebu-tt@ , @scc@ , @smpt@ , @srt@ , @ttml@ (first div element only), and @webvtt@  Valid outputs include: @dfxp@ (first div element only), @scc@ , @srt@ , and @webvtt@ . If you want ttml or smpte-tt compatible captions, specify dfxp as your output format. Elastic Transcoder does not support OCR (Optical Character Recognition), does not accept pictures as a valid input for captions, and is not available for audio-only transcoding. Elastic Transcoder does not preserve text formatting (for example, italics) during the transcoding process. To remove captions or leave the captions empty, set @Captions@ to null. To pass through existing captions unchanged, set the @MergePolicy@ to @MergeRetain@ , and pass in a null @CaptionSources@ array. For more information on embedded files, see the Subtitles Wikipedia page. For more information on sidecar files, see the Extensible Metadata Platform and Sidecar file Wikipedia pages.
cjoCaptions :: Lens' CreateJobOutput (Maybe Captions)
cjoCaptions = lens _cjoCaptions (\s a -> s {_cjoCaptions = a})

-- | The @Id@ of the preset to use for this job. The preset determines the audio, video, and thumbnail settings that Elastic Transcoder uses for transcoding.
cjoPresetId :: Lens' CreateJobOutput (Maybe Text)
cjoPresetId = lens _cjoPresetId (\s a -> s {_cjoPresetId = a})

-- | You can create an output file that contains an excerpt from the input file. This excerpt, called a clip, can come from the beginning, middle, or end of the file. The Composition object contains settings for the clips that make up an output file. For the current release, you can only specify settings for a single clip per output file. The Composition object cannot be null.
cjoComposition :: Lens' CreateJobOutput [Clip]
cjoComposition = lens _cjoComposition (\s a -> s {_cjoComposition = a}) . _Default . _Coerce

-- | Information about the album art that you want Elastic Transcoder to add to the file during transcoding. You can specify up to twenty album artworks for each output. Settings for each artwork must be defined in the job for the current output.
cjoAlbumArt :: Lens' CreateJobOutput (Maybe JobAlbumArt)
cjoAlbumArt = lens _cjoAlbumArt (\s a -> s {_cjoAlbumArt = a})

-- | Information about the watermarks that you want Elastic Transcoder to add to the video during transcoding. You can specify up to four watermarks for each output. Settings for each watermark must be defined in the preset for the current output.
cjoWatermarks :: Lens' CreateJobOutput [JobWatermark]
cjoWatermarks = lens _cjoWatermarks (\s a -> s {_cjoWatermarks = a}) . _Default . _Coerce

-- | You can specify encryption settings for any output files that you want to use for a transcoding job. This includes the output file and any watermarks, thumbnails, album art, or captions that you want to use. You must specify encryption settings for each file individually.
cjoEncryption :: Lens' CreateJobOutput (Maybe Encryption)
cjoEncryption = lens _cjoEncryption (\s a -> s {_cjoEncryption = a})

-- | The name to assign to the transcoded file. Elastic Transcoder saves the file in the Amazon S3 bucket specified by the @OutputBucket@ object in the pipeline that is specified by the pipeline ID. If a file with the specified name already exists in the output bucket, the job fails.
cjoKey :: Lens' CreateJobOutput (Maybe Text)
cjoKey = lens _cjoKey (\s a -> s {_cjoKey = a})

-- | /Important:/ (Outputs in Fragmented MP4 or MPEG-TS format only. If you specify a preset in @PresetId@ for which the value of @Container@ is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), @SegmentDuration@ is the target maximum duration of each segment in seconds. For @HLSv3@ format playlists, each media segment is stored in a separate @.ts@ file. For @HLSv4@ and @Smooth@ playlists, all media segments for an output are stored in a single file. Each segment is approximately the length of the @SegmentDuration@ , though individual segments might be shorter or longer. The range of valid values is 1 to 60 seconds. If the duration of the video is not evenly divisible by @SegmentDuration@ , the duration of the last segment is the remainder of total length/SegmentDuration. Elastic Transcoder creates an output-specific playlist for each output @HLS@ output that you specify in OutputKeys. To add an output to the master playlist for this job, include it in the @OutputKeys@ of the associated playlist.
cjoSegmentDuration :: Lens' CreateJobOutput (Maybe Text)
cjoSegmentDuration = lens _cjoSegmentDuration (\s a -> s {_cjoSegmentDuration = a})

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to your thumbnail.
cjoThumbnailEncryption :: Lens' CreateJobOutput (Maybe Encryption)
cjoThumbnailEncryption = lens _cjoThumbnailEncryption (\s a -> s {_cjoThumbnailEncryption = a})

-- | The number of degrees clockwise by which you want Elastic Transcoder to rotate the output relative to the input. Enter one of the following values: @auto@ , @0@ , @90@ , @180@ , @270@ . The value @auto@ generally works only if the file that you're transcoding contains rotation metadata.
cjoRotate :: Lens' CreateJobOutput (Maybe Text)
cjoRotate = lens _cjoRotate (\s a -> s {_cjoRotate = a})

instance Hashable CreateJobOutput

instance NFData CreateJobOutput

instance ToJSON CreateJobOutput where
  toJSON CreateJobOutput' {..} =
    object
      ( catMaybes
          [ ("ThumbnailPattern" .=) <$> _cjoThumbnailPattern,
            ("Captions" .=) <$> _cjoCaptions,
            ("PresetId" .=) <$> _cjoPresetId,
            ("Composition" .=) <$> _cjoComposition,
            ("AlbumArt" .=) <$> _cjoAlbumArt,
            ("Watermarks" .=) <$> _cjoWatermarks,
            ("Encryption" .=) <$> _cjoEncryption,
            ("Key" .=) <$> _cjoKey,
            ("SegmentDuration" .=) <$> _cjoSegmentDuration,
            ("ThumbnailEncryption" .=) <$> _cjoThumbnailEncryption,
            ("Rotate" .=) <$> _cjoRotate
          ]
      )
