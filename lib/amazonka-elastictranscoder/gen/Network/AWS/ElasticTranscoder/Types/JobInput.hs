{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.JobInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.JobInput where

import Network.AWS.ElasticTranscoder.Types.DetectedProperties
import Network.AWS.ElasticTranscoder.Types.Encryption
import Network.AWS.ElasticTranscoder.Types.InputCaptions
import Network.AWS.ElasticTranscoder.Types.TimeSpan
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the file that you're transcoding.
--
--
--
-- /See:/ 'jobInput' smart constructor.
data JobInput = JobInput'
  { _jiFrameRate :: !(Maybe Text),
    _jiResolution :: !(Maybe Text),
    _jiAspectRatio :: !(Maybe Text),
    _jiTimeSpan :: !(Maybe TimeSpan),
    _jiEncryption :: !(Maybe Encryption),
    _jiKey :: !(Maybe Text),
    _jiDetectedProperties :: !(Maybe DetectedProperties),
    _jiContainer :: !(Maybe Text),
    _jiInterlaced :: !(Maybe Text),
    _jiInputCaptions :: !(Maybe InputCaptions)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jiFrameRate' - The frame rate of the input file. If you want Elastic Transcoder to automatically detect the frame rate of the input file, specify @auto@ . If you want to specify the frame rate for the input file, enter one of the following values:  @10@ , @15@ , @23.97@ , @24@ , @25@ , @29.97@ , @30@ , @60@  If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of the frame rate.
--
-- * 'jiResolution' - This value must be @auto@ , which causes Elastic Transcoder to automatically detect the resolution of the input file.
--
-- * 'jiAspectRatio' - The aspect ratio of the input file. If you want Elastic Transcoder to automatically detect the aspect ratio of the input file, specify @auto@ . If you want to specify the aspect ratio for the output file, enter one of the following values:  @1:1@ , @4:3@ , @3:2@ , @16:9@  If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of the aspect ratio.
--
-- * 'jiTimeSpan' - Settings for clipping an input. Each input can have different clip settings.
--
-- * 'jiEncryption' - The encryption settings, if any, that are used for decrypting your input files. If your input file is encrypted, you must specify the mode that Elastic Transcoder uses to decrypt your file.
--
-- * 'jiKey' - The name of the file to transcode. Elsewhere in the body of the JSON block is the the ID of the pipeline to use for processing the job. The @InputBucket@ object in that pipeline tells Elastic Transcoder which Amazon S3 bucket to get the file from.  If the file name includes a prefix, such as @cooking/lasagna.mpg@ , include the prefix in the key. If the file isn't in the specified bucket, Elastic Transcoder returns an error.
--
-- * 'jiDetectedProperties' - The detected properties of the input file.
--
-- * 'jiContainer' - The container type for the input file. If you want Elastic Transcoder to automatically detect the container type of the input file, specify @auto@ . If you want to specify the container type for the input file, enter one of the following values:  @3gp@ , @aac@ , @asf@ , @avi@ , @divx@ , @flv@ , @m4a@ , @mkv@ , @mov@ , @mp3@ , @mp4@ , @mpeg@ , @mpeg-ps@ , @mpeg-ts@ , @mxf@ , @ogg@ , @vob@ , @wav@ , @webm@
--
-- * 'jiInterlaced' - Whether the input file is interlaced. If you want Elastic Transcoder to automatically detect whether the input file is interlaced, specify @auto@ . If you want to specify whether the input file is interlaced, enter one of the following values: @true@ , @false@  If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of interlacing.
--
-- * 'jiInputCaptions' - You can configure Elastic Transcoder to transcode captions, or subtitles, from one format to another. All captions must be in UTF-8. Elastic Transcoder supports two types of captions:     * __Embedded:__ Embedded captions are included in the same file as the audio and video. Elastic Transcoder supports only one embedded caption per language, to a maximum of 300 embedded captions per file. Valid input values include: @CEA-608 (EIA-608@ , first non-empty channel only), @CEA-708 (EIA-708@ , first non-empty channel only), and @mov-text@  Valid outputs include: @mov-text@  Elastic Transcoder supports a maximum of one embedded format per output.     * __Sidecar:__ Sidecar captions are kept in a separate metadata file from the audio and video data. Sidecar captions require a player that is capable of understanding the relationship between the video file and the sidecar file. Elastic Transcoder supports only one sidecar caption per language, to a maximum of 20 sidecar captions per file. Valid input values include: @dfxp@ (first div element only), @ebu-tt@ , @scc@ , @smpt@ , @srt@ , @ttml@ (first div element only), and @webvtt@  Valid outputs include: @dfxp@ (first div element only), @scc@ , @srt@ , and @webvtt@ . If you want ttml or smpte-tt compatible captions, specify dfxp as your output format. Elastic Transcoder does not support OCR (Optical Character Recognition), does not accept pictures as a valid input for captions, and is not available for audio-only transcoding. Elastic Transcoder does not preserve text formatting (for example, italics) during the transcoding process. To remove captions or leave the captions empty, set @Captions@ to null. To pass through existing captions unchanged, set the @MergePolicy@ to @MergeRetain@ , and pass in a null @CaptionSources@ array. For more information on embedded files, see the Subtitles Wikipedia page. For more information on sidecar files, see the Extensible Metadata Platform and Sidecar file Wikipedia pages.
jobInput ::
  JobInput
jobInput =
  JobInput'
    { _jiFrameRate = Nothing,
      _jiResolution = Nothing,
      _jiAspectRatio = Nothing,
      _jiTimeSpan = Nothing,
      _jiEncryption = Nothing,
      _jiKey = Nothing,
      _jiDetectedProperties = Nothing,
      _jiContainer = Nothing,
      _jiInterlaced = Nothing,
      _jiInputCaptions = Nothing
    }

-- | The frame rate of the input file. If you want Elastic Transcoder to automatically detect the frame rate of the input file, specify @auto@ . If you want to specify the frame rate for the input file, enter one of the following values:  @10@ , @15@ , @23.97@ , @24@ , @25@ , @29.97@ , @30@ , @60@  If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of the frame rate.
jiFrameRate :: Lens' JobInput (Maybe Text)
jiFrameRate = lens _jiFrameRate (\s a -> s {_jiFrameRate = a})

-- | This value must be @auto@ , which causes Elastic Transcoder to automatically detect the resolution of the input file.
jiResolution :: Lens' JobInput (Maybe Text)
jiResolution = lens _jiResolution (\s a -> s {_jiResolution = a})

-- | The aspect ratio of the input file. If you want Elastic Transcoder to automatically detect the aspect ratio of the input file, specify @auto@ . If you want to specify the aspect ratio for the output file, enter one of the following values:  @1:1@ , @4:3@ , @3:2@ , @16:9@  If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of the aspect ratio.
jiAspectRatio :: Lens' JobInput (Maybe Text)
jiAspectRatio = lens _jiAspectRatio (\s a -> s {_jiAspectRatio = a})

-- | Settings for clipping an input. Each input can have different clip settings.
jiTimeSpan :: Lens' JobInput (Maybe TimeSpan)
jiTimeSpan = lens _jiTimeSpan (\s a -> s {_jiTimeSpan = a})

-- | The encryption settings, if any, that are used for decrypting your input files. If your input file is encrypted, you must specify the mode that Elastic Transcoder uses to decrypt your file.
jiEncryption :: Lens' JobInput (Maybe Encryption)
jiEncryption = lens _jiEncryption (\s a -> s {_jiEncryption = a})

-- | The name of the file to transcode. Elsewhere in the body of the JSON block is the the ID of the pipeline to use for processing the job. The @InputBucket@ object in that pipeline tells Elastic Transcoder which Amazon S3 bucket to get the file from.  If the file name includes a prefix, such as @cooking/lasagna.mpg@ , include the prefix in the key. If the file isn't in the specified bucket, Elastic Transcoder returns an error.
jiKey :: Lens' JobInput (Maybe Text)
jiKey = lens _jiKey (\s a -> s {_jiKey = a})

-- | The detected properties of the input file.
jiDetectedProperties :: Lens' JobInput (Maybe DetectedProperties)
jiDetectedProperties = lens _jiDetectedProperties (\s a -> s {_jiDetectedProperties = a})

-- | The container type for the input file. If you want Elastic Transcoder to automatically detect the container type of the input file, specify @auto@ . If you want to specify the container type for the input file, enter one of the following values:  @3gp@ , @aac@ , @asf@ , @avi@ , @divx@ , @flv@ , @m4a@ , @mkv@ , @mov@ , @mp3@ , @mp4@ , @mpeg@ , @mpeg-ps@ , @mpeg-ts@ , @mxf@ , @ogg@ , @vob@ , @wav@ , @webm@
jiContainer :: Lens' JobInput (Maybe Text)
jiContainer = lens _jiContainer (\s a -> s {_jiContainer = a})

-- | Whether the input file is interlaced. If you want Elastic Transcoder to automatically detect whether the input file is interlaced, specify @auto@ . If you want to specify whether the input file is interlaced, enter one of the following values: @true@ , @false@  If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of interlacing.
jiInterlaced :: Lens' JobInput (Maybe Text)
jiInterlaced = lens _jiInterlaced (\s a -> s {_jiInterlaced = a})

-- | You can configure Elastic Transcoder to transcode captions, or subtitles, from one format to another. All captions must be in UTF-8. Elastic Transcoder supports two types of captions:     * __Embedded:__ Embedded captions are included in the same file as the audio and video. Elastic Transcoder supports only one embedded caption per language, to a maximum of 300 embedded captions per file. Valid input values include: @CEA-608 (EIA-608@ , first non-empty channel only), @CEA-708 (EIA-708@ , first non-empty channel only), and @mov-text@  Valid outputs include: @mov-text@  Elastic Transcoder supports a maximum of one embedded format per output.     * __Sidecar:__ Sidecar captions are kept in a separate metadata file from the audio and video data. Sidecar captions require a player that is capable of understanding the relationship between the video file and the sidecar file. Elastic Transcoder supports only one sidecar caption per language, to a maximum of 20 sidecar captions per file. Valid input values include: @dfxp@ (first div element only), @ebu-tt@ , @scc@ , @smpt@ , @srt@ , @ttml@ (first div element only), and @webvtt@  Valid outputs include: @dfxp@ (first div element only), @scc@ , @srt@ , and @webvtt@ . If you want ttml or smpte-tt compatible captions, specify dfxp as your output format. Elastic Transcoder does not support OCR (Optical Character Recognition), does not accept pictures as a valid input for captions, and is not available for audio-only transcoding. Elastic Transcoder does not preserve text formatting (for example, italics) during the transcoding process. To remove captions or leave the captions empty, set @Captions@ to null. To pass through existing captions unchanged, set the @MergePolicy@ to @MergeRetain@ , and pass in a null @CaptionSources@ array. For more information on embedded files, see the Subtitles Wikipedia page. For more information on sidecar files, see the Extensible Metadata Platform and Sidecar file Wikipedia pages.
jiInputCaptions :: Lens' JobInput (Maybe InputCaptions)
jiInputCaptions = lens _jiInputCaptions (\s a -> s {_jiInputCaptions = a})

instance FromJSON JobInput where
  parseJSON =
    withObject
      "JobInput"
      ( \x ->
          JobInput'
            <$> (x .:? "FrameRate")
            <*> (x .:? "Resolution")
            <*> (x .:? "AspectRatio")
            <*> (x .:? "TimeSpan")
            <*> (x .:? "Encryption")
            <*> (x .:? "Key")
            <*> (x .:? "DetectedProperties")
            <*> (x .:? "Container")
            <*> (x .:? "Interlaced")
            <*> (x .:? "InputCaptions")
      )

instance Hashable JobInput

instance NFData JobInput

instance ToJSON JobInput where
  toJSON JobInput' {..} =
    object
      ( catMaybes
          [ ("FrameRate" .=) <$> _jiFrameRate,
            ("Resolution" .=) <$> _jiResolution,
            ("AspectRatio" .=) <$> _jiAspectRatio,
            ("TimeSpan" .=) <$> _jiTimeSpan,
            ("Encryption" .=) <$> _jiEncryption,
            ("Key" .=) <$> _jiKey,
            ("DetectedProperties" .=) <$> _jiDetectedProperties,
            ("Container" .=) <$> _jiContainer,
            ("Interlaced" .=) <$> _jiInterlaced,
            ("InputCaptions" .=) <$> _jiInputCaptions
          ]
      )
