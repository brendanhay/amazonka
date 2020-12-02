{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.JobSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.JobSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.AvailBlanking
import Network.AWS.MediaConvert.Types.EsamSettings
import Network.AWS.MediaConvert.Types.Input
import Network.AWS.MediaConvert.Types.MotionImageInserter
import Network.AWS.MediaConvert.Types.NielsenConfiguration
import Network.AWS.MediaConvert.Types.NielsenNonLinearWatermarkSettings
import Network.AWS.MediaConvert.Types.OutputGroup
import Network.AWS.MediaConvert.Types.TimecodeConfig
import Network.AWS.MediaConvert.Types.TimedMetadataInsertion
import Network.AWS.Prelude

-- | JobSettings contains all the transcode settings for a job.
--
-- /See:/ 'jobSettings' smart constructor.
data JobSettings = JobSettings'
  { _jsNielsenNonLinearWatermark ::
      !(Maybe NielsenNonLinearWatermarkSettings),
    _jsEsam :: !(Maybe EsamSettings),
    _jsInputs :: !(Maybe [Input]),
    _jsTimedMetadataInsertion :: !(Maybe TimedMetadataInsertion),
    _jsNielsenConfiguration :: !(Maybe NielsenConfiguration),
    _jsAvailBlanking :: !(Maybe AvailBlanking),
    _jsMotionImageInserter :: !(Maybe MotionImageInserter),
    _jsTimecodeConfig :: !(Maybe TimecodeConfig),
    _jsOutputGroups :: !(Maybe [OutputGroup]),
    _jsAdAvailOffset :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jsNielsenNonLinearWatermark' - Ignore these settings unless you are using Nielsen non-linear watermarking. Specify the values that  MediaConvert uses to generate and place Nielsen watermarks in your output audio. In addition to  specifying these values, you also need to set up your cloud TIC server. These settings apply to  every output in your job. The MediaConvert implementation is currently with the following Nielsen versions: Nielsen Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7 Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
--
-- * 'jsEsam' - Settings for Event Signaling And Messaging (ESAM).
--
-- * 'jsInputs' - Use Inputs (inputs) to define source file used in the transcode job. There can be multiple inputs add in a job. These inputs will be concantenated together to create the output.
--
-- * 'jsTimedMetadataInsertion' - Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3 tags in any HLS outputs. To include timed metadata, you must enable it here, enable it in each output container, and specify tags and timecodes in ID3 insertion (Id3Insertion) objects.
--
-- * 'jsNielsenConfiguration' - Settings for your Nielsen configuration. If you don't do Nielsen measurement and analytics, ignore these settings. When you enable Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM to ID3 tagging for all outputs in the job. To enable Nielsen configuration programmatically, include an instance of nielsenConfiguration in your JSON job specification. Even if you don't include any children of nielsenConfiguration, you still enable the setting.
--
-- * 'jsAvailBlanking' - Settings for ad avail blanking.  Video can be blanked or overlaid with an image, and audio muted during SCTE-35 triggered ad avails.
--
-- * 'jsMotionImageInserter' - Overlay motion graphics on top of your video. The motion graphics that you specify here appear on all outputs in all output groups.
--
-- * 'jsTimecodeConfig' - Contains settings used to acquire and adjust timecode information from inputs.
--
-- * 'jsOutputGroups' - (OutputGroups) contains one group of settings for each set of outputs that share a common package type. All unpackaged files (MPEG-4, MPEG-2 TS, Quicktime, MXF, and no container) are grouped in a single output group as well. Required in (OutputGroups) is a group of settings that apply to the whole group. This required object depends on the value you set for (Type) under (OutputGroups)>(OutputGroupSettings). Type, settings object pairs are as follows. * FILE_GROUP_SETTINGS, FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings * DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings * MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings * CMAF_GROUP_SETTINGS, CmafGroupSettings
--
-- * 'jsAdAvailOffset' - When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time.
jobSettings ::
  JobSettings
jobSettings =
  JobSettings'
    { _jsNielsenNonLinearWatermark = Nothing,
      _jsEsam = Nothing,
      _jsInputs = Nothing,
      _jsTimedMetadataInsertion = Nothing,
      _jsNielsenConfiguration = Nothing,
      _jsAvailBlanking = Nothing,
      _jsMotionImageInserter = Nothing,
      _jsTimecodeConfig = Nothing,
      _jsOutputGroups = Nothing,
      _jsAdAvailOffset = Nothing
    }

-- | Ignore these settings unless you are using Nielsen non-linear watermarking. Specify the values that  MediaConvert uses to generate and place Nielsen watermarks in your output audio. In addition to  specifying these values, you also need to set up your cloud TIC server. These settings apply to  every output in your job. The MediaConvert implementation is currently with the following Nielsen versions: Nielsen Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7 Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
jsNielsenNonLinearWatermark :: Lens' JobSettings (Maybe NielsenNonLinearWatermarkSettings)
jsNielsenNonLinearWatermark = lens _jsNielsenNonLinearWatermark (\s a -> s {_jsNielsenNonLinearWatermark = a})

-- | Settings for Event Signaling And Messaging (ESAM).
jsEsam :: Lens' JobSettings (Maybe EsamSettings)
jsEsam = lens _jsEsam (\s a -> s {_jsEsam = a})

-- | Use Inputs (inputs) to define source file used in the transcode job. There can be multiple inputs add in a job. These inputs will be concantenated together to create the output.
jsInputs :: Lens' JobSettings [Input]
jsInputs = lens _jsInputs (\s a -> s {_jsInputs = a}) . _Default . _Coerce

-- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3 tags in any HLS outputs. To include timed metadata, you must enable it here, enable it in each output container, and specify tags and timecodes in ID3 insertion (Id3Insertion) objects.
jsTimedMetadataInsertion :: Lens' JobSettings (Maybe TimedMetadataInsertion)
jsTimedMetadataInsertion = lens _jsTimedMetadataInsertion (\s a -> s {_jsTimedMetadataInsertion = a})

-- | Settings for your Nielsen configuration. If you don't do Nielsen measurement and analytics, ignore these settings. When you enable Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM to ID3 tagging for all outputs in the job. To enable Nielsen configuration programmatically, include an instance of nielsenConfiguration in your JSON job specification. Even if you don't include any children of nielsenConfiguration, you still enable the setting.
jsNielsenConfiguration :: Lens' JobSettings (Maybe NielsenConfiguration)
jsNielsenConfiguration = lens _jsNielsenConfiguration (\s a -> s {_jsNielsenConfiguration = a})

-- | Settings for ad avail blanking.  Video can be blanked or overlaid with an image, and audio muted during SCTE-35 triggered ad avails.
jsAvailBlanking :: Lens' JobSettings (Maybe AvailBlanking)
jsAvailBlanking = lens _jsAvailBlanking (\s a -> s {_jsAvailBlanking = a})

-- | Overlay motion graphics on top of your video. The motion graphics that you specify here appear on all outputs in all output groups.
jsMotionImageInserter :: Lens' JobSettings (Maybe MotionImageInserter)
jsMotionImageInserter = lens _jsMotionImageInserter (\s a -> s {_jsMotionImageInserter = a})

-- | Contains settings used to acquire and adjust timecode information from inputs.
jsTimecodeConfig :: Lens' JobSettings (Maybe TimecodeConfig)
jsTimecodeConfig = lens _jsTimecodeConfig (\s a -> s {_jsTimecodeConfig = a})

-- | (OutputGroups) contains one group of settings for each set of outputs that share a common package type. All unpackaged files (MPEG-4, MPEG-2 TS, Quicktime, MXF, and no container) are grouped in a single output group as well. Required in (OutputGroups) is a group of settings that apply to the whole group. This required object depends on the value you set for (Type) under (OutputGroups)>(OutputGroupSettings). Type, settings object pairs are as follows. * FILE_GROUP_SETTINGS, FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings * DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings * MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings * CMAF_GROUP_SETTINGS, CmafGroupSettings
jsOutputGroups :: Lens' JobSettings [OutputGroup]
jsOutputGroups = lens _jsOutputGroups (\s a -> s {_jsOutputGroups = a}) . _Default . _Coerce

-- | When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time.
jsAdAvailOffset :: Lens' JobSettings (Maybe Int)
jsAdAvailOffset = lens _jsAdAvailOffset (\s a -> s {_jsAdAvailOffset = a})

instance FromJSON JobSettings where
  parseJSON =
    withObject
      "JobSettings"
      ( \x ->
          JobSettings'
            <$> (x .:? "nielsenNonLinearWatermark")
            <*> (x .:? "esam")
            <*> (x .:? "inputs" .!= mempty)
            <*> (x .:? "timedMetadataInsertion")
            <*> (x .:? "nielsenConfiguration")
            <*> (x .:? "availBlanking")
            <*> (x .:? "motionImageInserter")
            <*> (x .:? "timecodeConfig")
            <*> (x .:? "outputGroups" .!= mempty)
            <*> (x .:? "adAvailOffset")
      )

instance Hashable JobSettings

instance NFData JobSettings

instance ToJSON JobSettings where
  toJSON JobSettings' {..} =
    object
      ( catMaybes
          [ ("nielsenNonLinearWatermark" .=) <$> _jsNielsenNonLinearWatermark,
            ("esam" .=) <$> _jsEsam,
            ("inputs" .=) <$> _jsInputs,
            ("timedMetadataInsertion" .=) <$> _jsTimedMetadataInsertion,
            ("nielsenConfiguration" .=) <$> _jsNielsenConfiguration,
            ("availBlanking" .=) <$> _jsAvailBlanking,
            ("motionImageInserter" .=) <$> _jsMotionImageInserter,
            ("timecodeConfig" .=) <$> _jsTimecodeConfig,
            ("outputGroups" .=) <$> _jsOutputGroups,
            ("adAvailOffset" .=) <$> _jsAdAvailOffset
          ]
      )
