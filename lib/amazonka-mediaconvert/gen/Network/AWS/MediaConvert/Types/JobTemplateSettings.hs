{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.JobTemplateSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.JobTemplateSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.AvailBlanking
import Network.AWS.MediaConvert.Types.EsamSettings
import Network.AWS.MediaConvert.Types.InputTemplate
import Network.AWS.MediaConvert.Types.MotionImageInserter
import Network.AWS.MediaConvert.Types.NielsenConfiguration
import Network.AWS.MediaConvert.Types.NielsenNonLinearWatermarkSettings
import Network.AWS.MediaConvert.Types.OutputGroup
import Network.AWS.MediaConvert.Types.TimecodeConfig
import Network.AWS.MediaConvert.Types.TimedMetadataInsertion
import Network.AWS.Prelude

-- | JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
--
-- /See:/ 'jobTemplateSettings' smart constructor.
data JobTemplateSettings = JobTemplateSettings'
  { _jtsNielsenNonLinearWatermark ::
      !(Maybe NielsenNonLinearWatermarkSettings),
    _jtsEsam :: !(Maybe EsamSettings),
    _jtsInputs :: !(Maybe [InputTemplate]),
    _jtsTimedMetadataInsertion ::
      !(Maybe TimedMetadataInsertion),
    _jtsNielsenConfiguration ::
      !(Maybe NielsenConfiguration),
    _jtsAvailBlanking :: !(Maybe AvailBlanking),
    _jtsMotionImageInserter ::
      !(Maybe MotionImageInserter),
    _jtsTimecodeConfig :: !(Maybe TimecodeConfig),
    _jtsOutputGroups :: !(Maybe [OutputGroup]),
    _jtsAdAvailOffset :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobTemplateSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jtsNielsenNonLinearWatermark' - Ignore these settings unless you are using Nielsen non-linear watermarking. Specify the values that  MediaConvert uses to generate and place Nielsen watermarks in your output audio. In addition to  specifying these values, you also need to set up your cloud TIC server. These settings apply to  every output in your job. The MediaConvert implementation is currently with the following Nielsen versions: Nielsen Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7 Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
--
-- * 'jtsEsam' - Settings for Event Signaling And Messaging (ESAM).
--
-- * 'jtsInputs' - Use Inputs (inputs) to define the source file used in the transcode job. There can only be one input in a job template.  Using the API, you can include multiple inputs when referencing a job template.
--
-- * 'jtsTimedMetadataInsertion' - Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3 tags in any HLS outputs. To include timed metadata, you must enable it here, enable it in each output container, and specify tags and timecodes in ID3 insertion (Id3Insertion) objects.
--
-- * 'jtsNielsenConfiguration' - Settings for your Nielsen configuration. If you don't do Nielsen measurement and analytics, ignore these settings. When you enable Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM to ID3 tagging for all outputs in the job. To enable Nielsen configuration programmatically, include an instance of nielsenConfiguration in your JSON job specification. Even if you don't include any children of nielsenConfiguration, you still enable the setting.
--
-- * 'jtsAvailBlanking' - Settings for ad avail blanking.  Video can be blanked or overlaid with an image, and audio muted during SCTE-35 triggered ad avails.
--
-- * 'jtsMotionImageInserter' - Overlay motion graphics on top of your video. The motion graphics that you specify here appear on all outputs in all output groups.
--
-- * 'jtsTimecodeConfig' - Contains settings used to acquire and adjust timecode information from inputs.
--
-- * 'jtsOutputGroups' - (OutputGroups) contains one group of settings for each set of outputs that share a common package type. All unpackaged files (MPEG-4, MPEG-2 TS, Quicktime, MXF, and no container) are grouped in a single output group as well. Required in (OutputGroups) is a group of settings that apply to the whole group. This required object depends on the value you set for (Type) under (OutputGroups)>(OutputGroupSettings). Type, settings object pairs are as follows. * FILE_GROUP_SETTINGS, FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings * DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings * MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings * CMAF_GROUP_SETTINGS, CmafGroupSettings
--
-- * 'jtsAdAvailOffset' - When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time.
jobTemplateSettings ::
  JobTemplateSettings
jobTemplateSettings =
  JobTemplateSettings'
    { _jtsNielsenNonLinearWatermark = Nothing,
      _jtsEsam = Nothing,
      _jtsInputs = Nothing,
      _jtsTimedMetadataInsertion = Nothing,
      _jtsNielsenConfiguration = Nothing,
      _jtsAvailBlanking = Nothing,
      _jtsMotionImageInserter = Nothing,
      _jtsTimecodeConfig = Nothing,
      _jtsOutputGroups = Nothing,
      _jtsAdAvailOffset = Nothing
    }

-- | Ignore these settings unless you are using Nielsen non-linear watermarking. Specify the values that  MediaConvert uses to generate and place Nielsen watermarks in your output audio. In addition to  specifying these values, you also need to set up your cloud TIC server. These settings apply to  every output in your job. The MediaConvert implementation is currently with the following Nielsen versions: Nielsen Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7 Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
jtsNielsenNonLinearWatermark :: Lens' JobTemplateSettings (Maybe NielsenNonLinearWatermarkSettings)
jtsNielsenNonLinearWatermark = lens _jtsNielsenNonLinearWatermark (\s a -> s {_jtsNielsenNonLinearWatermark = a})

-- | Settings for Event Signaling And Messaging (ESAM).
jtsEsam :: Lens' JobTemplateSettings (Maybe EsamSettings)
jtsEsam = lens _jtsEsam (\s a -> s {_jtsEsam = a})

-- | Use Inputs (inputs) to define the source file used in the transcode job. There can only be one input in a job template.  Using the API, you can include multiple inputs when referencing a job template.
jtsInputs :: Lens' JobTemplateSettings [InputTemplate]
jtsInputs = lens _jtsInputs (\s a -> s {_jtsInputs = a}) . _Default . _Coerce

-- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3 tags in any HLS outputs. To include timed metadata, you must enable it here, enable it in each output container, and specify tags and timecodes in ID3 insertion (Id3Insertion) objects.
jtsTimedMetadataInsertion :: Lens' JobTemplateSettings (Maybe TimedMetadataInsertion)
jtsTimedMetadataInsertion = lens _jtsTimedMetadataInsertion (\s a -> s {_jtsTimedMetadataInsertion = a})

-- | Settings for your Nielsen configuration. If you don't do Nielsen measurement and analytics, ignore these settings. When you enable Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM to ID3 tagging for all outputs in the job. To enable Nielsen configuration programmatically, include an instance of nielsenConfiguration in your JSON job specification. Even if you don't include any children of nielsenConfiguration, you still enable the setting.
jtsNielsenConfiguration :: Lens' JobTemplateSettings (Maybe NielsenConfiguration)
jtsNielsenConfiguration = lens _jtsNielsenConfiguration (\s a -> s {_jtsNielsenConfiguration = a})

-- | Settings for ad avail blanking.  Video can be blanked or overlaid with an image, and audio muted during SCTE-35 triggered ad avails.
jtsAvailBlanking :: Lens' JobTemplateSettings (Maybe AvailBlanking)
jtsAvailBlanking = lens _jtsAvailBlanking (\s a -> s {_jtsAvailBlanking = a})

-- | Overlay motion graphics on top of your video. The motion graphics that you specify here appear on all outputs in all output groups.
jtsMotionImageInserter :: Lens' JobTemplateSettings (Maybe MotionImageInserter)
jtsMotionImageInserter = lens _jtsMotionImageInserter (\s a -> s {_jtsMotionImageInserter = a})

-- | Contains settings used to acquire and adjust timecode information from inputs.
jtsTimecodeConfig :: Lens' JobTemplateSettings (Maybe TimecodeConfig)
jtsTimecodeConfig = lens _jtsTimecodeConfig (\s a -> s {_jtsTimecodeConfig = a})

-- | (OutputGroups) contains one group of settings for each set of outputs that share a common package type. All unpackaged files (MPEG-4, MPEG-2 TS, Quicktime, MXF, and no container) are grouped in a single output group as well. Required in (OutputGroups) is a group of settings that apply to the whole group. This required object depends on the value you set for (Type) under (OutputGroups)>(OutputGroupSettings). Type, settings object pairs are as follows. * FILE_GROUP_SETTINGS, FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings * DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings * MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings * CMAF_GROUP_SETTINGS, CmafGroupSettings
jtsOutputGroups :: Lens' JobTemplateSettings [OutputGroup]
jtsOutputGroups = lens _jtsOutputGroups (\s a -> s {_jtsOutputGroups = a}) . _Default . _Coerce

-- | When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time.
jtsAdAvailOffset :: Lens' JobTemplateSettings (Maybe Int)
jtsAdAvailOffset = lens _jtsAdAvailOffset (\s a -> s {_jtsAdAvailOffset = a})

instance FromJSON JobTemplateSettings where
  parseJSON =
    withObject
      "JobTemplateSettings"
      ( \x ->
          JobTemplateSettings'
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

instance Hashable JobTemplateSettings

instance NFData JobTemplateSettings

instance ToJSON JobTemplateSettings where
  toJSON JobTemplateSettings' {..} =
    object
      ( catMaybes
          [ ("nielsenNonLinearWatermark" .=)
              <$> _jtsNielsenNonLinearWatermark,
            ("esam" .=) <$> _jtsEsam,
            ("inputs" .=) <$> _jtsInputs,
            ("timedMetadataInsertion" .=) <$> _jtsTimedMetadataInsertion,
            ("nielsenConfiguration" .=) <$> _jtsNielsenConfiguration,
            ("availBlanking" .=) <$> _jtsAvailBlanking,
            ("motionImageInserter" .=) <$> _jtsMotionImageInserter,
            ("timecodeConfig" .=) <$> _jtsTimecodeConfig,
            ("outputGroups" .=) <$> _jtsOutputGroups,
            ("adAvailOffset" .=) <$> _jtsAdAvailOffset
          ]
      )
