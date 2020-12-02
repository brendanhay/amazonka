{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.EncoderSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.EncoderSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.AudioDescription
import Network.AWS.MediaLive.Types.AvailBlanking
import Network.AWS.MediaLive.Types.AvailConfiguration
import Network.AWS.MediaLive.Types.BlackoutSlate
import Network.AWS.MediaLive.Types.CaptionDescription
import Network.AWS.MediaLive.Types.FeatureActivations
import Network.AWS.MediaLive.Types.GlobalConfiguration
import Network.AWS.MediaLive.Types.NielsenConfiguration
import Network.AWS.MediaLive.Types.OutputGroup
import Network.AWS.MediaLive.Types.TimecodeConfig
import Network.AWS.MediaLive.Types.VideoDescription
import Network.AWS.Prelude

-- | Encoder Settings
--
-- /See:/ 'encoderSettings' smart constructor.
data EncoderSettings = EncoderSettings'
  { _esCaptionDescriptions ::
      !(Maybe [CaptionDescription]),
    _esAvailConfiguration :: !(Maybe AvailConfiguration),
    _esFeatureActivations :: !(Maybe FeatureActivations),
    _esNielsenConfiguration :: !(Maybe NielsenConfiguration),
    _esAvailBlanking :: !(Maybe AvailBlanking),
    _esGlobalConfiguration :: !(Maybe GlobalConfiguration),
    _esBlackoutSlate :: !(Maybe BlackoutSlate),
    _esVideoDescriptions :: ![VideoDescription],
    _esAudioDescriptions :: ![AudioDescription],
    _esOutputGroups :: ![OutputGroup],
    _esTimecodeConfig :: !TimecodeConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EncoderSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esCaptionDescriptions' - Settings for caption decriptions
--
-- * 'esAvailConfiguration' - Event-wide configuration settings for ad avail insertion.
--
-- * 'esFeatureActivations' - Feature Activations
--
-- * 'esNielsenConfiguration' - Nielsen configuration settings.
--
-- * 'esAvailBlanking' - Settings for ad avail blanking.
--
-- * 'esGlobalConfiguration' - Configuration settings that apply to the event as a whole.
--
-- * 'esBlackoutSlate' - Settings for blackout slate.
--
-- * 'esVideoDescriptions' - Undocumented member.
--
-- * 'esAudioDescriptions' - Undocumented member.
--
-- * 'esOutputGroups' - Undocumented member.
--
-- * 'esTimecodeConfig' - Contains settings used to acquire and adjust timecode information from inputs.
encoderSettings ::
  -- | 'esTimecodeConfig'
  TimecodeConfig ->
  EncoderSettings
encoderSettings pTimecodeConfig_ =
  EncoderSettings'
    { _esCaptionDescriptions = Nothing,
      _esAvailConfiguration = Nothing,
      _esFeatureActivations = Nothing,
      _esNielsenConfiguration = Nothing,
      _esAvailBlanking = Nothing,
      _esGlobalConfiguration = Nothing,
      _esBlackoutSlate = Nothing,
      _esVideoDescriptions = mempty,
      _esAudioDescriptions = mempty,
      _esOutputGroups = mempty,
      _esTimecodeConfig = pTimecodeConfig_
    }

-- | Settings for caption decriptions
esCaptionDescriptions :: Lens' EncoderSettings [CaptionDescription]
esCaptionDescriptions = lens _esCaptionDescriptions (\s a -> s {_esCaptionDescriptions = a}) . _Default . _Coerce

-- | Event-wide configuration settings for ad avail insertion.
esAvailConfiguration :: Lens' EncoderSettings (Maybe AvailConfiguration)
esAvailConfiguration = lens _esAvailConfiguration (\s a -> s {_esAvailConfiguration = a})

-- | Feature Activations
esFeatureActivations :: Lens' EncoderSettings (Maybe FeatureActivations)
esFeatureActivations = lens _esFeatureActivations (\s a -> s {_esFeatureActivations = a})

-- | Nielsen configuration settings.
esNielsenConfiguration :: Lens' EncoderSettings (Maybe NielsenConfiguration)
esNielsenConfiguration = lens _esNielsenConfiguration (\s a -> s {_esNielsenConfiguration = a})

-- | Settings for ad avail blanking.
esAvailBlanking :: Lens' EncoderSettings (Maybe AvailBlanking)
esAvailBlanking = lens _esAvailBlanking (\s a -> s {_esAvailBlanking = a})

-- | Configuration settings that apply to the event as a whole.
esGlobalConfiguration :: Lens' EncoderSettings (Maybe GlobalConfiguration)
esGlobalConfiguration = lens _esGlobalConfiguration (\s a -> s {_esGlobalConfiguration = a})

-- | Settings for blackout slate.
esBlackoutSlate :: Lens' EncoderSettings (Maybe BlackoutSlate)
esBlackoutSlate = lens _esBlackoutSlate (\s a -> s {_esBlackoutSlate = a})

-- | Undocumented member.
esVideoDescriptions :: Lens' EncoderSettings [VideoDescription]
esVideoDescriptions = lens _esVideoDescriptions (\s a -> s {_esVideoDescriptions = a}) . _Coerce

-- | Undocumented member.
esAudioDescriptions :: Lens' EncoderSettings [AudioDescription]
esAudioDescriptions = lens _esAudioDescriptions (\s a -> s {_esAudioDescriptions = a}) . _Coerce

-- | Undocumented member.
esOutputGroups :: Lens' EncoderSettings [OutputGroup]
esOutputGroups = lens _esOutputGroups (\s a -> s {_esOutputGroups = a}) . _Coerce

-- | Contains settings used to acquire and adjust timecode information from inputs.
esTimecodeConfig :: Lens' EncoderSettings TimecodeConfig
esTimecodeConfig = lens _esTimecodeConfig (\s a -> s {_esTimecodeConfig = a})

instance FromJSON EncoderSettings where
  parseJSON =
    withObject
      "EncoderSettings"
      ( \x ->
          EncoderSettings'
            <$> (x .:? "captionDescriptions" .!= mempty)
            <*> (x .:? "availConfiguration")
            <*> (x .:? "featureActivations")
            <*> (x .:? "nielsenConfiguration")
            <*> (x .:? "availBlanking")
            <*> (x .:? "globalConfiguration")
            <*> (x .:? "blackoutSlate")
            <*> (x .:? "videoDescriptions" .!= mempty)
            <*> (x .:? "audioDescriptions" .!= mempty)
            <*> (x .:? "outputGroups" .!= mempty)
            <*> (x .: "timecodeConfig")
      )

instance Hashable EncoderSettings

instance NFData EncoderSettings

instance ToJSON EncoderSettings where
  toJSON EncoderSettings' {..} =
    object
      ( catMaybes
          [ ("captionDescriptions" .=) <$> _esCaptionDescriptions,
            ("availConfiguration" .=) <$> _esAvailConfiguration,
            ("featureActivations" .=) <$> _esFeatureActivations,
            ("nielsenConfiguration" .=) <$> _esNielsenConfiguration,
            ("availBlanking" .=) <$> _esAvailBlanking,
            ("globalConfiguration" .=) <$> _esGlobalConfiguration,
            ("blackoutSlate" .=) <$> _esBlackoutSlate,
            Just ("videoDescriptions" .= _esVideoDescriptions),
            Just ("audioDescriptions" .= _esAudioDescriptions),
            Just ("outputGroups" .= _esOutputGroups),
            Just ("timecodeConfig" .= _esTimecodeConfig)
          ]
      )
