{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioDescription where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.AudioChannelTaggingSettings
import Network.AWS.MediaConvert.Types.AudioCodecSettings
import Network.AWS.MediaConvert.Types.AudioLanguageCodeControl
import Network.AWS.MediaConvert.Types.AudioNormalizationSettings
import Network.AWS.MediaConvert.Types.AudioTypeControl
import Network.AWS.MediaConvert.Types.LanguageCode
import Network.AWS.MediaConvert.Types.RemixSettings
import Network.AWS.Prelude

-- | Description of audio output
--
-- /See:/ 'audioDescription' smart constructor.
data AudioDescription = AudioDescription'
  { _adAudioSourceName ::
      !(Maybe Text),
    _adCustomLanguageCode :: !(Maybe Text),
    _adLanguageCode :: !(Maybe LanguageCode),
    _adAudioChannelTaggingSettings ::
      !(Maybe AudioChannelTaggingSettings),
    _adAudioType :: !(Maybe Nat),
    _adAudioNormalizationSettings ::
      !(Maybe AudioNormalizationSettings),
    _adLanguageCodeControl ::
      !(Maybe AudioLanguageCodeControl),
    _adCodecSettings :: !(Maybe AudioCodecSettings),
    _adStreamName :: !(Maybe Text),
    _adRemixSettings :: !(Maybe RemixSettings),
    _adAudioTypeControl :: !(Maybe AudioTypeControl)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adAudioSourceName' - Specifies which audio data to use from each input. In the simplest case, specify an "Audio Selector":#inputs-audio_selector by name based on its order within each input. For example if you specify "Audio Selector 3", then the third audio selector will be used from each input. If an input does not have an "Audio Selector 3", then the audio selector marked as "default" in that input will be used. If there is no audio selector marked as "default", silence will be inserted for the duration of that input. Alternatively, an "Audio Selector Group":#inputs-audio_selector_group name may be specified, with similar default/silence behavior. If no audio_source_name is specified, then "Audio Selector 1" will be chosen automatically.
--
-- * 'adCustomLanguageCode' - Specify the language for this audio output track. The service puts this language code into your output audio track when you set Language code control (AudioLanguageCodeControl) to Use configured (USE_CONFIGURED). The service also uses your specified custom language code when you set Language code control (AudioLanguageCodeControl) to Follow input (FOLLOW_INPUT), but your input file doesn't specify a language code. For all outputs, you can use an ISO 639-2 or ISO 639-3 code. For streaming outputs, you can also use any other code in the full RFC-5646 specification. Streaming outputs are those that are in one of the following output groups: CMAF, DASH ISO, Apple HLS, or Microsoft Smooth Streaming.
--
-- * 'adLanguageCode' - Indicates the language of the audio output track. The ISO 639 language specified in the 'Language Code' drop down will be used when 'Follow Input Language Code' is not selected or when 'Follow Input Language Code' is selected but there is no ISO 639 language code specified by the input.
--
-- * 'adAudioChannelTaggingSettings' - When you mimic a multi-channel audio layout with multiple mono-channel tracks, you can tag each channel layout manually. For example, you would tag the tracks that contain your left, right, and center audio with Left (L), Right (R), and Center (C), respectively. When you don't specify a value, MediaConvert labels your track as Center (C) by default. To use audio layout tagging, your output must be in a QuickTime (.mov) container; your audio codec must be AAC, WAV, or AIFF; and you must set up your audio track to have only one channel.
--
-- * 'adAudioType' - Applies only if Follow Input Audio Type is unchecked (false). A number between 0 and 255. The following are defined in ISO-IEC 13818-1: 0 = Undefined, 1 = Clean Effects, 2 = Hearing Impaired, 3 = Visually Impaired Commentary, 4-255 = Reserved.
--
-- * 'adAudioNormalizationSettings' - Advanced audio normalization settings. Ignore these settings unless you need to comply with a loudness standard.
--
-- * 'adLanguageCodeControl' - Specify which source for language code takes precedence for this audio track. When you choose Follow input (FOLLOW_INPUT), the service uses the language code from the input track if it's present. If there's no languge code on the input track, the service uses the code that you specify in the setting Language code (languageCode or customLanguageCode). When you choose Use configured (USE_CONFIGURED), the service uses the language code that you specify.
--
-- * 'adCodecSettings' - Audio codec settings (CodecSettings) under (AudioDescriptions) contains the group of settings related to audio encoding. The settings in this group vary depending on the value that you choose for Audio codec (Codec). For each codec enum that you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * AAC, AacSettings * MP2, Mp2Settings * MP3, Mp3Settings * WAV, WavSettings * AIFF, AiffSettings * AC3, Ac3Settings * EAC3, Eac3Settings * EAC3_ATMOS, Eac3AtmosSettings * VORBIS, VorbisSettings * OPUS, OpusSettings
--
-- * 'adStreamName' - Specify a label for this output audio stream. For example, "English", "Director commentary", or "track_2". For streaming outputs, MediaConvert passes this information into destination manifests for display on the end-viewer's player device. For outputs in other output groups, the service ignores this setting.
--
-- * 'adRemixSettings' - Advanced audio remixing settings.
--
-- * 'adAudioTypeControl' - When set to FOLLOW_INPUT, if the input contains an ISO 639 audio_type, then that value is passed through to the output. If the input contains no ISO 639 audio_type, the value in Audio Type is included in the output. Otherwise the value in Audio Type is included in the output. Note that this field and audioType are both ignored if audioDescriptionBroadcasterMix is set to BROADCASTER_MIXED_AD.
audioDescription ::
  AudioDescription
audioDescription =
  AudioDescription'
    { _adAudioSourceName = Nothing,
      _adCustomLanguageCode = Nothing,
      _adLanguageCode = Nothing,
      _adAudioChannelTaggingSettings = Nothing,
      _adAudioType = Nothing,
      _adAudioNormalizationSettings = Nothing,
      _adLanguageCodeControl = Nothing,
      _adCodecSettings = Nothing,
      _adStreamName = Nothing,
      _adRemixSettings = Nothing,
      _adAudioTypeControl = Nothing
    }

-- | Specifies which audio data to use from each input. In the simplest case, specify an "Audio Selector":#inputs-audio_selector by name based on its order within each input. For example if you specify "Audio Selector 3", then the third audio selector will be used from each input. If an input does not have an "Audio Selector 3", then the audio selector marked as "default" in that input will be used. If there is no audio selector marked as "default", silence will be inserted for the duration of that input. Alternatively, an "Audio Selector Group":#inputs-audio_selector_group name may be specified, with similar default/silence behavior. If no audio_source_name is specified, then "Audio Selector 1" will be chosen automatically.
adAudioSourceName :: Lens' AudioDescription (Maybe Text)
adAudioSourceName = lens _adAudioSourceName (\s a -> s {_adAudioSourceName = a})

-- | Specify the language for this audio output track. The service puts this language code into your output audio track when you set Language code control (AudioLanguageCodeControl) to Use configured (USE_CONFIGURED). The service also uses your specified custom language code when you set Language code control (AudioLanguageCodeControl) to Follow input (FOLLOW_INPUT), but your input file doesn't specify a language code. For all outputs, you can use an ISO 639-2 or ISO 639-3 code. For streaming outputs, you can also use any other code in the full RFC-5646 specification. Streaming outputs are those that are in one of the following output groups: CMAF, DASH ISO, Apple HLS, or Microsoft Smooth Streaming.
adCustomLanguageCode :: Lens' AudioDescription (Maybe Text)
adCustomLanguageCode = lens _adCustomLanguageCode (\s a -> s {_adCustomLanguageCode = a})

-- | Indicates the language of the audio output track. The ISO 639 language specified in the 'Language Code' drop down will be used when 'Follow Input Language Code' is not selected or when 'Follow Input Language Code' is selected but there is no ISO 639 language code specified by the input.
adLanguageCode :: Lens' AudioDescription (Maybe LanguageCode)
adLanguageCode = lens _adLanguageCode (\s a -> s {_adLanguageCode = a})

-- | When you mimic a multi-channel audio layout with multiple mono-channel tracks, you can tag each channel layout manually. For example, you would tag the tracks that contain your left, right, and center audio with Left (L), Right (R), and Center (C), respectively. When you don't specify a value, MediaConvert labels your track as Center (C) by default. To use audio layout tagging, your output must be in a QuickTime (.mov) container; your audio codec must be AAC, WAV, or AIFF; and you must set up your audio track to have only one channel.
adAudioChannelTaggingSettings :: Lens' AudioDescription (Maybe AudioChannelTaggingSettings)
adAudioChannelTaggingSettings = lens _adAudioChannelTaggingSettings (\s a -> s {_adAudioChannelTaggingSettings = a})

-- | Applies only if Follow Input Audio Type is unchecked (false). A number between 0 and 255. The following are defined in ISO-IEC 13818-1: 0 = Undefined, 1 = Clean Effects, 2 = Hearing Impaired, 3 = Visually Impaired Commentary, 4-255 = Reserved.
adAudioType :: Lens' AudioDescription (Maybe Natural)
adAudioType = lens _adAudioType (\s a -> s {_adAudioType = a}) . mapping _Nat

-- | Advanced audio normalization settings. Ignore these settings unless you need to comply with a loudness standard.
adAudioNormalizationSettings :: Lens' AudioDescription (Maybe AudioNormalizationSettings)
adAudioNormalizationSettings = lens _adAudioNormalizationSettings (\s a -> s {_adAudioNormalizationSettings = a})

-- | Specify which source for language code takes precedence for this audio track. When you choose Follow input (FOLLOW_INPUT), the service uses the language code from the input track if it's present. If there's no languge code on the input track, the service uses the code that you specify in the setting Language code (languageCode or customLanguageCode). When you choose Use configured (USE_CONFIGURED), the service uses the language code that you specify.
adLanguageCodeControl :: Lens' AudioDescription (Maybe AudioLanguageCodeControl)
adLanguageCodeControl = lens _adLanguageCodeControl (\s a -> s {_adLanguageCodeControl = a})

-- | Audio codec settings (CodecSettings) under (AudioDescriptions) contains the group of settings related to audio encoding. The settings in this group vary depending on the value that you choose for Audio codec (Codec). For each codec enum that you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * AAC, AacSettings * MP2, Mp2Settings * MP3, Mp3Settings * WAV, WavSettings * AIFF, AiffSettings * AC3, Ac3Settings * EAC3, Eac3Settings * EAC3_ATMOS, Eac3AtmosSettings * VORBIS, VorbisSettings * OPUS, OpusSettings
adCodecSettings :: Lens' AudioDescription (Maybe AudioCodecSettings)
adCodecSettings = lens _adCodecSettings (\s a -> s {_adCodecSettings = a})

-- | Specify a label for this output audio stream. For example, "English", "Director commentary", or "track_2". For streaming outputs, MediaConvert passes this information into destination manifests for display on the end-viewer's player device. For outputs in other output groups, the service ignores this setting.
adStreamName :: Lens' AudioDescription (Maybe Text)
adStreamName = lens _adStreamName (\s a -> s {_adStreamName = a})

-- | Advanced audio remixing settings.
adRemixSettings :: Lens' AudioDescription (Maybe RemixSettings)
adRemixSettings = lens _adRemixSettings (\s a -> s {_adRemixSettings = a})

-- | When set to FOLLOW_INPUT, if the input contains an ISO 639 audio_type, then that value is passed through to the output. If the input contains no ISO 639 audio_type, the value in Audio Type is included in the output. Otherwise the value in Audio Type is included in the output. Note that this field and audioType are both ignored if audioDescriptionBroadcasterMix is set to BROADCASTER_MIXED_AD.
adAudioTypeControl :: Lens' AudioDescription (Maybe AudioTypeControl)
adAudioTypeControl = lens _adAudioTypeControl (\s a -> s {_adAudioTypeControl = a})

instance FromJSON AudioDescription where
  parseJSON =
    withObject
      "AudioDescription"
      ( \x ->
          AudioDescription'
            <$> (x .:? "audioSourceName")
            <*> (x .:? "customLanguageCode")
            <*> (x .:? "languageCode")
            <*> (x .:? "audioChannelTaggingSettings")
            <*> (x .:? "audioType")
            <*> (x .:? "audioNormalizationSettings")
            <*> (x .:? "languageCodeControl")
            <*> (x .:? "codecSettings")
            <*> (x .:? "streamName")
            <*> (x .:? "remixSettings")
            <*> (x .:? "audioTypeControl")
      )

instance Hashable AudioDescription

instance NFData AudioDescription

instance ToJSON AudioDescription where
  toJSON AudioDescription' {..} =
    object
      ( catMaybes
          [ ("audioSourceName" .=) <$> _adAudioSourceName,
            ("customLanguageCode" .=) <$> _adCustomLanguageCode,
            ("languageCode" .=) <$> _adLanguageCode,
            ("audioChannelTaggingSettings" .=)
              <$> _adAudioChannelTaggingSettings,
            ("audioType" .=) <$> _adAudioType,
            ("audioNormalizationSettings" .=)
              <$> _adAudioNormalizationSettings,
            ("languageCodeControl" .=) <$> _adLanguageCodeControl,
            ("codecSettings" .=) <$> _adCodecSettings,
            ("streamName" .=) <$> _adStreamName,
            ("remixSettings" .=) <$> _adRemixSettings,
            ("audioTypeControl" .=) <$> _adAudioTypeControl
          ]
      )
