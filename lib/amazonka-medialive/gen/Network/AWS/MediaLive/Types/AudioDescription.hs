{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioDescription where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.AudioCodecSettings
import Network.AWS.MediaLive.Types.AudioDescriptionAudioTypeControl
import Network.AWS.MediaLive.Types.AudioDescriptionLanguageCodeControl
import Network.AWS.MediaLive.Types.AudioNormalizationSettings
import Network.AWS.MediaLive.Types.AudioType
import Network.AWS.MediaLive.Types.RemixSettings
import Network.AWS.Prelude

-- | Audio Description
--
-- /See:/ 'audioDescription' smart constructor.
data AudioDescription = AudioDescription'
  { _adLanguageCode ::
      !(Maybe Text),
    _adAudioType :: !(Maybe AudioType),
    _adAudioNormalizationSettings ::
      !(Maybe AudioNormalizationSettings),
    _adLanguageCodeControl ::
      !(Maybe AudioDescriptionLanguageCodeControl),
    _adCodecSettings :: !(Maybe AudioCodecSettings),
    _adStreamName :: !(Maybe Text),
    _adRemixSettings :: !(Maybe RemixSettings),
    _adAudioTypeControl ::
      !(Maybe AudioDescriptionAudioTypeControl),
    _adAudioSelectorName :: !Text,
    _adName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adLanguageCode' - RFC 5646 language code representing the language of the audio output track. Only used if languageControlMode is useConfigured, or there is no ISO 639 language code specified in the input.
--
-- * 'adAudioType' - Applies only if audioTypeControl is useConfigured. The values for audioType are defined in ISO-IEC 13818-1.
--
-- * 'adAudioNormalizationSettings' - Advanced audio normalization settings.
--
-- * 'adLanguageCodeControl' - Choosing followInput will cause the ISO 639 language code of the output to follow the ISO 639 language code of the input. The languageCode will be used when useConfigured is set, or when followInput is selected but there is no ISO 639 language code specified by the input.
--
-- * 'adCodecSettings' - Audio codec settings.
--
-- * 'adStreamName' - Used for MS Smooth and Apple HLS outputs. Indicates the name displayed by the player (eg. English, or Director Commentary).
--
-- * 'adRemixSettings' - Settings that control how input audio channels are remixed into the output audio channels.
--
-- * 'adAudioTypeControl' - Determines how audio type is determined.   followInput: If the input contains an ISO 639 audioType, then that value is passed through to the output. If the input contains no ISO 639 audioType, the value in Audio Type is included in the output.   useConfigured: The value in Audio Type is included in the output. Note that this field and audioType are both ignored if inputType is broadcasterMixedAd.
--
-- * 'adAudioSelectorName' - The name of the AudioSelector used as the source for this AudioDescription.
--
-- * 'adName' - The name of this AudioDescription. Outputs will use this name to uniquely identify this AudioDescription.  Description names should be unique within this Live Event.
audioDescription ::
  -- | 'adAudioSelectorName'
  Text ->
  -- | 'adName'
  Text ->
  AudioDescription
audioDescription pAudioSelectorName_ pName_ =
  AudioDescription'
    { _adLanguageCode = Nothing,
      _adAudioType = Nothing,
      _adAudioNormalizationSettings = Nothing,
      _adLanguageCodeControl = Nothing,
      _adCodecSettings = Nothing,
      _adStreamName = Nothing,
      _adRemixSettings = Nothing,
      _adAudioTypeControl = Nothing,
      _adAudioSelectorName = pAudioSelectorName_,
      _adName = pName_
    }

-- | RFC 5646 language code representing the language of the audio output track. Only used if languageControlMode is useConfigured, or there is no ISO 639 language code specified in the input.
adLanguageCode :: Lens' AudioDescription (Maybe Text)
adLanguageCode = lens _adLanguageCode (\s a -> s {_adLanguageCode = a})

-- | Applies only if audioTypeControl is useConfigured. The values for audioType are defined in ISO-IEC 13818-1.
adAudioType :: Lens' AudioDescription (Maybe AudioType)
adAudioType = lens _adAudioType (\s a -> s {_adAudioType = a})

-- | Advanced audio normalization settings.
adAudioNormalizationSettings :: Lens' AudioDescription (Maybe AudioNormalizationSettings)
adAudioNormalizationSettings = lens _adAudioNormalizationSettings (\s a -> s {_adAudioNormalizationSettings = a})

-- | Choosing followInput will cause the ISO 639 language code of the output to follow the ISO 639 language code of the input. The languageCode will be used when useConfigured is set, or when followInput is selected but there is no ISO 639 language code specified by the input.
adLanguageCodeControl :: Lens' AudioDescription (Maybe AudioDescriptionLanguageCodeControl)
adLanguageCodeControl = lens _adLanguageCodeControl (\s a -> s {_adLanguageCodeControl = a})

-- | Audio codec settings.
adCodecSettings :: Lens' AudioDescription (Maybe AudioCodecSettings)
adCodecSettings = lens _adCodecSettings (\s a -> s {_adCodecSettings = a})

-- | Used for MS Smooth and Apple HLS outputs. Indicates the name displayed by the player (eg. English, or Director Commentary).
adStreamName :: Lens' AudioDescription (Maybe Text)
adStreamName = lens _adStreamName (\s a -> s {_adStreamName = a})

-- | Settings that control how input audio channels are remixed into the output audio channels.
adRemixSettings :: Lens' AudioDescription (Maybe RemixSettings)
adRemixSettings = lens _adRemixSettings (\s a -> s {_adRemixSettings = a})

-- | Determines how audio type is determined.   followInput: If the input contains an ISO 639 audioType, then that value is passed through to the output. If the input contains no ISO 639 audioType, the value in Audio Type is included in the output.   useConfigured: The value in Audio Type is included in the output. Note that this field and audioType are both ignored if inputType is broadcasterMixedAd.
adAudioTypeControl :: Lens' AudioDescription (Maybe AudioDescriptionAudioTypeControl)
adAudioTypeControl = lens _adAudioTypeControl (\s a -> s {_adAudioTypeControl = a})

-- | The name of the AudioSelector used as the source for this AudioDescription.
adAudioSelectorName :: Lens' AudioDescription Text
adAudioSelectorName = lens _adAudioSelectorName (\s a -> s {_adAudioSelectorName = a})

-- | The name of this AudioDescription. Outputs will use this name to uniquely identify this AudioDescription.  Description names should be unique within this Live Event.
adName :: Lens' AudioDescription Text
adName = lens _adName (\s a -> s {_adName = a})

instance FromJSON AudioDescription where
  parseJSON =
    withObject
      "AudioDescription"
      ( \x ->
          AudioDescription'
            <$> (x .:? "languageCode")
            <*> (x .:? "audioType")
            <*> (x .:? "audioNormalizationSettings")
            <*> (x .:? "languageCodeControl")
            <*> (x .:? "codecSettings")
            <*> (x .:? "streamName")
            <*> (x .:? "remixSettings")
            <*> (x .:? "audioTypeControl")
            <*> (x .: "audioSelectorName")
            <*> (x .: "name")
      )

instance Hashable AudioDescription

instance NFData AudioDescription

instance ToJSON AudioDescription where
  toJSON AudioDescription' {..} =
    object
      ( catMaybes
          [ ("languageCode" .=) <$> _adLanguageCode,
            ("audioType" .=) <$> _adAudioType,
            ("audioNormalizationSettings" .=)
              <$> _adAudioNormalizationSettings,
            ("languageCodeControl" .=) <$> _adLanguageCodeControl,
            ("codecSettings" .=) <$> _adCodecSettings,
            ("streamName" .=) <$> _adStreamName,
            ("remixSettings" .=) <$> _adRemixSettings,
            ("audioTypeControl" .=) <$> _adAudioTypeControl,
            Just ("audioSelectorName" .= _adAudioSelectorName),
            Just ("name" .= _adName)
          ]
      )
