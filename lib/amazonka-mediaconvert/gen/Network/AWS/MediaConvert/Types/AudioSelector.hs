{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioSelector where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.AudioDefaultSelection
import Network.AWS.MediaConvert.Types.AudioSelectorType
import Network.AWS.MediaConvert.Types.LanguageCode
import Network.AWS.MediaConvert.Types.RemixSettings
import Network.AWS.Prelude

-- | Selector for Audio
--
-- /See:/ 'audioSelector' smart constructor.
data AudioSelector = AudioSelector'
  { _asTracks :: !(Maybe [Nat]),
    _asCustomLanguageCode :: !(Maybe Text),
    _asProgramSelection :: !(Maybe Nat),
    _asLanguageCode :: !(Maybe LanguageCode),
    _asOffset :: !(Maybe Int),
    _asDefaultSelection :: !(Maybe AudioDefaultSelection),
    _asPids :: !(Maybe [Nat]),
    _asSelectorType :: !(Maybe AudioSelectorType),
    _asExternalAudioFileInput :: !(Maybe Text),
    _asRemixSettings :: !(Maybe RemixSettings)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asTracks' - Identify a track from the input audio to include in this selector by entering the track index number. To include several tracks in a single audio selector, specify multiple tracks as follows. Using the console, enter a comma-separated list. For examle, type "1,2,3" to include tracks 1 through 3. Specifying directly in your JSON job file, provide the track numbers in an array. For example, "tracks": [1,2,3].
--
-- * 'asCustomLanguageCode' - Selects a specific language code from within an audio source, using the ISO 639-2 or ISO 639-3 three-letter language code
--
-- * 'asProgramSelection' - Use this setting for input streams that contain Dolby E, to have the service extract specific program data from the track. To select multiple programs, create multiple selectors with the same Track and different Program numbers. In the console, this setting is visible when you set Selector type to Track. Choose the program number from the dropdown list. If you are sending a JSON file, provide the program ID, which is part of the audio metadata. If your input file has incorrect metadata, you can choose All channels instead of a program number to have the service ignore the program IDs and include all the programs in the track.
--
-- * 'asLanguageCode' - Selects a specific language code from within an audio source.
--
-- * 'asOffset' - Specifies a time delta in milliseconds to offset the audio from the input video.
--
-- * 'asDefaultSelection' - Enable this setting on one audio selector to set it as the default for the job. The service uses this default for outputs where it can't find the specified input audio. If you don't set a default, those outputs have no audio.
--
-- * 'asPids' - Selects a specific PID from within an audio source (e.g. 257 selects PID 0x101).
--
-- * 'asSelectorType' - Specifies the type of the audio selector.
--
-- * 'asExternalAudioFileInput' - Specifies audio data from an external file source.
--
-- * 'asRemixSettings' - Use these settings to reorder the audio channels of one input to match those of another input. This allows you to combine the two files into a single output, one after the other.
audioSelector ::
  AudioSelector
audioSelector =
  AudioSelector'
    { _asTracks = Nothing,
      _asCustomLanguageCode = Nothing,
      _asProgramSelection = Nothing,
      _asLanguageCode = Nothing,
      _asOffset = Nothing,
      _asDefaultSelection = Nothing,
      _asPids = Nothing,
      _asSelectorType = Nothing,
      _asExternalAudioFileInput = Nothing,
      _asRemixSettings = Nothing
    }

-- | Identify a track from the input audio to include in this selector by entering the track index number. To include several tracks in a single audio selector, specify multiple tracks as follows. Using the console, enter a comma-separated list. For examle, type "1,2,3" to include tracks 1 through 3. Specifying directly in your JSON job file, provide the track numbers in an array. For example, "tracks": [1,2,3].
asTracks :: Lens' AudioSelector [Natural]
asTracks = lens _asTracks (\s a -> s {_asTracks = a}) . _Default . _Coerce

-- | Selects a specific language code from within an audio source, using the ISO 639-2 or ISO 639-3 three-letter language code
asCustomLanguageCode :: Lens' AudioSelector (Maybe Text)
asCustomLanguageCode = lens _asCustomLanguageCode (\s a -> s {_asCustomLanguageCode = a})

-- | Use this setting for input streams that contain Dolby E, to have the service extract specific program data from the track. To select multiple programs, create multiple selectors with the same Track and different Program numbers. In the console, this setting is visible when you set Selector type to Track. Choose the program number from the dropdown list. If you are sending a JSON file, provide the program ID, which is part of the audio metadata. If your input file has incorrect metadata, you can choose All channels instead of a program number to have the service ignore the program IDs and include all the programs in the track.
asProgramSelection :: Lens' AudioSelector (Maybe Natural)
asProgramSelection = lens _asProgramSelection (\s a -> s {_asProgramSelection = a}) . mapping _Nat

-- | Selects a specific language code from within an audio source.
asLanguageCode :: Lens' AudioSelector (Maybe LanguageCode)
asLanguageCode = lens _asLanguageCode (\s a -> s {_asLanguageCode = a})

-- | Specifies a time delta in milliseconds to offset the audio from the input video.
asOffset :: Lens' AudioSelector (Maybe Int)
asOffset = lens _asOffset (\s a -> s {_asOffset = a})

-- | Enable this setting on one audio selector to set it as the default for the job. The service uses this default for outputs where it can't find the specified input audio. If you don't set a default, those outputs have no audio.
asDefaultSelection :: Lens' AudioSelector (Maybe AudioDefaultSelection)
asDefaultSelection = lens _asDefaultSelection (\s a -> s {_asDefaultSelection = a})

-- | Selects a specific PID from within an audio source (e.g. 257 selects PID 0x101).
asPids :: Lens' AudioSelector [Natural]
asPids = lens _asPids (\s a -> s {_asPids = a}) . _Default . _Coerce

-- | Specifies the type of the audio selector.
asSelectorType :: Lens' AudioSelector (Maybe AudioSelectorType)
asSelectorType = lens _asSelectorType (\s a -> s {_asSelectorType = a})

-- | Specifies audio data from an external file source.
asExternalAudioFileInput :: Lens' AudioSelector (Maybe Text)
asExternalAudioFileInput = lens _asExternalAudioFileInput (\s a -> s {_asExternalAudioFileInput = a})

-- | Use these settings to reorder the audio channels of one input to match those of another input. This allows you to combine the two files into a single output, one after the other.
asRemixSettings :: Lens' AudioSelector (Maybe RemixSettings)
asRemixSettings = lens _asRemixSettings (\s a -> s {_asRemixSettings = a})

instance FromJSON AudioSelector where
  parseJSON =
    withObject
      "AudioSelector"
      ( \x ->
          AudioSelector'
            <$> (x .:? "tracks" .!= mempty)
            <*> (x .:? "customLanguageCode")
            <*> (x .:? "programSelection")
            <*> (x .:? "languageCode")
            <*> (x .:? "offset")
            <*> (x .:? "defaultSelection")
            <*> (x .:? "pids" .!= mempty)
            <*> (x .:? "selectorType")
            <*> (x .:? "externalAudioFileInput")
            <*> (x .:? "remixSettings")
      )

instance Hashable AudioSelector

instance NFData AudioSelector

instance ToJSON AudioSelector where
  toJSON AudioSelector' {..} =
    object
      ( catMaybes
          [ ("tracks" .=) <$> _asTracks,
            ("customLanguageCode" .=) <$> _asCustomLanguageCode,
            ("programSelection" .=) <$> _asProgramSelection,
            ("languageCode" .=) <$> _asLanguageCode,
            ("offset" .=) <$> _asOffset,
            ("defaultSelection" .=) <$> _asDefaultSelection,
            ("pids" .=) <$> _asPids,
            ("selectorType" .=) <$> _asSelectorType,
            ("externalAudioFileInput" .=) <$> _asExternalAudioFileInput,
            ("remixSettings" .=) <$> _asRemixSettings
          ]
      )
