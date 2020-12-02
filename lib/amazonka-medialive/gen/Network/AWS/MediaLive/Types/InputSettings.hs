{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.AudioSelector
import Network.AWS.MediaLive.Types.CaptionSelector
import Network.AWS.MediaLive.Types.InputDeblockFilter
import Network.AWS.MediaLive.Types.InputDenoiseFilter
import Network.AWS.MediaLive.Types.InputFilter
import Network.AWS.MediaLive.Types.InputSourceEndBehavior
import Network.AWS.MediaLive.Types.NetworkInputSettings
import Network.AWS.MediaLive.Types.Smpte2038DataPreference
import Network.AWS.MediaLive.Types.VideoSelector
import Network.AWS.Prelude

-- | Live Event input parameters. There can be multiple inputs in a single Live Event.
--
-- /See:/ 'inputSettings' smart constructor.
data InputSettings = InputSettings'
  { _isVideoSelector ::
      !(Maybe VideoSelector),
    _isSmpte2038DataPreference :: !(Maybe Smpte2038DataPreference),
    _isNetworkInputSettings :: !(Maybe NetworkInputSettings),
    _isAudioSelectors :: !(Maybe [AudioSelector]),
    _isDeblockFilter :: !(Maybe InputDeblockFilter),
    _isDenoiseFilter :: !(Maybe InputDenoiseFilter),
    _isFilterStrength :: !(Maybe Nat),
    _isCaptionSelectors :: !(Maybe [CaptionSelector]),
    _isInputFilter :: !(Maybe InputFilter),
    _isSourceEndBehavior :: !(Maybe InputSourceEndBehavior)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isVideoSelector' - Informs which video elementary stream to decode for input types that have multiple available.
--
-- * 'isSmpte2038DataPreference' - Specifies whether to extract applicable ancillary data from a SMPTE-2038 source in this input. Applicable data types are captions, timecode, AFD, and SCTE-104 messages. - PREFER: Extract from SMPTE-2038 if present in this input, otherwise extract from another source (if any). - IGNORE: Never extract any ancillary data from SMPTE-2038.
--
-- * 'isNetworkInputSettings' - Input settings.
--
-- * 'isAudioSelectors' - Used to select the audio stream to decode for inputs that have multiple available.
--
-- * 'isDeblockFilter' - Enable or disable the deblock filter when filtering.
--
-- * 'isDenoiseFilter' - Enable or disable the denoise filter when filtering.
--
-- * 'isFilterStrength' - Adjusts the magnitude of filtering from 1 (minimal) to 5 (strongest).
--
-- * 'isCaptionSelectors' - Used to select the caption input to use for inputs that have multiple available.
--
-- * 'isInputFilter' - Turns on the filter for this input. MPEG-2 inputs have the deblocking filter enabled by default. 1) auto - filtering will be applied depending on input type/quality 2) disabled - no filtering will be applied to the input 3) forced - filtering will be applied regardless of input type
--
-- * 'isSourceEndBehavior' - Loop input if it is a file. This allows a file input to be streamed indefinitely.
inputSettings ::
  InputSettings
inputSettings =
  InputSettings'
    { _isVideoSelector = Nothing,
      _isSmpte2038DataPreference = Nothing,
      _isNetworkInputSettings = Nothing,
      _isAudioSelectors = Nothing,
      _isDeblockFilter = Nothing,
      _isDenoiseFilter = Nothing,
      _isFilterStrength = Nothing,
      _isCaptionSelectors = Nothing,
      _isInputFilter = Nothing,
      _isSourceEndBehavior = Nothing
    }

-- | Informs which video elementary stream to decode for input types that have multiple available.
isVideoSelector :: Lens' InputSettings (Maybe VideoSelector)
isVideoSelector = lens _isVideoSelector (\s a -> s {_isVideoSelector = a})

-- | Specifies whether to extract applicable ancillary data from a SMPTE-2038 source in this input. Applicable data types are captions, timecode, AFD, and SCTE-104 messages. - PREFER: Extract from SMPTE-2038 if present in this input, otherwise extract from another source (if any). - IGNORE: Never extract any ancillary data from SMPTE-2038.
isSmpte2038DataPreference :: Lens' InputSettings (Maybe Smpte2038DataPreference)
isSmpte2038DataPreference = lens _isSmpte2038DataPreference (\s a -> s {_isSmpte2038DataPreference = a})

-- | Input settings.
isNetworkInputSettings :: Lens' InputSettings (Maybe NetworkInputSettings)
isNetworkInputSettings = lens _isNetworkInputSettings (\s a -> s {_isNetworkInputSettings = a})

-- | Used to select the audio stream to decode for inputs that have multiple available.
isAudioSelectors :: Lens' InputSettings [AudioSelector]
isAudioSelectors = lens _isAudioSelectors (\s a -> s {_isAudioSelectors = a}) . _Default . _Coerce

-- | Enable or disable the deblock filter when filtering.
isDeblockFilter :: Lens' InputSettings (Maybe InputDeblockFilter)
isDeblockFilter = lens _isDeblockFilter (\s a -> s {_isDeblockFilter = a})

-- | Enable or disable the denoise filter when filtering.
isDenoiseFilter :: Lens' InputSettings (Maybe InputDenoiseFilter)
isDenoiseFilter = lens _isDenoiseFilter (\s a -> s {_isDenoiseFilter = a})

-- | Adjusts the magnitude of filtering from 1 (minimal) to 5 (strongest).
isFilterStrength :: Lens' InputSettings (Maybe Natural)
isFilterStrength = lens _isFilterStrength (\s a -> s {_isFilterStrength = a}) . mapping _Nat

-- | Used to select the caption input to use for inputs that have multiple available.
isCaptionSelectors :: Lens' InputSettings [CaptionSelector]
isCaptionSelectors = lens _isCaptionSelectors (\s a -> s {_isCaptionSelectors = a}) . _Default . _Coerce

-- | Turns on the filter for this input. MPEG-2 inputs have the deblocking filter enabled by default. 1) auto - filtering will be applied depending on input type/quality 2) disabled - no filtering will be applied to the input 3) forced - filtering will be applied regardless of input type
isInputFilter :: Lens' InputSettings (Maybe InputFilter)
isInputFilter = lens _isInputFilter (\s a -> s {_isInputFilter = a})

-- | Loop input if it is a file. This allows a file input to be streamed indefinitely.
isSourceEndBehavior :: Lens' InputSettings (Maybe InputSourceEndBehavior)
isSourceEndBehavior = lens _isSourceEndBehavior (\s a -> s {_isSourceEndBehavior = a})

instance FromJSON InputSettings where
  parseJSON =
    withObject
      "InputSettings"
      ( \x ->
          InputSettings'
            <$> (x .:? "videoSelector")
            <*> (x .:? "smpte2038DataPreference")
            <*> (x .:? "networkInputSettings")
            <*> (x .:? "audioSelectors" .!= mempty)
            <*> (x .:? "deblockFilter")
            <*> (x .:? "denoiseFilter")
            <*> (x .:? "filterStrength")
            <*> (x .:? "captionSelectors" .!= mempty)
            <*> (x .:? "inputFilter")
            <*> (x .:? "sourceEndBehavior")
      )

instance Hashable InputSettings

instance NFData InputSettings

instance ToJSON InputSettings where
  toJSON InputSettings' {..} =
    object
      ( catMaybes
          [ ("videoSelector" .=) <$> _isVideoSelector,
            ("smpte2038DataPreference" .=) <$> _isSmpte2038DataPreference,
            ("networkInputSettings" .=) <$> _isNetworkInputSettings,
            ("audioSelectors" .=) <$> _isAudioSelectors,
            ("deblockFilter" .=) <$> _isDeblockFilter,
            ("denoiseFilter" .=) <$> _isDenoiseFilter,
            ("filterStrength" .=) <$> _isFilterStrength,
            ("captionSelectors" .=) <$> _isCaptionSelectors,
            ("inputFilter" .=) <$> _isInputFilter,
            ("sourceEndBehavior" .=) <$> _isSourceEndBehavior
          ]
      )
