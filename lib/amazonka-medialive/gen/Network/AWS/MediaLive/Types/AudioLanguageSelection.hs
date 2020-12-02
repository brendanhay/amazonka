{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioLanguageSelection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioLanguageSelection where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.AudioLanguageSelectionPolicy
import Network.AWS.Prelude

-- | Audio Language Selection
--
-- /See:/ 'audioLanguageSelection' smart constructor.
data AudioLanguageSelection = AudioLanguageSelection'
  { _alsLanguageSelectionPolicy ::
      !(Maybe AudioLanguageSelectionPolicy),
    _alsLanguageCode :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioLanguageSelection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alsLanguageSelectionPolicy' - When set to "strict", the transport stream demux strictly identifies audio streams by their language descriptor. If a PMT update occurs such that an audio stream matching the initially selected language is no longer present then mute will be encoded until the language returns. If "loose", then on a PMT update the demux will choose another audio stream in the program with the same stream type if it can't find one with the same language.
--
-- * 'alsLanguageCode' - Selects a specific three-letter language code from within an audio source.
audioLanguageSelection ::
  -- | 'alsLanguageCode'
  Text ->
  AudioLanguageSelection
audioLanguageSelection pLanguageCode_ =
  AudioLanguageSelection'
    { _alsLanguageSelectionPolicy = Nothing,
      _alsLanguageCode = pLanguageCode_
    }

-- | When set to "strict", the transport stream demux strictly identifies audio streams by their language descriptor. If a PMT update occurs such that an audio stream matching the initially selected language is no longer present then mute will be encoded until the language returns. If "loose", then on a PMT update the demux will choose another audio stream in the program with the same stream type if it can't find one with the same language.
alsLanguageSelectionPolicy :: Lens' AudioLanguageSelection (Maybe AudioLanguageSelectionPolicy)
alsLanguageSelectionPolicy = lens _alsLanguageSelectionPolicy (\s a -> s {_alsLanguageSelectionPolicy = a})

-- | Selects a specific three-letter language code from within an audio source.
alsLanguageCode :: Lens' AudioLanguageSelection Text
alsLanguageCode = lens _alsLanguageCode (\s a -> s {_alsLanguageCode = a})

instance FromJSON AudioLanguageSelection where
  parseJSON =
    withObject
      "AudioLanguageSelection"
      ( \x ->
          AudioLanguageSelection'
            <$> (x .:? "languageSelectionPolicy") <*> (x .: "languageCode")
      )

instance Hashable AudioLanguageSelection

instance NFData AudioLanguageSelection

instance ToJSON AudioLanguageSelection where
  toJSON AudioLanguageSelection' {..} =
    object
      ( catMaybes
          [ ("languageSelectionPolicy" .=) <$> _alsLanguageSelectionPolicy,
            Just ("languageCode" .= _alsLanguageCode)
          ]
      )
