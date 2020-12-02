{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioSelector where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.AudioSelectorSettings
import Network.AWS.Prelude

-- | Audio Selector
--
-- /See:/ 'audioSelector' smart constructor.
data AudioSelector = AudioSelector'
  { _asSelectorSettings ::
      !(Maybe AudioSelectorSettings),
    _asName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asSelectorSettings' - The audio selector settings.
--
-- * 'asName' - The name of this AudioSelector. AudioDescriptions will use this name to uniquely identify this Selector.  Selector names should be unique per input.
audioSelector ::
  -- | 'asName'
  Text ->
  AudioSelector
audioSelector pName_ =
  AudioSelector' {_asSelectorSettings = Nothing, _asName = pName_}

-- | The audio selector settings.
asSelectorSettings :: Lens' AudioSelector (Maybe AudioSelectorSettings)
asSelectorSettings = lens _asSelectorSettings (\s a -> s {_asSelectorSettings = a})

-- | The name of this AudioSelector. AudioDescriptions will use this name to uniquely identify this Selector.  Selector names should be unique per input.
asName :: Lens' AudioSelector Text
asName = lens _asName (\s a -> s {_asName = a})

instance FromJSON AudioSelector where
  parseJSON =
    withObject
      "AudioSelector"
      ( \x ->
          AudioSelector' <$> (x .:? "selectorSettings") <*> (x .: "name")
      )

instance Hashable AudioSelector

instance NFData AudioSelector

instance ToJSON AudioSelector where
  toJSON AudioSelector' {..} =
    object
      ( catMaybes
          [ ("selectorSettings" .=) <$> _asSelectorSettings,
            Just ("name" .= _asName)
          ]
      )
