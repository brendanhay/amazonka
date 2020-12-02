{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioSelectorGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioSelectorGroup where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Group of Audio Selectors
--
-- /See:/ 'audioSelectorGroup' smart constructor.
newtype AudioSelectorGroup = AudioSelectorGroup'
  { _asgAudioSelectorNames ::
      Maybe [Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioSelectorGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asgAudioSelectorNames' - Name of an Audio Selector within the same input to include in the group.  Audio selector names are standardized, based on their order within the input (e.g., "Audio Selector 1"). The audio selector name parameter can be repeated to add any number of audio selectors to the group.
audioSelectorGroup ::
  AudioSelectorGroup
audioSelectorGroup =
  AudioSelectorGroup' {_asgAudioSelectorNames = Nothing}

-- | Name of an Audio Selector within the same input to include in the group.  Audio selector names are standardized, based on their order within the input (e.g., "Audio Selector 1"). The audio selector name parameter can be repeated to add any number of audio selectors to the group.
asgAudioSelectorNames :: Lens' AudioSelectorGroup [Text]
asgAudioSelectorNames = lens _asgAudioSelectorNames (\s a -> s {_asgAudioSelectorNames = a}) . _Default . _Coerce

instance FromJSON AudioSelectorGroup where
  parseJSON =
    withObject
      "AudioSelectorGroup"
      ( \x ->
          AudioSelectorGroup' <$> (x .:? "audioSelectorNames" .!= mempty)
      )

instance Hashable AudioSelectorGroup

instance NFData AudioSelectorGroup

instance ToJSON AudioSelectorGroup where
  toJSON AudioSelectorGroup' {..} =
    object
      (catMaybes [("audioSelectorNames" .=) <$> _asgAudioSelectorNames])
