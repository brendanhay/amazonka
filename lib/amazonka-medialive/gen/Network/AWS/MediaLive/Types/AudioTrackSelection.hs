{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioTrackSelection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioTrackSelection where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.AudioTrack
import Network.AWS.Prelude

-- | Audio Track Selection
--
-- /See:/ 'audioTrackSelection' smart constructor.
newtype AudioTrackSelection = AudioTrackSelection'
  { _atsTracks ::
      [AudioTrack]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioTrackSelection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atsTracks' - Selects one or more unique audio tracks from within a source.
audioTrackSelection ::
  AudioTrackSelection
audioTrackSelection = AudioTrackSelection' {_atsTracks = mempty}

-- | Selects one or more unique audio tracks from within a source.
atsTracks :: Lens' AudioTrackSelection [AudioTrack]
atsTracks = lens _atsTracks (\s a -> s {_atsTracks = a}) . _Coerce

instance FromJSON AudioTrackSelection where
  parseJSON =
    withObject
      "AudioTrackSelection"
      (\x -> AudioTrackSelection' <$> (x .:? "tracks" .!= mempty))

instance Hashable AudioTrackSelection

instance NFData AudioTrackSelection

instance ToJSON AudioTrackSelection where
  toJSON AudioTrackSelection' {..} =
    object (catMaybes [Just ("tracks" .= _atsTracks)])
