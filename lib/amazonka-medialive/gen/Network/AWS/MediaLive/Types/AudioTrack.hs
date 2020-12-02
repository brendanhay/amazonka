{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioTrack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioTrack where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Audio Track
--
-- /See:/ 'audioTrack' smart constructor.
newtype AudioTrack = AudioTrack' {_atTrack :: Nat}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioTrack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atTrack' - 1-based integer value that maps to a specific audio track
audioTrack ::
  -- | 'atTrack'
  Natural ->
  AudioTrack
audioTrack pTrack_ = AudioTrack' {_atTrack = _Nat # pTrack_}

-- | 1-based integer value that maps to a specific audio track
atTrack :: Lens' AudioTrack Natural
atTrack = lens _atTrack (\s a -> s {_atTrack = a}) . _Nat

instance FromJSON AudioTrack where
  parseJSON =
    withObject "AudioTrack" (\x -> AudioTrack' <$> (x .: "track"))

instance Hashable AudioTrack

instance NFData AudioTrack

instance ToJSON AudioTrack where
  toJSON AudioTrack' {..} =
    object (catMaybes [Just ("track" .= _atTrack)])
