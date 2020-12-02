{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioPidSelection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioPidSelection where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Audio Pid Selection
--
-- /See:/ 'audioPidSelection' smart constructor.
newtype AudioPidSelection = AudioPidSelection' {_apsPid :: Nat}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioPidSelection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apsPid' - Selects a specific PID from within a source.
audioPidSelection ::
  -- | 'apsPid'
  Natural ->
  AudioPidSelection
audioPidSelection pPid_ =
  AudioPidSelection' {_apsPid = _Nat # pPid_}

-- | Selects a specific PID from within a source.
apsPid :: Lens' AudioPidSelection Natural
apsPid = lens _apsPid (\s a -> s {_apsPid = a}) . _Nat

instance FromJSON AudioPidSelection where
  parseJSON =
    withObject
      "AudioPidSelection"
      (\x -> AudioPidSelection' <$> (x .: "pid"))

instance Hashable AudioPidSelection

instance NFData AudioPidSelection

instance ToJSON AudioPidSelection where
  toJSON AudioPidSelection' {..} =
    object (catMaybes [Just ("pid" .= _apsPid)])
