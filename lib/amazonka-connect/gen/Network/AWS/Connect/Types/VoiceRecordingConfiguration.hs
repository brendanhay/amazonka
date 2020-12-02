{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.VoiceRecordingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.VoiceRecordingConfiguration where

import Network.AWS.Connect.Types.VoiceRecordingTrack
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the recording configuration settings.
--
--
--
-- /See:/ 'voiceRecordingConfiguration' smart constructor.
newtype VoiceRecordingConfiguration = VoiceRecordingConfiguration'
  { _vrcVoiceRecordingTrack ::
      Maybe VoiceRecordingTrack
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VoiceRecordingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vrcVoiceRecordingTrack' - Identifies which track is being recorded.
voiceRecordingConfiguration ::
  VoiceRecordingConfiguration
voiceRecordingConfiguration =
  VoiceRecordingConfiguration' {_vrcVoiceRecordingTrack = Nothing}

-- | Identifies which track is being recorded.
vrcVoiceRecordingTrack :: Lens' VoiceRecordingConfiguration (Maybe VoiceRecordingTrack)
vrcVoiceRecordingTrack = lens _vrcVoiceRecordingTrack (\s a -> s {_vrcVoiceRecordingTrack = a})

instance Hashable VoiceRecordingConfiguration

instance NFData VoiceRecordingConfiguration

instance ToJSON VoiceRecordingConfiguration where
  toJSON VoiceRecordingConfiguration' {..} =
    object
      ( catMaybes
          [("VoiceRecordingTrack" .=) <$> _vrcVoiceRecordingTrack]
      )
