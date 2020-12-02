{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Fmp4HlsSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Fmp4HlsSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.Fmp4NielsenId3Behavior
import Network.AWS.MediaLive.Types.Fmp4TimedMetadataBehavior
import Network.AWS.Prelude

-- | Fmp4 Hls Settings
--
-- /See:/ 'fmp4HlsSettings' smart constructor.
data Fmp4HlsSettings = Fmp4HlsSettings'
  { _fhsNielsenId3Behavior ::
      !(Maybe Fmp4NielsenId3Behavior),
    _fhsAudioRenditionSets :: !(Maybe Text),
    _fhsTimedMetadataBehavior ::
      !(Maybe Fmp4TimedMetadataBehavior)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Fmp4HlsSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fhsNielsenId3Behavior' - If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
--
-- * 'fhsAudioRenditionSets' - List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
--
-- * 'fhsTimedMetadataBehavior' - When set to passthrough, timed metadata is passed through from input to output.
fmp4HlsSettings ::
  Fmp4HlsSettings
fmp4HlsSettings =
  Fmp4HlsSettings'
    { _fhsNielsenId3Behavior = Nothing,
      _fhsAudioRenditionSets = Nothing,
      _fhsTimedMetadataBehavior = Nothing
    }

-- | If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
fhsNielsenId3Behavior :: Lens' Fmp4HlsSettings (Maybe Fmp4NielsenId3Behavior)
fhsNielsenId3Behavior = lens _fhsNielsenId3Behavior (\s a -> s {_fhsNielsenId3Behavior = a})

-- | List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
fhsAudioRenditionSets :: Lens' Fmp4HlsSettings (Maybe Text)
fhsAudioRenditionSets = lens _fhsAudioRenditionSets (\s a -> s {_fhsAudioRenditionSets = a})

-- | When set to passthrough, timed metadata is passed through from input to output.
fhsTimedMetadataBehavior :: Lens' Fmp4HlsSettings (Maybe Fmp4TimedMetadataBehavior)
fhsTimedMetadataBehavior = lens _fhsTimedMetadataBehavior (\s a -> s {_fhsTimedMetadataBehavior = a})

instance FromJSON Fmp4HlsSettings where
  parseJSON =
    withObject
      "Fmp4HlsSettings"
      ( \x ->
          Fmp4HlsSettings'
            <$> (x .:? "nielsenId3Behavior")
            <*> (x .:? "audioRenditionSets")
            <*> (x .:? "timedMetadataBehavior")
      )

instance Hashable Fmp4HlsSettings

instance NFData Fmp4HlsSettings

instance ToJSON Fmp4HlsSettings where
  toJSON Fmp4HlsSettings' {..} =
    object
      ( catMaybes
          [ ("nielsenId3Behavior" .=) <$> _fhsNielsenId3Behavior,
            ("audioRenditionSets" .=) <$> _fhsAudioRenditionSets,
            ("timedMetadataBehavior" .=) <$> _fhsTimedMetadataBehavior
          ]
      )
