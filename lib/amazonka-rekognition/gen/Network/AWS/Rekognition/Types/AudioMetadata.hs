{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.AudioMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.AudioMetadata where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Metadata information about an audio stream. An array of @AudioMetadata@ objects for the audio streams found in a stored video is returned by 'GetSegmentDetection' .
--
--
--
-- /See:/ 'audioMetadata' smart constructor.
data AudioMetadata = AudioMetadata'
  { _amCodec :: !(Maybe Text),
    _amSampleRate :: !(Maybe Nat),
    _amNumberOfChannels :: !(Maybe Nat),
    _amDurationMillis :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amCodec' - The audio codec used to encode or decode the audio stream.
--
-- * 'amSampleRate' - The sample rate for the audio stream.
--
-- * 'amNumberOfChannels' - The number of audio channels in the segment.
--
-- * 'amDurationMillis' - The duration of the audio stream in milliseconds.
audioMetadata ::
  AudioMetadata
audioMetadata =
  AudioMetadata'
    { _amCodec = Nothing,
      _amSampleRate = Nothing,
      _amNumberOfChannels = Nothing,
      _amDurationMillis = Nothing
    }

-- | The audio codec used to encode or decode the audio stream.
amCodec :: Lens' AudioMetadata (Maybe Text)
amCodec = lens _amCodec (\s a -> s {_amCodec = a})

-- | The sample rate for the audio stream.
amSampleRate :: Lens' AudioMetadata (Maybe Natural)
amSampleRate = lens _amSampleRate (\s a -> s {_amSampleRate = a}) . mapping _Nat

-- | The number of audio channels in the segment.
amNumberOfChannels :: Lens' AudioMetadata (Maybe Natural)
amNumberOfChannels = lens _amNumberOfChannels (\s a -> s {_amNumberOfChannels = a}) . mapping _Nat

-- | The duration of the audio stream in milliseconds.
amDurationMillis :: Lens' AudioMetadata (Maybe Natural)
amDurationMillis = lens _amDurationMillis (\s a -> s {_amDurationMillis = a}) . mapping _Nat

instance FromJSON AudioMetadata where
  parseJSON =
    withObject
      "AudioMetadata"
      ( \x ->
          AudioMetadata'
            <$> (x .:? "Codec")
            <*> (x .:? "SampleRate")
            <*> (x .:? "NumberOfChannels")
            <*> (x .:? "DurationMillis")
      )

instance Hashable AudioMetadata

instance NFData AudioMetadata
