{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.StreamProcessorInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StreamProcessorInput where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.KinesisVideoStream

-- | Information about the source streaming video.
--
--
--
-- /See:/ 'streamProcessorInput' smart constructor.
newtype StreamProcessorInput = StreamProcessorInput'
  { _spiKinesisVideoStream ::
      Maybe KinesisVideoStream
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StreamProcessorInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spiKinesisVideoStream' - The Kinesis video stream input stream for the source streaming video.
streamProcessorInput ::
  StreamProcessorInput
streamProcessorInput =
  StreamProcessorInput' {_spiKinesisVideoStream = Nothing}

-- | The Kinesis video stream input stream for the source streaming video.
spiKinesisVideoStream :: Lens' StreamProcessorInput (Maybe KinesisVideoStream)
spiKinesisVideoStream = lens _spiKinesisVideoStream (\s a -> s {_spiKinesisVideoStream = a})

instance FromJSON StreamProcessorInput where
  parseJSON =
    withObject
      "StreamProcessorInput"
      (\x -> StreamProcessorInput' <$> (x .:? "KinesisVideoStream"))

instance Hashable StreamProcessorInput

instance NFData StreamProcessorInput

instance ToJSON StreamProcessorInput where
  toJSON StreamProcessorInput' {..} =
    object
      (catMaybes [("KinesisVideoStream" .=) <$> _spiKinesisVideoStream])
