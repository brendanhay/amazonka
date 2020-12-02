{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.StreamProcessorOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StreamProcessorOutput where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.KinesisDataStream

-- | Information about the Amazon Kinesis Data Streams stream to which a Amazon Rekognition Video stream processor streams the results of a video analysis. For more information, see CreateStreamProcessor in the Amazon Rekognition Developer Guide.
--
--
--
-- /See:/ 'streamProcessorOutput' smart constructor.
newtype StreamProcessorOutput = StreamProcessorOutput'
  { _spoKinesisDataStream ::
      Maybe KinesisDataStream
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StreamProcessorOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spoKinesisDataStream' - The Amazon Kinesis Data Streams stream to which the Amazon Rekognition stream processor streams the analysis results.
streamProcessorOutput ::
  StreamProcessorOutput
streamProcessorOutput =
  StreamProcessorOutput' {_spoKinesisDataStream = Nothing}

-- | The Amazon Kinesis Data Streams stream to which the Amazon Rekognition stream processor streams the analysis results.
spoKinesisDataStream :: Lens' StreamProcessorOutput (Maybe KinesisDataStream)
spoKinesisDataStream = lens _spoKinesisDataStream (\s a -> s {_spoKinesisDataStream = a})

instance FromJSON StreamProcessorOutput where
  parseJSON =
    withObject
      "StreamProcessorOutput"
      (\x -> StreamProcessorOutput' <$> (x .:? "KinesisDataStream"))

instance Hashable StreamProcessorOutput

instance NFData StreamProcessorOutput

instance ToJSON StreamProcessorOutput where
  toJSON StreamProcessorOutput' {..} =
    object
      (catMaybes [("KinesisDataStream" .=) <$> _spoKinesisDataStream])
