{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.KinesisVideoStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.KinesisVideoStream where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Kinesis video stream stream that provides the source streaming video for a Amazon Rekognition Video stream processor. For more information, see CreateStreamProcessor in the Amazon Rekognition Developer Guide.
--
--
--
-- /See:/ 'kinesisVideoStream' smart constructor.
newtype KinesisVideoStream = KinesisVideoStream'
  { _kvsARN ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisVideoStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kvsARN' - ARN of the Kinesis video stream stream that streams the source video.
kinesisVideoStream ::
  KinesisVideoStream
kinesisVideoStream = KinesisVideoStream' {_kvsARN = Nothing}

-- | ARN of the Kinesis video stream stream that streams the source video.
kvsARN :: Lens' KinesisVideoStream (Maybe Text)
kvsARN = lens _kvsARN (\s a -> s {_kvsARN = a})

instance FromJSON KinesisVideoStream where
  parseJSON =
    withObject
      "KinesisVideoStream"
      (\x -> KinesisVideoStream' <$> (x .:? "Arn"))

instance Hashable KinesisVideoStream

instance NFData KinesisVideoStream

instance ToJSON KinesisVideoStream where
  toJSON KinesisVideoStream' {..} =
    object (catMaybes [("Arn" .=) <$> _kvsARN])
