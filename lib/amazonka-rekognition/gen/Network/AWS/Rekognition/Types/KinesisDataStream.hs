{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.KinesisDataStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.KinesisDataStream where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Kinesis data stream Amazon Rekognition to which the analysis results of a Amazon Rekognition stream processor are streamed. For more information, see CreateStreamProcessor in the Amazon Rekognition Developer Guide.
--
--
--
-- /See:/ 'kinesisDataStream' smart constructor.
newtype KinesisDataStream = KinesisDataStream'
  { _kdsARN ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisDataStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kdsARN' - ARN of the output Amazon Kinesis Data Streams stream.
kinesisDataStream ::
  KinesisDataStream
kinesisDataStream = KinesisDataStream' {_kdsARN = Nothing}

-- | ARN of the output Amazon Kinesis Data Streams stream.
kdsARN :: Lens' KinesisDataStream (Maybe Text)
kdsARN = lens _kdsARN (\s a -> s {_kdsARN = a})

instance FromJSON KinesisDataStream where
  parseJSON =
    withObject
      "KinesisDataStream"
      (\x -> KinesisDataStream' <$> (x .:? "Arn"))

instance Hashable KinesisDataStream

instance NFData KinesisDataStream

instance ToJSON KinesisDataStream where
  toJSON KinesisDataStream' {..} =
    object (catMaybes [("Arn" .=) <$> _kdsARN])
