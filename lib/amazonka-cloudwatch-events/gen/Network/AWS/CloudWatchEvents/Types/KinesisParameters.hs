{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.KinesisParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.KinesisParameters where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | This object enables you to specify a JSON path to extract from the event and use as the partition key for the Amazon Kinesis data stream, so that you can control the shard to which the event goes. If you do not include this parameter, the default is to use the @eventId@ as the partition key.
--
--
--
-- /See:/ 'kinesisParameters' smart constructor.
newtype KinesisParameters = KinesisParameters'
  { _kpPartitionKeyPath ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kpPartitionKeyPath' - The JSON path to be extracted from the event and used as the partition key. For more information, see <https://docs.aws.amazon.com/streams/latest/dev/key-concepts.html#partition-key Amazon Kinesis Streams Key Concepts> in the /Amazon Kinesis Streams Developer Guide/ .
kinesisParameters ::
  -- | 'kpPartitionKeyPath'
  Text ->
  KinesisParameters
kinesisParameters pPartitionKeyPath_ =
  KinesisParameters' {_kpPartitionKeyPath = pPartitionKeyPath_}

-- | The JSON path to be extracted from the event and used as the partition key. For more information, see <https://docs.aws.amazon.com/streams/latest/dev/key-concepts.html#partition-key Amazon Kinesis Streams Key Concepts> in the /Amazon Kinesis Streams Developer Guide/ .
kpPartitionKeyPath :: Lens' KinesisParameters Text
kpPartitionKeyPath = lens _kpPartitionKeyPath (\s a -> s {_kpPartitionKeyPath = a})

instance FromJSON KinesisParameters where
  parseJSON =
    withObject
      "KinesisParameters"
      (\x -> KinesisParameters' <$> (x .: "PartitionKeyPath"))

instance Hashable KinesisParameters

instance NFData KinesisParameters

instance ToJSON KinesisParameters where
  toJSON KinesisParameters' {..} =
    object
      (catMaybes [Just ("PartitionKeyPath" .= _kpPartitionKeyPath)])
