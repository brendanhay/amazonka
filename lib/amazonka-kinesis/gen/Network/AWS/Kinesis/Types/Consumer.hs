{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.Consumer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.Consumer where

import Network.AWS.Kinesis.Types.ConsumerStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that represents the details of the consumer you registered. This type of object is returned by 'RegisterStreamConsumer' .
--
--
--
-- /See:/ 'consumer' smart constructor.
data Consumer = Consumer'
  { _cConsumerName :: !Text,
    _cConsumerARN :: !Text,
    _cConsumerStatus :: !ConsumerStatus,
    _cConsumerCreationTimestamp :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Consumer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cConsumerName' - The name of the consumer is something you choose when you register the consumer.
--
-- * 'cConsumerARN' - When you register a consumer, Kinesis Data Streams generates an ARN for it. You need this ARN to be able to call 'SubscribeToShard' . If you delete a consumer and then create a new one with the same name, it won't have the same ARN. That's because consumer ARNs contain the creation timestamp. This is important to keep in mind if you have IAM policies that reference consumer ARNs.
--
-- * 'cConsumerStatus' - A consumer can't read data while in the @CREATING@ or @DELETING@ states.
--
-- * 'cConsumerCreationTimestamp' -
consumer ::
  -- | 'cConsumerName'
  Text ->
  -- | 'cConsumerARN'
  Text ->
  -- | 'cConsumerStatus'
  ConsumerStatus ->
  -- | 'cConsumerCreationTimestamp'
  UTCTime ->
  Consumer
consumer
  pConsumerName_
  pConsumerARN_
  pConsumerStatus_
  pConsumerCreationTimestamp_ =
    Consumer'
      { _cConsumerName = pConsumerName_,
        _cConsumerARN = pConsumerARN_,
        _cConsumerStatus = pConsumerStatus_,
        _cConsumerCreationTimestamp = _Time # pConsumerCreationTimestamp_
      }

-- | The name of the consumer is something you choose when you register the consumer.
cConsumerName :: Lens' Consumer Text
cConsumerName = lens _cConsumerName (\s a -> s {_cConsumerName = a})

-- | When you register a consumer, Kinesis Data Streams generates an ARN for it. You need this ARN to be able to call 'SubscribeToShard' . If you delete a consumer and then create a new one with the same name, it won't have the same ARN. That's because consumer ARNs contain the creation timestamp. This is important to keep in mind if you have IAM policies that reference consumer ARNs.
cConsumerARN :: Lens' Consumer Text
cConsumerARN = lens _cConsumerARN (\s a -> s {_cConsumerARN = a})

-- | A consumer can't read data while in the @CREATING@ or @DELETING@ states.
cConsumerStatus :: Lens' Consumer ConsumerStatus
cConsumerStatus = lens _cConsumerStatus (\s a -> s {_cConsumerStatus = a})

-- |
cConsumerCreationTimestamp :: Lens' Consumer UTCTime
cConsumerCreationTimestamp = lens _cConsumerCreationTimestamp (\s a -> s {_cConsumerCreationTimestamp = a}) . _Time

instance FromJSON Consumer where
  parseJSON =
    withObject
      "Consumer"
      ( \x ->
          Consumer'
            <$> (x .: "ConsumerName")
            <*> (x .: "ConsumerARN")
            <*> (x .: "ConsumerStatus")
            <*> (x .: "ConsumerCreationTimestamp")
      )

instance Hashable Consumer

instance NFData Consumer
