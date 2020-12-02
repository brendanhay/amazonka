{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.ConsumerDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.ConsumerDescription where

import Network.AWS.Kinesis.Types.ConsumerStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that represents the details of a registered consumer. This type of object is returned by 'DescribeStreamConsumer' .
--
--
--
-- /See:/ 'consumerDescription' smart constructor.
data ConsumerDescription = ConsumerDescription'
  { _cdConsumerName ::
      !Text,
    _cdConsumerARN :: !Text,
    _cdConsumerStatus :: !ConsumerStatus,
    _cdConsumerCreationTimestamp :: !POSIX,
    _cdStreamARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConsumerDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdConsumerName' - The name of the consumer is something you choose when you register the consumer.
--
-- * 'cdConsumerARN' - When you register a consumer, Kinesis Data Streams generates an ARN for it. You need this ARN to be able to call 'SubscribeToShard' . If you delete a consumer and then create a new one with the same name, it won't have the same ARN. That's because consumer ARNs contain the creation timestamp. This is important to keep in mind if you have IAM policies that reference consumer ARNs.
--
-- * 'cdConsumerStatus' - A consumer can't read data while in the @CREATING@ or @DELETING@ states.
--
-- * 'cdConsumerCreationTimestamp' -
--
-- * 'cdStreamARN' - The ARN of the stream with which you registered the consumer.
consumerDescription ::
  -- | 'cdConsumerName'
  Text ->
  -- | 'cdConsumerARN'
  Text ->
  -- | 'cdConsumerStatus'
  ConsumerStatus ->
  -- | 'cdConsumerCreationTimestamp'
  UTCTime ->
  -- | 'cdStreamARN'
  Text ->
  ConsumerDescription
consumerDescription
  pConsumerName_
  pConsumerARN_
  pConsumerStatus_
  pConsumerCreationTimestamp_
  pStreamARN_ =
    ConsumerDescription'
      { _cdConsumerName = pConsumerName_,
        _cdConsumerARN = pConsumerARN_,
        _cdConsumerStatus = pConsumerStatus_,
        _cdConsumerCreationTimestamp = _Time # pConsumerCreationTimestamp_,
        _cdStreamARN = pStreamARN_
      }

-- | The name of the consumer is something you choose when you register the consumer.
cdConsumerName :: Lens' ConsumerDescription Text
cdConsumerName = lens _cdConsumerName (\s a -> s {_cdConsumerName = a})

-- | When you register a consumer, Kinesis Data Streams generates an ARN for it. You need this ARN to be able to call 'SubscribeToShard' . If you delete a consumer and then create a new one with the same name, it won't have the same ARN. That's because consumer ARNs contain the creation timestamp. This is important to keep in mind if you have IAM policies that reference consumer ARNs.
cdConsumerARN :: Lens' ConsumerDescription Text
cdConsumerARN = lens _cdConsumerARN (\s a -> s {_cdConsumerARN = a})

-- | A consumer can't read data while in the @CREATING@ or @DELETING@ states.
cdConsumerStatus :: Lens' ConsumerDescription ConsumerStatus
cdConsumerStatus = lens _cdConsumerStatus (\s a -> s {_cdConsumerStatus = a})

-- |
cdConsumerCreationTimestamp :: Lens' ConsumerDescription UTCTime
cdConsumerCreationTimestamp = lens _cdConsumerCreationTimestamp (\s a -> s {_cdConsumerCreationTimestamp = a}) . _Time

-- | The ARN of the stream with which you registered the consumer.
cdStreamARN :: Lens' ConsumerDescription Text
cdStreamARN = lens _cdStreamARN (\s a -> s {_cdStreamARN = a})

instance FromJSON ConsumerDescription where
  parseJSON =
    withObject
      "ConsumerDescription"
      ( \x ->
          ConsumerDescription'
            <$> (x .: "ConsumerName")
            <*> (x .: "ConsumerARN")
            <*> (x .: "ConsumerStatus")
            <*> (x .: "ConsumerCreationTimestamp")
            <*> (x .: "StreamARN")
      )

instance Hashable ConsumerDescription

instance NFData ConsumerDescription
