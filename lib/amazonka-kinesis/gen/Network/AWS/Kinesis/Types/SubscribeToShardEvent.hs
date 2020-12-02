{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.SubscribeToShardEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.SubscribeToShardEvent where

import Network.AWS.Kinesis.Types.ChildShard
import Network.AWS.Kinesis.Types.Record
import Network.AWS.Lens
import Network.AWS.Prelude

-- | After you call 'SubscribeToShard' , Kinesis Data Streams sends events of this type over an HTTP/2 connection to your consumer.
--
--
--
-- /See:/ 'subscribeToShardEvent' smart constructor.
data SubscribeToShardEvent = SubscribeToShardEvent'
  { _stseChildShards ::
      !(Maybe [ChildShard]),
    _stseRecords :: ![Record],
    _stseContinuationSequenceNumber :: !Text,
    _stseMillisBehindLatest :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SubscribeToShardEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stseChildShards' - Undocumented member.
--
-- * 'stseRecords' -
--
-- * 'stseContinuationSequenceNumber' - Use this as @SequenceNumber@ in the next call to 'SubscribeToShard' , with @StartingPosition@ set to @AT_SEQUENCE_NUMBER@ or @AFTER_SEQUENCE_NUMBER@ . Use @ContinuationSequenceNumber@ for checkpointing because it captures your shard progress even when no data is written to the shard.
--
-- * 'stseMillisBehindLatest' - The number of milliseconds the read records are from the tip of the stream, indicating how far behind current time the consumer is. A value of zero indicates that record processing is caught up, and there are no new records to process at this moment.
subscribeToShardEvent ::
  -- | 'stseContinuationSequenceNumber'
  Text ->
  -- | 'stseMillisBehindLatest'
  Natural ->
  SubscribeToShardEvent
subscribeToShardEvent
  pContinuationSequenceNumber_
  pMillisBehindLatest_ =
    SubscribeToShardEvent'
      { _stseChildShards = Nothing,
        _stseRecords = mempty,
        _stseContinuationSequenceNumber = pContinuationSequenceNumber_,
        _stseMillisBehindLatest = _Nat # pMillisBehindLatest_
      }

-- | Undocumented member.
stseChildShards :: Lens' SubscribeToShardEvent [ChildShard]
stseChildShards = lens _stseChildShards (\s a -> s {_stseChildShards = a}) . _Default . _Coerce

-- |
stseRecords :: Lens' SubscribeToShardEvent [Record]
stseRecords = lens _stseRecords (\s a -> s {_stseRecords = a}) . _Coerce

-- | Use this as @SequenceNumber@ in the next call to 'SubscribeToShard' , with @StartingPosition@ set to @AT_SEQUENCE_NUMBER@ or @AFTER_SEQUENCE_NUMBER@ . Use @ContinuationSequenceNumber@ for checkpointing because it captures your shard progress even when no data is written to the shard.
stseContinuationSequenceNumber :: Lens' SubscribeToShardEvent Text
stseContinuationSequenceNumber = lens _stseContinuationSequenceNumber (\s a -> s {_stseContinuationSequenceNumber = a})

-- | The number of milliseconds the read records are from the tip of the stream, indicating how far behind current time the consumer is. A value of zero indicates that record processing is caught up, and there are no new records to process at this moment.
stseMillisBehindLatest :: Lens' SubscribeToShardEvent Natural
stseMillisBehindLatest = lens _stseMillisBehindLatest (\s a -> s {_stseMillisBehindLatest = a}) . _Nat

instance FromJSON SubscribeToShardEvent where
  parseJSON =
    withObject
      "SubscribeToShardEvent"
      ( \x ->
          SubscribeToShardEvent'
            <$> (x .:? "ChildShards" .!= mempty)
            <*> (x .:? "Records" .!= mempty)
            <*> (x .: "ContinuationSequenceNumber")
            <*> (x .: "MillisBehindLatest")
      )

instance Hashable SubscribeToShardEvent

instance NFData SubscribeToShardEvent
