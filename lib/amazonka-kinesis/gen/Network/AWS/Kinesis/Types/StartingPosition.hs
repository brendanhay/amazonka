{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.StartingPosition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.StartingPosition where

import Network.AWS.Kinesis.Types.ShardIteratorType
import Network.AWS.Lens
import Network.AWS.Prelude

-- |
--
--
--
-- /See:/ 'startingPosition' smart constructor.
data StartingPosition = StartingPosition'
  { _spSequenceNumber ::
      !(Maybe Text),
    _spTimestamp :: !(Maybe POSIX),
    _spType :: !ShardIteratorType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartingPosition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spSequenceNumber' - The sequence number of the data record in the shard from which to start streaming. To specify a sequence number, set @StartingPosition@ to @AT_SEQUENCE_NUMBER@ or @AFTER_SEQUENCE_NUMBER@ .
--
-- * 'spTimestamp' - The time stamp of the data record from which to start reading. To specify a time stamp, set @StartingPosition@ to @Type AT_TIMESTAMP@ . A time stamp is the Unix epoch date with precision in milliseconds. For example, @2016-04-04T19:58:46.480-00:00@ or @1459799926.480@ . If a record with this exact time stamp does not exist, records will be streamed from the next (later) record. If the time stamp is older than the current trim horizon, records will be streamed from the oldest untrimmed data record (@TRIM_HORIZON@ ).
--
-- * 'spType' - You can set the starting position to one of the following values: @AT_SEQUENCE_NUMBER@ : Start streaming from the position denoted by the sequence number specified in the @SequenceNumber@ field. @AFTER_SEQUENCE_NUMBER@ : Start streaming right after the position denoted by the sequence number specified in the @SequenceNumber@ field. @AT_TIMESTAMP@ : Start streaming from the position denoted by the time stamp specified in the @Timestamp@ field. @TRIM_HORIZON@ : Start streaming at the last untrimmed record in the shard, which is the oldest data record in the shard. @LATEST@ : Start streaming just after the most recent record in the shard, so that you always read the most recent data in the shard.
startingPosition ::
  -- | 'spType'
  ShardIteratorType ->
  StartingPosition
startingPosition pType_ =
  StartingPosition'
    { _spSequenceNumber = Nothing,
      _spTimestamp = Nothing,
      _spType = pType_
    }

-- | The sequence number of the data record in the shard from which to start streaming. To specify a sequence number, set @StartingPosition@ to @AT_SEQUENCE_NUMBER@ or @AFTER_SEQUENCE_NUMBER@ .
spSequenceNumber :: Lens' StartingPosition (Maybe Text)
spSequenceNumber = lens _spSequenceNumber (\s a -> s {_spSequenceNumber = a})

-- | The time stamp of the data record from which to start reading. To specify a time stamp, set @StartingPosition@ to @Type AT_TIMESTAMP@ . A time stamp is the Unix epoch date with precision in milliseconds. For example, @2016-04-04T19:58:46.480-00:00@ or @1459799926.480@ . If a record with this exact time stamp does not exist, records will be streamed from the next (later) record. If the time stamp is older than the current trim horizon, records will be streamed from the oldest untrimmed data record (@TRIM_HORIZON@ ).
spTimestamp :: Lens' StartingPosition (Maybe UTCTime)
spTimestamp = lens _spTimestamp (\s a -> s {_spTimestamp = a}) . mapping _Time

-- | You can set the starting position to one of the following values: @AT_SEQUENCE_NUMBER@ : Start streaming from the position denoted by the sequence number specified in the @SequenceNumber@ field. @AFTER_SEQUENCE_NUMBER@ : Start streaming right after the position denoted by the sequence number specified in the @SequenceNumber@ field. @AT_TIMESTAMP@ : Start streaming from the position denoted by the time stamp specified in the @Timestamp@ field. @TRIM_HORIZON@ : Start streaming at the last untrimmed record in the shard, which is the oldest data record in the shard. @LATEST@ : Start streaming just after the most recent record in the shard, so that you always read the most recent data in the shard.
spType :: Lens' StartingPosition ShardIteratorType
spType = lens _spType (\s a -> s {_spType = a})

instance Hashable StartingPosition

instance NFData StartingPosition

instance ToJSON StartingPosition where
  toJSON StartingPosition' {..} =
    object
      ( catMaybes
          [ ("SequenceNumber" .=) <$> _spSequenceNumber,
            ("Timestamp" .=) <$> _spTimestamp,
            Just ("Type" .= _spType)
          ]
      )
