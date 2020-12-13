{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.GetShardIterator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an Amazon Kinesis shard iterator. A shard iterator expires 5 minutes after it is returned to the requester.
--
-- A shard iterator specifies the shard position from which to start reading data records sequentially. The position is specified using the sequence number of a data record in a shard. A sequence number is the identifier associated with every record ingested in the stream, and is assigned when a record is put into the stream. Each stream has one or more shards.
-- You must specify the shard iterator type. For example, you can set the @ShardIteratorType@ parameter to read exactly from the position denoted by a specific sequence number by using the @AT_SEQUENCE_NUMBER@ shard iterator type. Alternatively, the parameter can read right after the sequence number by using the @AFTER_SEQUENCE_NUMBER@ shard iterator type, using sequence numbers returned by earlier calls to 'PutRecord' , 'PutRecords' , 'GetRecords' , or 'DescribeStream' . In the request, you can specify the shard iterator type @AT_TIMESTAMP@ to read records from an arbitrary point in time, @TRIM_HORIZON@ to cause @ShardIterator@ to point to the last untrimmed record in the shard in the system (the oldest data record in the shard), or @LATEST@ so that you always read the most recent data in the shard.
-- When you read repeatedly from a stream, use a 'GetShardIterator' request to get the first shard iterator for use in your first 'GetRecords' request and for subsequent reads use the shard iterator returned by the 'GetRecords' request in @NextShardIterator@ . A new shard iterator is returned by every 'GetRecords' request in @NextShardIterator@ , which you use in the @ShardIterator@ parameter of the next 'GetRecords' request.
-- If a 'GetShardIterator' request is made too often, you receive a @ProvisionedThroughputExceededException@ . For more information about throughput limits, see 'GetRecords' , and <https://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html Streams Limits> in the /Amazon Kinesis Data Streams Developer Guide/ .
-- If the shard is closed, 'GetShardIterator' returns a valid iterator for the last sequence number of the shard. A shard can be closed as a result of using 'SplitShard' or 'MergeShards' .
-- 'GetShardIterator' has a limit of five transactions per second per account per open shard.
module Network.AWS.Kinesis.GetShardIterator
  ( -- * Creating a request
    GetShardIterator (..),
    mkGetShardIterator,

    -- ** Request lenses
    gsiStartingSequenceNumber,
    gsiStreamName,
    gsiTimestamp,
    gsiShardIteratorType,
    gsiShardId,

    -- * Destructuring the response
    GetShardIteratorResponse (..),
    mkGetShardIteratorResponse,

    -- ** Response lenses
    gsirsShardIterator,
    gsirsResponseStatus,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for @GetShardIterator@ .
--
-- /See:/ 'mkGetShardIterator' smart constructor.
data GetShardIterator = GetShardIterator'
  { -- | The sequence number of the data record in the shard from which to start reading. Used with shard iterator type AT_SEQUENCE_NUMBER and AFTER_SEQUENCE_NUMBER.
    startingSequenceNumber :: Lude.Maybe Lude.Text,
    -- | The name of the Amazon Kinesis data stream.
    streamName :: Lude.Text,
    -- | The time stamp of the data record from which to start reading. Used with shard iterator type AT_TIMESTAMP. A time stamp is the Unix epoch date with precision in milliseconds. For example, @2016-04-04T19:58:46.480-00:00@ or @1459799926.480@ . If a record with this exact time stamp does not exist, the iterator returned is for the next (later) record. If the time stamp is older than the current trim horizon, the iterator returned is for the oldest untrimmed data record (TRIM_HORIZON).
    timestamp :: Lude.Maybe Lude.Timestamp,
    -- | Determines how the shard iterator is used to start reading data records from the shard.
    --
    -- The following are the valid Amazon Kinesis shard iterator types:
    --
    --     * AT_SEQUENCE_NUMBER - Start reading from the position denoted by a specific sequence number, provided in the value @StartingSequenceNumber@ .
    --
    --
    --     * AFTER_SEQUENCE_NUMBER - Start reading right after the position denoted by a specific sequence number, provided in the value @StartingSequenceNumber@ .
    --
    --
    --     * AT_TIMESTAMP - Start reading from the position denoted by a specific time stamp, provided in the value @Timestamp@ .
    --
    --
    --     * TRIM_HORIZON - Start reading at the last untrimmed record in the shard in the system, which is the oldest data record in the shard.
    --
    --
    --     * LATEST - Start reading just after the most recent record in the shard, so that you always read the most recent data in the shard.
    shardIteratorType :: ShardIteratorType,
    -- | The shard ID of the Kinesis Data Streams shard to get the iterator for.
    shardId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetShardIterator' with the minimum fields required to make a request.
--
-- * 'startingSequenceNumber' - The sequence number of the data record in the shard from which to start reading. Used with shard iterator type AT_SEQUENCE_NUMBER and AFTER_SEQUENCE_NUMBER.
-- * 'streamName' - The name of the Amazon Kinesis data stream.
-- * 'timestamp' - The time stamp of the data record from which to start reading. Used with shard iterator type AT_TIMESTAMP. A time stamp is the Unix epoch date with precision in milliseconds. For example, @2016-04-04T19:58:46.480-00:00@ or @1459799926.480@ . If a record with this exact time stamp does not exist, the iterator returned is for the next (later) record. If the time stamp is older than the current trim horizon, the iterator returned is for the oldest untrimmed data record (TRIM_HORIZON).
-- * 'shardIteratorType' - Determines how the shard iterator is used to start reading data records from the shard.
--
-- The following are the valid Amazon Kinesis shard iterator types:
--
--     * AT_SEQUENCE_NUMBER - Start reading from the position denoted by a specific sequence number, provided in the value @StartingSequenceNumber@ .
--
--
--     * AFTER_SEQUENCE_NUMBER - Start reading right after the position denoted by a specific sequence number, provided in the value @StartingSequenceNumber@ .
--
--
--     * AT_TIMESTAMP - Start reading from the position denoted by a specific time stamp, provided in the value @Timestamp@ .
--
--
--     * TRIM_HORIZON - Start reading at the last untrimmed record in the shard in the system, which is the oldest data record in the shard.
--
--
--     * LATEST - Start reading just after the most recent record in the shard, so that you always read the most recent data in the shard.
--
--
-- * 'shardId' - The shard ID of the Kinesis Data Streams shard to get the iterator for.
mkGetShardIterator ::
  -- | 'streamName'
  Lude.Text ->
  -- | 'shardIteratorType'
  ShardIteratorType ->
  -- | 'shardId'
  Lude.Text ->
  GetShardIterator
mkGetShardIterator pStreamName_ pShardIteratorType_ pShardId_ =
  GetShardIterator'
    { startingSequenceNumber = Lude.Nothing,
      streamName = pStreamName_,
      timestamp = Lude.Nothing,
      shardIteratorType = pShardIteratorType_,
      shardId = pShardId_
    }

-- | The sequence number of the data record in the shard from which to start reading. Used with shard iterator type AT_SEQUENCE_NUMBER and AFTER_SEQUENCE_NUMBER.
--
-- /Note:/ Consider using 'startingSequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiStartingSequenceNumber :: Lens.Lens' GetShardIterator (Lude.Maybe Lude.Text)
gsiStartingSequenceNumber = Lens.lens (startingSequenceNumber :: GetShardIterator -> Lude.Maybe Lude.Text) (\s a -> s {startingSequenceNumber = a} :: GetShardIterator)
{-# DEPRECATED gsiStartingSequenceNumber "Use generic-lens or generic-optics with 'startingSequenceNumber' instead." #-}

-- | The name of the Amazon Kinesis data stream.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiStreamName :: Lens.Lens' GetShardIterator Lude.Text
gsiStreamName = Lens.lens (streamName :: GetShardIterator -> Lude.Text) (\s a -> s {streamName = a} :: GetShardIterator)
{-# DEPRECATED gsiStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | The time stamp of the data record from which to start reading. Used with shard iterator type AT_TIMESTAMP. A time stamp is the Unix epoch date with precision in milliseconds. For example, @2016-04-04T19:58:46.480-00:00@ or @1459799926.480@ . If a record with this exact time stamp does not exist, the iterator returned is for the next (later) record. If the time stamp is older than the current trim horizon, the iterator returned is for the oldest untrimmed data record (TRIM_HORIZON).
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiTimestamp :: Lens.Lens' GetShardIterator (Lude.Maybe Lude.Timestamp)
gsiTimestamp = Lens.lens (timestamp :: GetShardIterator -> Lude.Maybe Lude.Timestamp) (\s a -> s {timestamp = a} :: GetShardIterator)
{-# DEPRECATED gsiTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | Determines how the shard iterator is used to start reading data records from the shard.
--
-- The following are the valid Amazon Kinesis shard iterator types:
--
--     * AT_SEQUENCE_NUMBER - Start reading from the position denoted by a specific sequence number, provided in the value @StartingSequenceNumber@ .
--
--
--     * AFTER_SEQUENCE_NUMBER - Start reading right after the position denoted by a specific sequence number, provided in the value @StartingSequenceNumber@ .
--
--
--     * AT_TIMESTAMP - Start reading from the position denoted by a specific time stamp, provided in the value @Timestamp@ .
--
--
--     * TRIM_HORIZON - Start reading at the last untrimmed record in the shard in the system, which is the oldest data record in the shard.
--
--
--     * LATEST - Start reading just after the most recent record in the shard, so that you always read the most recent data in the shard.
--
--
--
-- /Note:/ Consider using 'shardIteratorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiShardIteratorType :: Lens.Lens' GetShardIterator ShardIteratorType
gsiShardIteratorType = Lens.lens (shardIteratorType :: GetShardIterator -> ShardIteratorType) (\s a -> s {shardIteratorType = a} :: GetShardIterator)
{-# DEPRECATED gsiShardIteratorType "Use generic-lens or generic-optics with 'shardIteratorType' instead." #-}

-- | The shard ID of the Kinesis Data Streams shard to get the iterator for.
--
-- /Note:/ Consider using 'shardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiShardId :: Lens.Lens' GetShardIterator Lude.Text
gsiShardId = Lens.lens (shardId :: GetShardIterator -> Lude.Text) (\s a -> s {shardId = a} :: GetShardIterator)
{-# DEPRECATED gsiShardId "Use generic-lens or generic-optics with 'shardId' instead." #-}

instance Lude.AWSRequest GetShardIterator where
  type Rs GetShardIterator = GetShardIteratorResponse
  request = Req.postJSON kinesisService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetShardIteratorResponse'
            Lude.<$> (x Lude..?> "ShardIterator")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetShardIterator where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.GetShardIterator" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetShardIterator where
  toJSON GetShardIterator' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StartingSequenceNumber" Lude..=)
              Lude.<$> startingSequenceNumber,
            Lude.Just ("StreamName" Lude..= streamName),
            ("Timestamp" Lude..=) Lude.<$> timestamp,
            Lude.Just ("ShardIteratorType" Lude..= shardIteratorType),
            Lude.Just ("ShardId" Lude..= shardId)
          ]
      )

instance Lude.ToPath GetShardIterator where
  toPath = Lude.const "/"

instance Lude.ToQuery GetShardIterator where
  toQuery = Lude.const Lude.mempty

-- | Represents the output for @GetShardIterator@ .
--
-- /See:/ 'mkGetShardIteratorResponse' smart constructor.
data GetShardIteratorResponse = GetShardIteratorResponse'
  { -- | The position in the shard from which to start reading data records sequentially. A shard iterator specifies this position using the sequence number of a data record in a shard.
    shardIterator :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetShardIteratorResponse' with the minimum fields required to make a request.
--
-- * 'shardIterator' - The position in the shard from which to start reading data records sequentially. A shard iterator specifies this position using the sequence number of a data record in a shard.
-- * 'responseStatus' - The response status code.
mkGetShardIteratorResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetShardIteratorResponse
mkGetShardIteratorResponse pResponseStatus_ =
  GetShardIteratorResponse'
    { shardIterator = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The position in the shard from which to start reading data records sequentially. A shard iterator specifies this position using the sequence number of a data record in a shard.
--
-- /Note:/ Consider using 'shardIterator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsirsShardIterator :: Lens.Lens' GetShardIteratorResponse (Lude.Maybe Lude.Text)
gsirsShardIterator = Lens.lens (shardIterator :: GetShardIteratorResponse -> Lude.Maybe Lude.Text) (\s a -> s {shardIterator = a} :: GetShardIteratorResponse)
{-# DEPRECATED gsirsShardIterator "Use generic-lens or generic-optics with 'shardIterator' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsirsResponseStatus :: Lens.Lens' GetShardIteratorResponse Lude.Int
gsirsResponseStatus = Lens.lens (responseStatus :: GetShardIteratorResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetShardIteratorResponse)
{-# DEPRECATED gsirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
