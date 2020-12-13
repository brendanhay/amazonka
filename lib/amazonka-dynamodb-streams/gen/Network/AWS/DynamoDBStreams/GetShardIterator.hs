{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.GetShardIterator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a shard iterator. A shard iterator provides information about how to retrieve the stream records from within a shard. Use the shard iterator in a subsequent @GetRecords@ request to read the stream records from the shard.
module Network.AWS.DynamoDBStreams.GetShardIterator
  ( -- * Creating a request
    GetShardIterator (..),
    mkGetShardIterator,

    -- ** Request lenses
    gsiSequenceNumber,
    gsiStreamARN,
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

import Network.AWS.DynamoDBStreams.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @GetShardIterator@ operation.
--
-- /See:/ 'mkGetShardIterator' smart constructor.
data GetShardIterator = GetShardIterator'
  { -- | The sequence number of a stream record in the shard from which to start reading.
    sequenceNumber :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) for the stream.
    streamARN :: Lude.Text,
    -- | Determines how the shard iterator is used to start reading stream records from the shard:
    --
    --
    --     * @AT_SEQUENCE_NUMBER@ - Start reading exactly from the position denoted by a specific sequence number.
    --
    --
    --     * @AFTER_SEQUENCE_NUMBER@ - Start reading right after the position denoted by a specific sequence number.
    --
    --
    --     * @TRIM_HORIZON@ - Start reading at the last (untrimmed) stream record, which is the oldest record in the shard. In DynamoDB Streams, there is a 24 hour limit on data retention. Stream records whose age exceeds this limit are subject to removal (trimming) from the stream.
    --
    --
    --     * @LATEST@ - Start reading just after the most recent stream record in the shard, so that you always read the most recent data in the shard.
    shardIteratorType :: ShardIteratorType,
    -- | The identifier of the shard. The iterator will be returned for this shard ID.
    shardId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetShardIterator' with the minimum fields required to make a request.
--
-- * 'sequenceNumber' - The sequence number of a stream record in the shard from which to start reading.
-- * 'streamARN' - The Amazon Resource Name (ARN) for the stream.
-- * 'shardIteratorType' - Determines how the shard iterator is used to start reading stream records from the shard:
--
--
--     * @AT_SEQUENCE_NUMBER@ - Start reading exactly from the position denoted by a specific sequence number.
--
--
--     * @AFTER_SEQUENCE_NUMBER@ - Start reading right after the position denoted by a specific sequence number.
--
--
--     * @TRIM_HORIZON@ - Start reading at the last (untrimmed) stream record, which is the oldest record in the shard. In DynamoDB Streams, there is a 24 hour limit on data retention. Stream records whose age exceeds this limit are subject to removal (trimming) from the stream.
--
--
--     * @LATEST@ - Start reading just after the most recent stream record in the shard, so that you always read the most recent data in the shard.
--
--
-- * 'shardId' - The identifier of the shard. The iterator will be returned for this shard ID.
mkGetShardIterator ::
  -- | 'streamARN'
  Lude.Text ->
  -- | 'shardIteratorType'
  ShardIteratorType ->
  -- | 'shardId'
  Lude.Text ->
  GetShardIterator
mkGetShardIterator pStreamARN_ pShardIteratorType_ pShardId_ =
  GetShardIterator'
    { sequenceNumber = Lude.Nothing,
      streamARN = pStreamARN_,
      shardIteratorType = pShardIteratorType_,
      shardId = pShardId_
    }

-- | The sequence number of a stream record in the shard from which to start reading.
--
-- /Note:/ Consider using 'sequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiSequenceNumber :: Lens.Lens' GetShardIterator (Lude.Maybe Lude.Text)
gsiSequenceNumber = Lens.lens (sequenceNumber :: GetShardIterator -> Lude.Maybe Lude.Text) (\s a -> s {sequenceNumber = a} :: GetShardIterator)
{-# DEPRECATED gsiSequenceNumber "Use generic-lens or generic-optics with 'sequenceNumber' instead." #-}

-- | The Amazon Resource Name (ARN) for the stream.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiStreamARN :: Lens.Lens' GetShardIterator Lude.Text
gsiStreamARN = Lens.lens (streamARN :: GetShardIterator -> Lude.Text) (\s a -> s {streamARN = a} :: GetShardIterator)
{-# DEPRECATED gsiStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | Determines how the shard iterator is used to start reading stream records from the shard:
--
--
--     * @AT_SEQUENCE_NUMBER@ - Start reading exactly from the position denoted by a specific sequence number.
--
--
--     * @AFTER_SEQUENCE_NUMBER@ - Start reading right after the position denoted by a specific sequence number.
--
--
--     * @TRIM_HORIZON@ - Start reading at the last (untrimmed) stream record, which is the oldest record in the shard. In DynamoDB Streams, there is a 24 hour limit on data retention. Stream records whose age exceeds this limit are subject to removal (trimming) from the stream.
--
--
--     * @LATEST@ - Start reading just after the most recent stream record in the shard, so that you always read the most recent data in the shard.
--
--
--
-- /Note:/ Consider using 'shardIteratorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiShardIteratorType :: Lens.Lens' GetShardIterator ShardIteratorType
gsiShardIteratorType = Lens.lens (shardIteratorType :: GetShardIterator -> ShardIteratorType) (\s a -> s {shardIteratorType = a} :: GetShardIterator)
{-# DEPRECATED gsiShardIteratorType "Use generic-lens or generic-optics with 'shardIteratorType' instead." #-}

-- | The identifier of the shard. The iterator will be returned for this shard ID.
--
-- /Note:/ Consider using 'shardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiShardId :: Lens.Lens' GetShardIterator Lude.Text
gsiShardId = Lens.lens (shardId :: GetShardIterator -> Lude.Text) (\s a -> s {shardId = a} :: GetShardIterator)
{-# DEPRECATED gsiShardId "Use generic-lens or generic-optics with 'shardId' instead." #-}

instance Lude.AWSRequest GetShardIterator where
  type Rs GetShardIterator = GetShardIteratorResponse
  request = Req.postJSON dynamoDBStreamsService
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
              Lude.=# ("DynamoDBStreams_20120810.GetShardIterator" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetShardIterator where
  toJSON GetShardIterator' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SequenceNumber" Lude..=) Lude.<$> sequenceNumber,
            Lude.Just ("StreamArn" Lude..= streamARN),
            Lude.Just ("ShardIteratorType" Lude..= shardIteratorType),
            Lude.Just ("ShardId" Lude..= shardId)
          ]
      )

instance Lude.ToPath GetShardIterator where
  toPath = Lude.const "/"

instance Lude.ToQuery GetShardIterator where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @GetShardIterator@ operation.
--
-- /See:/ 'mkGetShardIteratorResponse' smart constructor.
data GetShardIteratorResponse = GetShardIteratorResponse'
  { -- | The position in the shard from which to start reading stream records sequentially. A shard iterator specifies this position using the sequence number of a stream record in a shard.
    shardIterator :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetShardIteratorResponse' with the minimum fields required to make a request.
--
-- * 'shardIterator' - The position in the shard from which to start reading stream records sequentially. A shard iterator specifies this position using the sequence number of a stream record in a shard.
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

-- | The position in the shard from which to start reading stream records sequentially. A shard iterator specifies this position using the sequence number of a stream record in a shard.
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
