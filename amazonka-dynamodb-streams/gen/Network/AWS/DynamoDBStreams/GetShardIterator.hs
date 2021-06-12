{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.GetShardIterator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a shard iterator. A shard iterator provides information about
-- how to retrieve the stream records from within a shard. Use the shard
-- iterator in a subsequent @GetRecords@ request to read the stream records
-- from the shard.
--
-- A shard iterator expires 15 minutes after it is returned to the
-- requester.
module Network.AWS.DynamoDBStreams.GetShardIterator
  ( -- * Creating a Request
    GetShardIterator (..),
    newGetShardIterator,

    -- * Request Lenses
    getShardIterator_sequenceNumber,
    getShardIterator_streamArn,
    getShardIterator_shardId,
    getShardIterator_shardIteratorType,

    -- * Destructuring the Response
    GetShardIteratorResponse (..),
    newGetShardIteratorResponse,

    -- * Response Lenses
    getShardIteratorResponse_shardIterator,
    getShardIteratorResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDBStreams.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetShardIterator@ operation.
--
-- /See:/ 'newGetShardIterator' smart constructor.
data GetShardIterator = GetShardIterator'
  { -- | The sequence number of a stream record in the shard from which to start
    -- reading.
    sequenceNumber :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) for the stream.
    streamArn :: Core.Text,
    -- | The identifier of the shard. The iterator will be returned for this
    -- shard ID.
    shardId :: Core.Text,
    -- | Determines how the shard iterator is used to start reading stream
    -- records from the shard:
    --
    -- -   @AT_SEQUENCE_NUMBER@ - Start reading exactly from the position
    --     denoted by a specific sequence number.
    --
    -- -   @AFTER_SEQUENCE_NUMBER@ - Start reading right after the position
    --     denoted by a specific sequence number.
    --
    -- -   @TRIM_HORIZON@ - Start reading at the last (untrimmed) stream
    --     record, which is the oldest record in the shard. In DynamoDB
    --     Streams, there is a 24 hour limit on data retention. Stream records
    --     whose age exceeds this limit are subject to removal (trimming) from
    --     the stream.
    --
    -- -   @LATEST@ - Start reading just after the most recent stream record in
    --     the shard, so that you always read the most recent data in the
    --     shard.
    shardIteratorType :: ShardIteratorType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetShardIterator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sequenceNumber', 'getShardIterator_sequenceNumber' - The sequence number of a stream record in the shard from which to start
-- reading.
--
-- 'streamArn', 'getShardIterator_streamArn' - The Amazon Resource Name (ARN) for the stream.
--
-- 'shardId', 'getShardIterator_shardId' - The identifier of the shard. The iterator will be returned for this
-- shard ID.
--
-- 'shardIteratorType', 'getShardIterator_shardIteratorType' - Determines how the shard iterator is used to start reading stream
-- records from the shard:
--
-- -   @AT_SEQUENCE_NUMBER@ - Start reading exactly from the position
--     denoted by a specific sequence number.
--
-- -   @AFTER_SEQUENCE_NUMBER@ - Start reading right after the position
--     denoted by a specific sequence number.
--
-- -   @TRIM_HORIZON@ - Start reading at the last (untrimmed) stream
--     record, which is the oldest record in the shard. In DynamoDB
--     Streams, there is a 24 hour limit on data retention. Stream records
--     whose age exceeds this limit are subject to removal (trimming) from
--     the stream.
--
-- -   @LATEST@ - Start reading just after the most recent stream record in
--     the shard, so that you always read the most recent data in the
--     shard.
newGetShardIterator ::
  -- | 'streamArn'
  Core.Text ->
  -- | 'shardId'
  Core.Text ->
  -- | 'shardIteratorType'
  ShardIteratorType ->
  GetShardIterator
newGetShardIterator
  pStreamArn_
  pShardId_
  pShardIteratorType_ =
    GetShardIterator'
      { sequenceNumber = Core.Nothing,
        streamArn = pStreamArn_,
        shardId = pShardId_,
        shardIteratorType = pShardIteratorType_
      }

-- | The sequence number of a stream record in the shard from which to start
-- reading.
getShardIterator_sequenceNumber :: Lens.Lens' GetShardIterator (Core.Maybe Core.Text)
getShardIterator_sequenceNumber = Lens.lens (\GetShardIterator' {sequenceNumber} -> sequenceNumber) (\s@GetShardIterator' {} a -> s {sequenceNumber = a} :: GetShardIterator)

-- | The Amazon Resource Name (ARN) for the stream.
getShardIterator_streamArn :: Lens.Lens' GetShardIterator Core.Text
getShardIterator_streamArn = Lens.lens (\GetShardIterator' {streamArn} -> streamArn) (\s@GetShardIterator' {} a -> s {streamArn = a} :: GetShardIterator)

-- | The identifier of the shard. The iterator will be returned for this
-- shard ID.
getShardIterator_shardId :: Lens.Lens' GetShardIterator Core.Text
getShardIterator_shardId = Lens.lens (\GetShardIterator' {shardId} -> shardId) (\s@GetShardIterator' {} a -> s {shardId = a} :: GetShardIterator)

-- | Determines how the shard iterator is used to start reading stream
-- records from the shard:
--
-- -   @AT_SEQUENCE_NUMBER@ - Start reading exactly from the position
--     denoted by a specific sequence number.
--
-- -   @AFTER_SEQUENCE_NUMBER@ - Start reading right after the position
--     denoted by a specific sequence number.
--
-- -   @TRIM_HORIZON@ - Start reading at the last (untrimmed) stream
--     record, which is the oldest record in the shard. In DynamoDB
--     Streams, there is a 24 hour limit on data retention. Stream records
--     whose age exceeds this limit are subject to removal (trimming) from
--     the stream.
--
-- -   @LATEST@ - Start reading just after the most recent stream record in
--     the shard, so that you always read the most recent data in the
--     shard.
getShardIterator_shardIteratorType :: Lens.Lens' GetShardIterator ShardIteratorType
getShardIterator_shardIteratorType = Lens.lens (\GetShardIterator' {shardIteratorType} -> shardIteratorType) (\s@GetShardIterator' {} a -> s {shardIteratorType = a} :: GetShardIterator)

instance Core.AWSRequest GetShardIterator where
  type
    AWSResponse GetShardIterator =
      GetShardIteratorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetShardIteratorResponse'
            Core.<$> (x Core..?> "ShardIterator")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetShardIterator

instance Core.NFData GetShardIterator

instance Core.ToHeaders GetShardIterator where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDBStreams_20120810.GetShardIterator" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetShardIterator where
  toJSON GetShardIterator' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SequenceNumber" Core..=) Core.<$> sequenceNumber,
            Core.Just ("StreamArn" Core..= streamArn),
            Core.Just ("ShardId" Core..= shardId),
            Core.Just
              ("ShardIteratorType" Core..= shardIteratorType)
          ]
      )

instance Core.ToPath GetShardIterator where
  toPath = Core.const "/"

instance Core.ToQuery GetShardIterator where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @GetShardIterator@ operation.
--
-- /See:/ 'newGetShardIteratorResponse' smart constructor.
data GetShardIteratorResponse = GetShardIteratorResponse'
  { -- | The position in the shard from which to start reading stream records
    -- sequentially. A shard iterator specifies this position using the
    -- sequence number of a stream record in a shard.
    shardIterator :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetShardIteratorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shardIterator', 'getShardIteratorResponse_shardIterator' - The position in the shard from which to start reading stream records
-- sequentially. A shard iterator specifies this position using the
-- sequence number of a stream record in a shard.
--
-- 'httpStatus', 'getShardIteratorResponse_httpStatus' - The response's http status code.
newGetShardIteratorResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetShardIteratorResponse
newGetShardIteratorResponse pHttpStatus_ =
  GetShardIteratorResponse'
    { shardIterator =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The position in the shard from which to start reading stream records
-- sequentially. A shard iterator specifies this position using the
-- sequence number of a stream record in a shard.
getShardIteratorResponse_shardIterator :: Lens.Lens' GetShardIteratorResponse (Core.Maybe Core.Text)
getShardIteratorResponse_shardIterator = Lens.lens (\GetShardIteratorResponse' {shardIterator} -> shardIterator) (\s@GetShardIteratorResponse' {} a -> s {shardIterator = a} :: GetShardIteratorResponse)

-- | The response's http status code.
getShardIteratorResponse_httpStatus :: Lens.Lens' GetShardIteratorResponse Core.Int
getShardIteratorResponse_httpStatus = Lens.lens (\GetShardIteratorResponse' {httpStatus} -> httpStatus) (\s@GetShardIteratorResponse' {} a -> s {httpStatus = a} :: GetShardIteratorResponse)

instance Core.NFData GetShardIteratorResponse
