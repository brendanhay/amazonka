{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DynamoDBStreams.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetShardIterator@ operation.
--
-- /See:/ 'newGetShardIterator' smart constructor.
data GetShardIterator = GetShardIterator'
  { -- | The sequence number of a stream record in the shard from which to start
    -- reading.
    sequenceNumber :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the stream.
    streamArn :: Prelude.Text,
    -- | The identifier of the shard. The iterator will be returned for this
    -- shard ID.
    shardId :: Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'shardId'
  Prelude.Text ->
  -- | 'shardIteratorType'
  ShardIteratorType ->
  GetShardIterator
newGetShardIterator
  pStreamArn_
  pShardId_
  pShardIteratorType_ =
    GetShardIterator'
      { sequenceNumber = Prelude.Nothing,
        streamArn = pStreamArn_,
        shardId = pShardId_,
        shardIteratorType = pShardIteratorType_
      }

-- | The sequence number of a stream record in the shard from which to start
-- reading.
getShardIterator_sequenceNumber :: Lens.Lens' GetShardIterator (Prelude.Maybe Prelude.Text)
getShardIterator_sequenceNumber = Lens.lens (\GetShardIterator' {sequenceNumber} -> sequenceNumber) (\s@GetShardIterator' {} a -> s {sequenceNumber = a} :: GetShardIterator)

-- | The Amazon Resource Name (ARN) for the stream.
getShardIterator_streamArn :: Lens.Lens' GetShardIterator Prelude.Text
getShardIterator_streamArn = Lens.lens (\GetShardIterator' {streamArn} -> streamArn) (\s@GetShardIterator' {} a -> s {streamArn = a} :: GetShardIterator)

-- | The identifier of the shard. The iterator will be returned for this
-- shard ID.
getShardIterator_shardId :: Lens.Lens' GetShardIterator Prelude.Text
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

instance Prelude.AWSRequest GetShardIterator where
  type Rs GetShardIterator = GetShardIteratorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetShardIteratorResponse'
            Prelude.<$> (x Prelude..?> "ShardIterator")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetShardIterator

instance Prelude.NFData GetShardIterator

instance Prelude.ToHeaders GetShardIterator where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DynamoDBStreams_20120810.GetShardIterator" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetShardIterator where
  toJSON GetShardIterator' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SequenceNumber" Prelude..=)
              Prelude.<$> sequenceNumber,
            Prelude.Just ("StreamArn" Prelude..= streamArn),
            Prelude.Just ("ShardId" Prelude..= shardId),
            Prelude.Just
              ("ShardIteratorType" Prelude..= shardIteratorType)
          ]
      )

instance Prelude.ToPath GetShardIterator where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetShardIterator where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @GetShardIterator@ operation.
--
-- /See:/ 'newGetShardIteratorResponse' smart constructor.
data GetShardIteratorResponse = GetShardIteratorResponse'
  { -- | The position in the shard from which to start reading stream records
    -- sequentially. A shard iterator specifies this position using the
    -- sequence number of a stream record in a shard.
    shardIterator :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetShardIteratorResponse
newGetShardIteratorResponse pHttpStatus_ =
  GetShardIteratorResponse'
    { shardIterator =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The position in the shard from which to start reading stream records
-- sequentially. A shard iterator specifies this position using the
-- sequence number of a stream record in a shard.
getShardIteratorResponse_shardIterator :: Lens.Lens' GetShardIteratorResponse (Prelude.Maybe Prelude.Text)
getShardIteratorResponse_shardIterator = Lens.lens (\GetShardIteratorResponse' {shardIterator} -> shardIterator) (\s@GetShardIteratorResponse' {} a -> s {shardIterator = a} :: GetShardIteratorResponse)

-- | The response's http status code.
getShardIteratorResponse_httpStatus :: Lens.Lens' GetShardIteratorResponse Prelude.Int
getShardIteratorResponse_httpStatus = Lens.lens (\GetShardIteratorResponse' {httpStatus} -> httpStatus) (\s@GetShardIteratorResponse' {} a -> s {httpStatus = a} :: GetShardIteratorResponse)

instance Prelude.NFData GetShardIteratorResponse
